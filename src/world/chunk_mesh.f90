module chunk_mesh
  use :: mesh
  use :: chunk_data
  use :: chunk_handler
  use :: block_repo, only: block_definition
  use :: vector_3i
  use :: thread
  use, intrinsic :: iso_c_binding
  implicit none


  !
  ! +X RIGHT
  !
  ! +Y UP
  !
  ! +Z FORWARD (maybe)
  !
  ! Negatives will be root. (0.0, 0.0, 0.0)
  !
  ! 1 _______ 4
  !  |\      | <= Counter-clockwise.
  !  |  \    |
  !  |    \  | <= 1,2,3,3,4,1 improves cachiness.
  !  |______\|
  ! 2         3
  !
  !
  ! Texture ordering: -Z, +Z, -X, +X, -Y, +Y
  !


  private


  ! In offset for OpenGL/Vulkan.
  integer(c_int), dimension(6), parameter :: BASE_INDICES = (/ &
    0,1,2,2,3,0 &
    /)


  !? -Z (Facing the camera at rotation 0.0)
  real(c_float), dimension(12), parameter :: BACK_FACE = (/ &
    0.0, 1.0, 0.0, &
    0.0, 0.0, 0.0, &
    1.0, 0.0, 0.0, &
    1.0, 1.0, 0.0 &
    /)

  !? +Z (Facing away from camera at rotation 0.0)
  real(c_float), dimension(12), parameter :: FRONT_FACE = (/ &
    1.0, 1.0, 1.0, &
    1.0, 0.0, 1.0, &
    0.0, 0.0, 1.0, &
    0.0, 1.0, 1.0 &
    /)


  !? -X (Facing left at rotation 0.0)
  real(c_float), dimension(12), parameter :: LEFT_FACE = (/ &
    0.0, 1.0, 1.0, &
    0.0, 0.0, 1.0, &
    0.0, 0.0, 0.0, &
    0.0, 1.0, 0.0 &
    /)

  !? +X (Facing right at rotation 0.0)
  real(c_float), dimension(12), parameter :: RIGHT_FACE = (/ &
    1.0, 1.0, 0.0, &
    1.0, 0.0, 0.0, &
    1.0, 0.0, 1.0, &
    1.0, 1.0, 1.0 &
    /)


  !? -Y
  real(c_float), dimension(12), parameter :: BOTTOM_FACE = (/ &
    0.0, 0.0, 0.0, &
    0.0, 0.0, 1.0, &
    1.0, 0.0, 1.0, &
    1.0, 0.0, 0.0 &
    /)

  !? +Y
  real(c_float), dimension(12), parameter :: TOP_FACE = (/ &
    1.0, 1.0, 0.0, &
    1.0, 1.0, 1.0, &
    0.0, 1.0, 1.0, &
    0.0, 1.0, 0.0 &
    /)


  ! Chunks shall generate each block face as follows:
  ! [ -Z, +Z, -X, +X, -Y, +Y ]

  real(c_float), dimension(12, 6), parameter :: FACES = reshape((/ &
    BACK_FACE, FRONT_FACE, LEFT_FACE, RIGHT_FACE, BOTTOM_FACE, TOP_FACE &
    /), [12,6])

  integer(c_int), dimension(3, 6), parameter :: DIRECTIONS = reshape((/ &
    0,  0, -1, &
    0,  0,  1, &
    -1, 0,  0, &
    1,  0,  0, &
    0, -1,  0, &
    0,  1, 0 &
    /), [3,6])


  public :: chunk_mesh_initialize
  public :: chunk_mesh_generate


  type :: message_from_mesh_generator
    type(vec2i), pointer :: world_position
    real(c_float), dimension(12), pointer :: positions(:)
    real(c_float), dimension(8), pointer :: texture_coordinates(:)
    real(c_float), dimension(12), pointer :: colors(:)
    integer(c_int), dimension(6), pointer :: indices(:)
    integer(c_int) :: mesh_stack
  end type message_from_mesh_generator


  type(concurrent_linked_filo_queue) :: chunk_mesh_thread_output_queue

contains


  !* Basic initialization of the chunk mesh module.
  subroutine chunk_mesh_initialize()
    implicit none

    chunk_mesh_thread_output_queue = concurrent_linked_filo_queue()
  end subroutine chunk_mesh_initialize

  subroutine chunk_mesh_handle_output_queue()
    implicit none

    !! fixme: this should be passing it back into a concurrent FILO queue.

    ! mesh_id = "mesh_stack_"//int_to_string(chunk_pointer%world_position%x)//"_"//int_to_string(chunk_pointer%world_position%y)//"_"//int_to_string(mesh_stack)

    ! call mesh_create_3d(mesh_id, positions, texture_coordinates, colors, indices)

  end subroutine chunk_mesh_handle_output_queue


  !* This is the function which runs in the thread to actually generate chunk meshes.
  recursive function chunk_mesh_generation_thread(c_arg_pointer) result(void_pointer) bind(c)
    use, intrinsic :: iso_c_binding
    implicit none

    !? Parameters (IN).
    type(c_ptr), intent(in), value :: c_arg_pointer
    type(c_ptr) :: void_pointer
    type(thread_argument), pointer :: arguments
    type(chunk_mesh_generator_message), pointer :: generator_message
    !? Working variables.
    integer(c_int) :: status
    character(len = :, kind = c_char), allocatable :: mesh_id
    type(block_definition), pointer :: definition_pointer
    type(texture_rectangle), pointer :: tr_pointer
    ! Written like this to denote the multiplicative each should have.
    real(c_float), dimension(12), allocatable :: positions(:)
    real(c_float), dimension(8), allocatable :: texture_coordinates(:)
    real(c_float), dimension(12), allocatable :: colors(:)
    integer(c_int), dimension(6), allocatable :: indices(:)
    integer(c_int) :: limit, i, x, z, y, current_id, current_offset, p_index, t_index, c_index, i_index, base_y, max_y, current_rect_index
    type(vec3i) :: direction, pos, trajectory, offset
    type(message_from_mesh_generator), pointer :: output_message

    !? Transfer main argument pointer to Fortran.

    if (.not. c_associated(c_arg_pointer)) then
      error stop "[Chunk Mesh] Fatal error: Was passed a null thread_argument pointer."
    end if

    call c_f_pointer(c_arg_pointer, arguments)

    !? Transfer sent data pointer to Fortran.

    if (.not. c_associated(arguments%sent_data)) then
      error stop "[Chunk Mesh] Fatal error: Was passed a null sent_data pointer."
    end if

    call c_f_pointer(arguments%sent_data, generator_message)

    !? Ensure required components are present.

    if (.not. associated(generator_message%world_position)) then
      error stop "[Chunk Mesh] {thread} error: World position is a null pointer."
    end if

    if (.not. associated(generator_message%current)) then
      error stop "[Chunk Mesh] {thread} error: Current chunk is a null pointer."
    end if

    if (.not. associated(generator_message%texture_indices)) then
      error stop "[Chunk Mesh] {thread} error: Texture indices is a null pointer."
    end if

    if (.not. associated(generator_message%texture_positions_array)) then
      error stop "[Chunk Mesh] {thread} error: Texture positions array is a null pointer."
    end if

    if (generator_message%texture_count <= 0) then
      error stop "[Chunk Mesh] {thread} error: Texture count is 0 or less."
    end if

    if (generator_message%mesh_stack <= 0) then
      error stop "[Chunk Mesh] {thread} error: Mesh stack is 0 or less."
    end if


    !? Begin implementation.

    ! Now we assign.

    ! Limit it to 1 million faces per chunk.
    ! We will grab a slice of this later to shrink it.
    limit = 200000

    allocate(positions(12 * limit))
    allocate(texture_coordinates(8 * limit))
    allocate(colors(12 * limit))
    allocate(indices(6 * limit))

    current_offset = 0

    p_index = -1
    t_index = -1
    c_index = -1
    i_index = -1

    base_y = (MESH_STACK_HEIGHT * (generator_message%mesh_stack - 1)) + 1
    max_y = MESH_STACK_HEIGHT * (generator_message%mesh_stack)

    do x = 1,CHUNK_WIDTH
      do z = 1,CHUNK_WIDTH
        do y = base_y,max_y

          current_id = generator_message%current%data(y, z, x)%id

          ! Cycle on air.
          if (current_id == 0) then
            cycle
          end if

          ! Position in indices.
          pos = [x, y, z]

          ! Position in world offset.
          offset = [ &
            x - 1, &
            y - base_y, &
            z - 1 &
            ]

          do i = 1,6

            ! Direction we're looking towards.
            direction = DIRECTIONS(1:3, i)

            ! Block we're looking at.
            trajectory = pos + direction

            ! If we're going to go out of bounds, cycle.
            ! todo: check neighbor.
            if (trajectory%x < 1 .or. trajectory%x > CHUNK_WIDTH .or. trajectory%z < 1 .or. trajectory%z > CHUNK_WIDTH .or. trajectory%y < 1 .or. trajectory%y > CHUNK_HEIGHT) then
              cycle
            end if

            ! todo: if y height == chunk_height then just render the face.

            ! If it's another fullsize block, cycle.
            ! todo: check draw_type.
            if (generator_message%current%data(trajectory%y, trajectory%z, trajectory%x)%id /= 0) then
              cycle
            end if


            p_index = (current_offset * 12) + 1
            positions(p_index:p_index + 11) = (FACES(1:12, i) + (/offset%x, offset%y, offset%z, offset%x, offset%y, offset%z, offset%x, offset%y, offset%z, offset%x, offset%y, offset%z/))

            ! tr_pointer => texture_atlas_get_texture_rectangle_pointer(definition_pointer%textures(1)%get_pointer())
            current_rect_index = generator_message%texture_indices(i, current_id)
            tr_pointer => generator_message%texture_positions_array(current_rect_index)

            t_index = (current_offset * 8) + 1
            texture_coordinates(t_index:t_index + 7) = (/ &
              tr_pointer%min_x,tr_pointer%min_y, &
              tr_pointer%min_x,tr_pointer%max_y, &
              tr_pointer%max_x,tr_pointer%max_y, &
              tr_pointer%max_x,tr_pointer%min_y &
              /)

            c_index = (current_offset * 12) + 1
            colors(c_index:c_index + 11) = (/&
              1.0, 1.0, 1.0, &
              1.0, 1.0, 1.0, &
              1.0, 1.0, 1.0, &
              1.0, 1.0, 1.0 &
              /)

            ! ! I love Fortran's array intrinsics.
            i_index = (current_offset * 6) + 1
            indices(i_index:i_index + 5) = (BASE_INDICES + (4 * current_offset))

            current_offset = current_offset + 1
          end do
        end do
      end do
    end do

    ! It's a blank mesh.
    if (p_index == -1) then
      return
    end if

    p_index = p_index + 11
    t_index = t_index + 7
    c_index = c_index + 11
    i_index = i_index + 5

    positions = positions(1: p_index)
    texture_coordinates = texture_coordinates(1: t_index)
    colors = colors(1: c_index)
    indices = indices(1:i_index)

    !? Compose output.

    allocate(output_message)
    allocate(output_message%world_position)
    allocate(output_message%positions(p_index))
    allocate(output_message%texture_coordinates(t_index))
    allocate(output_message%colors(c_index))
    allocate(output_message%indices(i_index))

    output_message%world_position = generator_message%world_position
    output_message%positions = positions
    output_message%texture_coordinates = texture_coordinates
    output_message%colors = colors
    output_message%indices = indices
    output_message%mesh_stack = generator_message%mesh_stack

    !? Push it into the queue.

    call chunk_mesh_thread_output_queue%push(queue_data(output_message))





    !! fixme: this should be passing it back into a concurrent FILO queue.

    ! mesh_id = "mesh_stack_"//int_to_string(chunk_pointer%world_position%x)//"_"//int_to_string(chunk_pointer%world_position%y)//"_"//int_to_string(mesh_stack)

    ! call mesh_create_3d(mesh_id, positions, texture_coordinates, colors, indices)

    !? Deallocate all the memory regions in the message.

    if (associated(generator_message%current)) then
      deallocate(generator_message%current)
    end if

    if (associated(generator_message%left)) then
      deallocate(generator_message%left)
    end if

    if (associated(generator_message%right)) then
      deallocate(generator_message%right)
    end if

    if (associated(generator_message%back)) then
      deallocate(generator_message%back)
    end if

    if (associated(generator_message%front)) then
      deallocate(generator_message%front)
    end if

    deallocate(generator_message%texture_indices)

    deallocate(generator_message%texture_positions_array)

    !! Finally, deallocate the message itself.
    deallocate(generator_message)

    void_pointer = c_null_ptr
    status = thread_write_lock(arguments%mutex_pointer)
    arguments%active_flag = .false.
    status = thread_unlock_lock(arguments%mutex_pointer)
  end function chunk_mesh_generation_thread



  !* Queue up a chunk mesh generation call.
  subroutine chunk_mesh_generate(x,z, mesh_stack)
    use :: string
    implicit none

    integer(c_int), intent(in), value :: x, z, mesh_stack
    type(chunk_mesh_generator_message), pointer :: message_to_generator

    allocate(message_to_generator)

    allocate(message_to_generator%world_position)
    message_to_generator%world_position = [x, z]

    message_to_generator%current => chunk_handler_get_clone_chunk_pointer(x,z)

    message_to_generator%left => chunk_handler_get_clone_chunk_pointer(x - 1,z)
    message_to_generator%right => chunk_handler_get_clone_chunk_pointer(x + 1,z)

    message_to_generator%back => chunk_handler_get_clone_chunk_pointer(x,z - 1)
    message_to_generator%front => chunk_handler_get_clone_chunk_pointer(x,z + 1)

    message_to_generator%texture_indices => texture_atlas_get_texture_indices_clone_pointer()
    message_to_generator%texture_positions_array => texture_atlas_get_texture_positions_array_clone_pointer()
    message_to_generator%texture_count = texture_atlas_get_texture_count()

    message_to_generator%mesh_stack = mesh_stack

    call thread_create_detached(c_funloc(chunk_mesh_generation_thread), c_loc(message_to_generator))
  end subroutine chunk_mesh_generate


end module chunk_mesh
