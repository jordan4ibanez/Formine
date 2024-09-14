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
  public :: chunk_mesh_handle_output_queue
  public :: chunk_mesh_generate

  !* The data that will get sent to the chunk mesh generator.
  type :: chunk_mesh_generator_message
    !* The position in the world in which the chunk resides.
    type(vec2i), pointer :: world_position => null()
    !* Current chunk.
    type(memory_chunk), pointer :: current => null()
    !* Neighbor: -X
    type(memory_chunk), pointer :: left => null()
    !* Neighbor: +X
    type(memory_chunk), pointer :: right => null()
    !* Neighbor: -Z
    type(memory_chunk), pointer :: back => null()
    !* Neighbor: +Z
    type(memory_chunk), pointer :: front => null()
    !* Pointer of texture indices into the positions array.
    integer(c_int), dimension(6,0), pointer :: texture_indices(:, :)
    !* Texture positions array.
    type(texture_rectangle), dimension(:), pointer :: texture_positions_array
    !* Total number of textures.
    integer(c_int) :: texture_count = 0
    !* Which stack portion to generate.
    integer(c_int) :: mesh_stack = -1
  end type chunk_mesh_generator_message

  type :: message_from_mesh_generator
    type(vec2i), pointer :: world_position
    real(c_float), dimension(12), pointer :: positions(:)
    real(c_float), dimension(8), pointer :: texture_coordinates(:)
    real(c_float), dimension(12), pointer :: colors(:)
    integer(c_int), dimension(6), pointer :: indices(:)
    integer(c_int) :: mesh_stack
  end type message_from_mesh_generator


  type(concurrent_linked_filo_queue) :: thread_output_queue
  ! todo: make this a setting in the game's menu.
  integer(c_int) :: queue_pop_limit = 16


contains


  !* Basic initialization of the chunk mesh module.
  subroutine chunk_mesh_initialize()
    implicit none

    thread_output_queue = concurrent_linked_filo_queue()
  end subroutine chunk_mesh_initialize


  !* Handle output from the thread output queue.
  subroutine chunk_mesh_handle_output_queue()
    implicit none

    integer(c_int) :: i, total
    class(*), pointer :: generic_pointer
    type(message_from_mesh_generator), pointer :: new_message
    character(len = :, kind = c_char), allocatable :: mesh_id


    do i = 1,queue_pop_limit

      if (.not. thread_output_queue%pop(generic_pointer)) then
        total = i - 1
        exit
      end if

      select type(generic_pointer)
       type is (message_from_mesh_generator)
        new_message => generic_pointer
       class default
        error stop "[Chunk Mesh] Error: Wrong type in queue."
      end select

      mesh_id = "mesh_stack_"//int_to_string(new_message%world_position%x)//"_"//int_to_string(new_message%world_position%y)//"_"//int_to_string(new_message%mesh_stack)

      call mesh_create_3d(mesh_id, new_message%positions, new_message%texture_coordinates, new_message%colors, new_message%indices)

      call chunk_handler_set_chunk_mesh(new_message%world_position%x, new_message%world_position%y, new_message%mesh_stack, mesh_id)

      !? This is running through the main thread so we can free it now.

      deallocate(new_message%world_position)
      deallocate(new_message%positions)
      deallocate(new_message%texture_coordinates)
      deallocate(new_message%colors)
      deallocate(new_message%indices)
      deallocate(new_message)

    end do
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
    type(texture_rectangle), pointer :: tr_pointer
    ! ! Written like this to denote the multiplicative each should have.
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

    if (.not. c_associated(arguments%data)) then
      error stop "[Chunk Mesh] Fatal error: Was passed a null sent_data pointer."
    end if

    call c_f_pointer(arguments%data, generator_message)

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

            ! I love Fortran's array intrinsics.
            i_index = (current_offset * 6) + 1
            indices(i_index:i_index + 5) = (BASE_INDICES + (4 * current_offset))

            current_offset = current_offset + 1
          end do
        end do
      end do
    end do

    ! It's not a blank mesh.
    if (p_index > -1) then

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
      call thread_output_queue%push(output_message)
    end if

    deallocate(positions)
    deallocate(texture_coordinates)
    deallocate(colors)
    deallocate(indices)


    !? Deallocate all the memory regions in the message.

    deallocate(generator_message%world_position)

    deallocate(generator_message%current)

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

    deallocate(generator_message)

    status = thread_write_lock(arguments%mutex_pointer)

    void_pointer = c_null_ptr
    arguments%active_flag = .false.

    status = thread_unlock_lock(arguments%mutex_pointer)
  end function chunk_mesh_generation_thread


  !* The GC to free the memory sent to the thread.
  subroutine chunk_mesh_generation_garbage_collector(sent_data)
    use :: raw_c
    implicit none

    type(c_ptr), intent(in), value :: sent_data
    type(chunk_mesh_generator_message), pointer :: message_to_generator

    call c_f_pointer(sent_data, message_to_generator)

    ! deallocate(message_to_generator)
    call c_free(sent_data)
  end subroutine chunk_mesh_generation_garbage_collector



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

    !! Sample deallocation
    ! deallocate(message_to_generator%world_position)

    ! deallocate(message_to_generator%current)

    ! if (associated(message_to_generator%left)) then
    !   deallocate(message_to_generator%left)
    ! end if
    ! if (associated(message_to_generator%right)) then
    !   deallocate(message_to_generator%right)
    ! end if

    ! if (associated(message_to_generator%back)) then
    !   deallocate(message_to_generator%back)
    ! end if
    ! if (associated(message_to_generator%front)) then
    !   deallocate(message_to_generator%front)
    ! end if

    ! deallocate(message_to_generator%texture_indices)
    ! deallocate(message_to_generator%texture_positions_array)

    ! deallocate(message_to_generator)

    call thread_create(c_funloc(chunk_mesh_generation_thread), c_loc(message_to_generator))
  end subroutine chunk_mesh_generate




end module chunk_mesh
