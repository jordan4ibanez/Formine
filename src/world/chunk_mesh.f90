module chunk_mesh
  use :: mesh
  use :: chunk_data
  use :: block_repo
  use :: texture_atlas
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

  public :: chunk_mesh_generate


contains

  recursive function chunk_mesh_generation_thread(c_arg_pointer) result(void_pointer) bind(c)
    use, intrinsic :: iso_c_binding
    implicit none

    type(c_ptr), intent(in), value :: c_arg_pointer
    type(c_ptr) :: void_pointer
    type(thread_argument), pointer :: arguments
    type(chunk_mesh_generator_message), pointer :: generator_message

    integer(c_int) :: status
    character(len = :, kind = c_char), allocatable :: mesh_id
    type(block_definition), pointer :: definition_pointer
    type(texture_rectangle), pointer :: tr_pointer
    ! Written like this to denote the multiplicative each should have.
    real(c_float), dimension(12), allocatable :: positions(:)
    real(c_float), dimension(8), allocatable :: texture_coordinates(:)
    real(c_float), dimension(12), allocatable :: colors(:)
    integer(c_int), dimension(6), allocatable :: indices(:)
    integer(c_int) :: limit, i, x, z, y, current_offset, p_index, t_index, c_index, i_index, base_y, max_y
    type(vec3i) :: direction, pos, trajectory, offset

    if (.not. c_associated(c_arg_pointer)) then
      error stop "[Chunk Mesh] Fatal error: Was passed a null thread_argument pointer."
    end if

    call c_f_pointer(c_arg_pointer, arguments)

    print*,"hi"

    ! if (.not. c_associated(arguments%sent_data)) then
    !   error stop "[Chunk Mesh] Fatal error: Was passed a null sent_data pointer."
    ! end if

    ! call c_f_pointer(arguments%sent_data, generator_message)


    ! !!fixme: this is EXTREMELY unsafe.
    ! ! Very pointy. =>
    ! definition_pointer => block_repo_get_definition_pointer_by_id(1)

    ! ! Now we assign.

    ! ! Limit it to 1 million faces per chunk.
    ! ! We will grab a slice of this later to shrink it.
    ! limit = 200000

    ! allocate(positions(12 * limit))
    ! allocate(texture_coordinates(8 * limit))
    ! allocate(colors(12 * limit))
    ! allocate(indices(6 * limit))

    ! current_offset = 0

    ! p_index = -1
    ! t_index = -1
    ! c_index = -1
    ! i_index = -1

    ! base_y = (MESH_STACK_HEIGHT * (generator_message%mesh_stack - 1)) + 1
    ! max_y = MESH_STACK_HEIGHT * (generator_message%mesh_stack)

    ! do x = 1,CHUNK_WIDTH
    !   do z = 1,CHUNK_WIDTH
    !     do y = base_y,max_y

    !       ! Position in indices.
    !       pos = [x, y, z]

    !       ! Cycle on air.
    !       if (generator_message%current%data(pos%y, pos%z, pos%x)%id == 0) then
    !         cycle
    !       end if

    !       ! Position in world offset.
    !       offset = [ &
    !         x - 1, &
    !         y - base_y, &
    !         z - 1 &
    !         ]

    !       do i = 1,6

    !         ! Direction we're looking towards.
    !         direction = DIRECTIONS(1:3, i)

    !         ! Block we're looking at.
    !         trajectory = pos + direction

    !         ! If we're going to go out of bounds, cycle.
    !         ! todo: check neighbor.
    !         if (trajectory%x < 1 .or. trajectory%x > CHUNK_WIDTH .or. trajectory%z < 1 .or. trajectory%z > CHUNK_WIDTH .or. trajectory%y < 1 .or. trajectory%y > CHUNK_HEIGHT) then
    !           cycle
    !         end if

    !         ! todo: if y height == chunk_height then just render the face.

    !         ! If it's another fullsize block, cycle.
    !         ! todo: check draw_type.
    !         if (generator_message%current%data(trajectory%y, trajectory%z, trajectory%x)%id /= 0) then
    !           cycle
    !         end if


    !         p_index = (current_offset * 12) + 1
    !         positions(p_index:p_index + 11) = (FACES(1:12, i) + (/offset%x, offset%y, offset%z, offset%x, offset%y, offset%z, offset%x, offset%y, offset%z, offset%x, offset%y, offset%z/))

    !         tr_pointer => texture_atlas_get_texture_rectangle_pointer(definition_pointer%textures(1)%get_pointer())

    !         t_index = (current_offset * 8) + 1
    !         texture_coordinates(t_index:t_index + 7) = (/ &
    !           tr_pointer%min_x,tr_pointer%min_y, &
    !           tr_pointer%min_x,tr_pointer%max_y, &
    !           tr_pointer%max_x,tr_pointer%max_y, &
    !           tr_pointer%max_x,tr_pointer%min_y &
    !           /)

    !         c_index = (current_offset * 12) + 1
    !         colors(c_index:c_index + 11) = (/&
    !           1.0, 1.0, 1.0, &
    !           1.0, 1.0, 1.0, &
    !           1.0, 1.0, 1.0, &
    !           1.0, 1.0, 1.0 &
    !           /)

    !         ! ! I love Fortran's array intrinsics.
    !         i_index = (current_offset * 6) + 1
    !         indices(i_index:i_index + 5) = (BASE_INDICES + (4 * current_offset))

    !         current_offset = current_offset + 1
    !       end do
    !     end do
    !   end do
    ! end do

    ! ! It's a blank mesh.
    ! if (p_index == -1) then
    !   mesh_id = ""
    !   return
    ! end if

    ! p_index = p_index + 11
    ! t_index = t_index + 7
    ! c_index = c_index + 11
    ! i_index = i_index + 5

    ! positions = positions(1: p_index)
    ! texture_coordinates = texture_coordinates(1: t_index)
    ! colors = colors(1: c_index)
    ! indices = indices(1:i_index)


    !! fixme: this should be passing it back into a concurrent FILO queue.

    ! mesh_id = "mesh_stack_"//int_to_string(chunk_pointer%world_position%x)//"_"//int_to_string(chunk_pointer%world_position%y)//"_"//int_to_string(mesh_stack)

    ! call mesh_create_3d(mesh_id, positions, texture_coordinates, colors, indices)


    void_pointer = c_null_ptr
    status = thread_write_lock(arguments%mutex_pointer)
    arguments%active_flag = .false.
    status = thread_unlock_lock(arguments%mutex_pointer)
  end function chunk_mesh_generation_thread



  subroutine chunk_mesh_generate(x,z, mesh_stack)
    use :: string
    implicit none

    integer(c_int), intent(in), value :: x, z, mesh_stack


    call thread_create_detached(c_funloc(chunk_mesh_generation_thread), c_null_ptr)!c_loc(test_data))
  end subroutine chunk_mesh_generate


end module chunk_mesh
