module chunk_mesh
  use :: mesh
  use :: chunk_data
  use :: block_repo
  use :: texture_atlas
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

  public :: chunk_mesh_generate


contains


  function chunk_mesh_generate(input_chunk) result(mesh_id)
    implicit none

    type(memory_chunk), intent(in) :: input_chunk
    character(len = :, kind = c_char), allocatable :: mesh_id
    type(block_definition), pointer :: definition_pointer
    type(texture_rectangle), pointer :: tr_pointer
    ! Written like this to denote the multiplicative each should have.
    real(c_float), dimension(12), allocatable :: positions(:)
    real(c_float), dimension(8), allocatable :: texture_coordinates(:)
    real(c_float), dimension(12), allocatable :: colors(:)
    integer(c_int), dimension(6), allocatable :: indices(:)
    integer(c_int) :: i

    !! debugging one block, ID 1 (Stone)

    ! Very pointy. =>
    definition_pointer => block_repo_get_definition_pointer_by_id(1)

    ! Now we assign.

    allocate(positions(0))
    allocate(texture_coordinates(0))
    allocate(colors(0))
    allocate(indices(0))

    do i = 1,6
      positions = [positions, FACES(1:12,i)]

      tr_pointer => texture_atlas_get_texture_rectangle_pointer(definition_pointer%textures(1)%get_pointer())

      texture_coordinates = [texture_coordinates, (/ &
        tr_pointer%min_x,tr_pointer%min_y, &
        tr_pointer%min_x,tr_pointer%max_y, &
        tr_pointer%max_x,tr_pointer%max_y, &
        tr_pointer%max_x,tr_pointer%min_y &
        /)]

      colors = [colors, (/&
        1.0, 1.0, 1.0, &
        1.0, 1.0, 1.0, &
        1.0, 1.0, 1.0, &
        1.0, 1.0, 1.0 &
        /)]

      ! I love Fortran's array intrinsics.
      indices = [indices, BASE_INDICES + (4 * (i - 1))]
    end do

    call mesh_create_3d("debug_block", positions, texture_coordinates, colors, indices)

    mesh_id = "debug_block"
  end function chunk_mesh_generate


end module chunk_mesh
