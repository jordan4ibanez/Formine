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


  !? -Z (Facing camera at rotation 0.0)
  real(c_float), dimension(12), parameter :: BACK_FACE = (/ &
    0.0, 1.0, 0.0, & ! Top left.
    0.0, 0.0, 0.0, & ! Bottom Left.
    1.0, 0.0, 0.0, & ! Bottom Right.
    1.0, 1.0, 0.0 &  ! Top Right.
    /)

  !? +Z (Facing away from camera at rotation 0.0)
  real(c_float), dimension(12), parameter :: FRONT_FACE = (/ &
    1.0, 1.0, 1.0, & ! Top Right.
    1.0, 0.0, 1.0, & ! Bottom Right.
    0.0, 0.0, 1.0, & ! Bottom Left.
    0.0, 1.0, 1.0 &  ! Top left.
    /)

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

    !! debugging one block, ID 1 (Stone)

    !! Face 1 -Z

    ! Very pointy. =>
    definition_pointer => block_repo_get_definition_pointer_by_id(1)

    !* Tested with debug_texture.png, it works in right handed.
    tr_pointer => texture_atlas_get_texture_rectangle_pointer(definition_pointer%textures(1)%get_pointer())

    ! Now we assign.

    !? -Z
    positions = BACK_FACE
    texture_coordinates = (/ &
      tr_pointer%min_x,tr_pointer%max_y, &
      tr_pointer%min_x,tr_pointer%min_y, &
      tr_pointer%max_x,tr_pointer%min_y, &
      tr_pointer%max_x,tr_pointer%max_y &
      /)
    colors = (/&
      1.0, 1.0, 1.0, &
      1.0, 1.0, 1.0, &
      1.0, 1.0, 1.0, &
      1.0, 1.0, 1.0 &
      /)
    indices = BASE_INDICES

    !? +Z

    !! this causes a memory leak!
    positions = [positions, FRONT_FACE]

    texture_coordinates = [texture_coordinates, (/ &
      tr_pointer%min_x,tr_pointer%max_y, &
      tr_pointer%min_x,tr_pointer%min_y, &
      tr_pointer%max_x,tr_pointer%min_y, &
      tr_pointer%max_x,tr_pointer%max_y &
      /)]

    colors = [colors, (/&
      1.0, 1.0, 1.0, &
      1.0, 1.0, 1.0, &
      1.0, 1.0, 1.0, &
      1.0, 1.0, 1.0 &
      /)]
    indices = [indices, (BASE_INDICES + 4)]



    call mesh_create_3d("debug_block", positions, texture_coordinates, colors, indices)

    mesh_id = "debug_block"
  end function chunk_mesh_generate


end module chunk_mesh
