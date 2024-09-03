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

  integer(c_int), dimension(3, 6), parameter :: DIRECTIONS = reshape((/ &
    0,0,0, &
    0,0,0, &
    0,0,0, &
    0,0,0, &
    0,0,0, &
    0,0,0 &
    /), [3,6])

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
    integer(c_int) :: limit, index_of_face, x, z, y, x_pos, y_pos, z_pos, current_offset, p_index, t_index, c_index, i_index

    !! debugging one block, ID 1 (Stone)

    ! Very pointy. =>
    definition_pointer => block_repo_get_definition_pointer_by_id(1)

    ! Now we assign.

    ! Limit it to 1 million faces per chunk.
    ! We will grab a slice of this later to shrink it.
    limit = 1000000

    allocate(positions(12 * limit))
    allocate(texture_coordinates(8 * limit))
    allocate(colors(12 * limit))
    allocate(indices(6 * limit))

    !? We are working with raw numbers, doing the block reallocation should not cause issues here.

    current_offset = 0


    print*,"START"
    x = 0
    y = 0
    z = 0

    do x = 1,CHUNK_WIDTH
      do z = 1,CHUNK_WIDTH
        do y = 1,CHUNK_HEIGHT

          x_pos = x - 1
          y_pos = y - 1
          z_pos = z - 1
          do index_of_face = 1,6

            ! if ((x /= 1 .and. x /= CHUNK_WIDTH) .and. (z /= 1 .and. z /= CHUNK_WIDTH) .and. (y /= 1 .and. y /= CHUNK_HEIGHT)) then
            !   cycle
            ! end if

            p_index = (current_offset * 12) + 1
            positions(p_index:p_index + 11) = (FACES(1:12, index_of_face) + (/x_pos,y_pos,z_pos, x_pos,y_pos,z_pos, x_pos,y_pos,z_pos, x_pos,y_pos,z_pos/))

            tr_pointer => texture_atlas_get_texture_rectangle_pointer(definition_pointer%textures(1)%get_pointer())

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

    p_index = p_index + 11
    t_index = t_index + 7
    c_index = c_index + 11
    i_index = i_index + 5

    positions = positions(1: p_index)
    texture_coordinates = texture_coordinates(1: t_index)
    colors = colors(1: c_index)
    indices = indices(1:i_index)
    print*,"END"

    call mesh_create_3d("debug_block", positions, texture_coordinates, colors, indices)

    mesh_id = "debug_block"
  end function chunk_mesh_generate


end module chunk_mesh
