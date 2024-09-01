module chunk_mesh
  use :: mesh
  use :: chunk_data
  use :: block_repo
  use :: texture_atlas
  use, intrinsic :: iso_c_binding
  implicit none

  ! +X RIGHT
  !
  ! +Y UP
  !
  ! +Z FORWARD (maybe)
  !
  ! Negatives will be root. (0.0, 0.0, 0.0)
  !
  ! 1 _______ 4
  !  |\      |
  !  |  \    | <= Counter-clockwise.
  !  |    \  |
  !  |______\|
  ! 2         3
  private

  !? -Z
  real(c_float), dimension(12), parameter :: BACK_FACE = (/ &
    0.0, 1.0, 0.0, & ! Top left.
    0.0, 0.0, 0.0, & ! Bottom Left.
    1.0, 0.0, 0.0, & ! Bottom Right.
    1.0, 1.0, 0.0 &  ! Top Right.
    /)


  public :: chunk_mesh_generate


contains


  function chunk_mesh_generate(input_chunk) result(mesh_id)
    implicit none

    type(memory_chunk), intent(in) :: input_chunk
    character(len = :, kind = c_char), allocatable :: mesh_id

    !! debugging one block

  end function chunk_mesh_generate


end module chunk_mesh
