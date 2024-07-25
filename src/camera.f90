module camera
  use :: matrix_4f
  implicit none

  private

  !? On the stack, for now. Uses 128 bytes.
  type(mat4f) :: camera_matrix

contains

  subroutine update_camera_matrix()
    implicit none

    call camera_matrix%identity()


  end subroutine update_camera_matrix


end module camera
