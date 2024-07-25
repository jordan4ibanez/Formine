module camera
  use :: matrix_4f
  implicit none

  private

  public :: camera_update_matrix

  !? On the stack, for now. Uses 128 bytes.
  type(mat4f) :: camera_matrix

contains

  subroutine camera_update_matrix()
    use :: glfw, only: glfw_get_aspect_ratio

    implicit none

    call camera_matrix%identity()

    call camera_matrix%perspective(60.0, glfw_get_aspect_ratio(), 0.01, 100.0)


  end subroutine camera_update_matrix


end module camera
