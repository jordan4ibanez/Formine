module camera
  use :: matrix_4f
  implicit none

  private

  public :: camera_update_matrix

  !? On the stack, for now. Uses 64 bytes.
  type(mat4f) :: camera_matrix

contains

  subroutine camera_update_matrix()
    use :: glfw, only: glfw_get_aspect_ratio

    implicit none

    call camera_matrix%identity()

    call camera_matrix%perspective(60.0, glfw_get_aspect_ratio(), 0.1, 100.0)

    ! print*,camera_matrix%data

  end subroutine camera_update_matrix


end module camera
