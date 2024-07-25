module camera
  use :: matrix_4f
  use, intrinsic :: iso_c_binding, only: c_float
  implicit none

  private

  public :: camera_update_matrix

  real(c_float) :: fov_degrees = 72.0

  !? On the stack, for now. Uses 64 bytes.
  type(mat4f) :: camera_matrix

contains


  subroutine camera_update_matrix()
    use :: glfw, only: glfw_get_aspect_ratio
    use :: math_helpers, only: to_radians_f32

    implicit none

    call camera_matrix%identity()

    call camera_matrix%perspective(to_radians_f32(fov_degrees), glfw_get_aspect_ratio(), 0.01, 100.0)

    ! print*,camera_matrix%data

    call upload_camera_matrix_into_shader()
  end subroutine camera_update_matrix


  subroutine upload_camera_matrix_into_shader()
    use :: opengl
    use :: shader
    implicit none

    call gl_uniform_mat4f(shader_get_uniform("main", "camera_matrix"), camera_matrix)

    call gl_uniform_mat4f(shader_get_uniform("main", "object_matrix"), mat4f())
  end subroutine upload_camera_matrix_into_shader


end module camera
