module camera
  use :: matrix_4f
  use :: vector_3f
  use, intrinsic :: iso_c_binding, only: c_float, c_double
  implicit none

  private

  public :: camera_update_matrix


  real(c_float), parameter :: MIN_FOV = 50.0
  real(c_float), parameter :: MAX_FOV = 120.0

  logical :: up = .true.
  real(c_float) :: fov_degrees = 72.0

  !? On the stack, for now. Uses 64 bytes. I don't feel like listing the rest of the sizes.
  type(mat4f) :: camera_matrix
  !? Position is not translation, translation is the inverse of position!
  type(vec3f) :: camera_position

  real(c_float) :: debug_rotation

contains


  subroutine camera_update_matrix()
    use :: glfw, only: glfw_get_aspect_ratio
    use :: delta
    use :: math_helpers, only: to_radians_f32

    implicit none

    real(c_double) :: gotten_delta

    gotten_delta = get_delta_f64()

    ! print"(f00.30)",gotten_delta

    ! print"(f0.5)",fov_degrees

    ! if (up) then
    !   fov_degrees = real(fov_degrees + gotten_delta * 100.0d0, kind = c_float)

    !   if (fov_degrees >= MAX_FOV) then
    !     fov_degrees = MAX_FOV
    !     up = .false.
    !   end if
    ! else
    !   fov_degrees = real(fov_degrees - gotten_delta * 100.0d0, kind = c_float)
    !   if (fov_degrees <= MIN_FOV) then
    !     fov_degrees = MIN_FOV
    !     up = .true.
    !   end if
    ! end if

    if (up) then
      debug_rotation = debug_rotation + real(gotten_delta)
      if (debug_rotation > to_radians_f32(45.0)) then
        debug_rotation = to_radians_f32(45.0)
        up = .false.
      end if
    else
      debug_rotation = debug_rotation - real(gotten_delta)
      if (debug_rotation < to_radians_f32(-45.0)) then
        debug_rotation = to_radians_f32(-45.0)
        up = .true.
      end if
    end if
    print"(f0.10)", debug_rotation

    call camera_matrix%identity()

    call camera_matrix%perspective(to_radians_f32(fov_degrees), glfw_get_aspect_ratio(), 0.01, 100.0)

    call camera_matrix%rotate_x(debug_rotation)

    !* So the trick is, the camera actually never moves, but the world moves around it.
    !* This maintains as much precision as possible where you can see it.

    call upload_camera_matrix_into_shader()
  end subroutine camera_update_matrix


  subroutine upload_camera_matrix_into_shader()
    use :: opengl
    use :: shader
    implicit none

    call gl_uniform_mat4f(shader_get_uniform("main", "camera_matrix"), camera_matrix)

    !TODO: REMOVE THIS!
    call gl_uniform_mat4f(shader_get_uniform("main", "object_matrix"), mat4f())
  end subroutine upload_camera_matrix_into_shader


end module camera
