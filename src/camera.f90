module camera
  use :: matrix_4f
  use :: vector_3f
  use :: vector_3d
  use :: math_helpers, only: into_f32
  use, intrinsic :: iso_c_binding, only: c_float, c_double
  implicit none


  private


  public :: camera_update
  public :: camera_set_position
  public :: camera_set_position_vec3d
  public :: camera_set_object_matrix_f32
  public :: camera_set_object_matrix_f64


  real(c_float), parameter :: MIN_FOV = 50.0
  real(c_float), parameter :: MAX_FOV = 120.0

  real(c_float) :: fov_degrees = 72.0

  real(c_float) :: z_near = 0.01
  real(c_float) :: z_far = 100.0


  !? Position is not translation, translation is the inverse of position!
  type(vec3d) :: camera_position
  type(vec3d) :: camera_rotation
  ! Camera is always at scale 1, 1, 1.


contains


  real(c_float) function camera_get_z_near() result(near)
    implicit none

    near = z_near
  end function camera_get_z_near


  real(c_float) function camera_get_z_far() result(far)
    implicit none

    far = z_far
  end function camera_get_z_far


  subroutine camera_set_position(x, y, z)
    implicit none

    real(c_double), intent(in), value :: x, y, z

    camera_position%x = x
    camera_position%y = y
    camera_position%z = z
  end subroutine camera_set_position


  subroutine camera_set_position_vec3d(new_position)
    implicit none

    type(vec3d), intent(in), value :: new_position

    camera_position = new_position
  end subroutine camera_set_position_vec3d


  subroutine camera_rotate(x, y, z)
    implicit none

    !* This automatically handles PI2 wrapping.

    real(c_double), intent(in), value :: x, y, z

    camera_position%x = camera_position%x + x
    camera_position%y = camera_position%x + y
    camera_position%z = camera_position%x + z

    call wrap_camera_rotation()
  end subroutine camera_rotate


  subroutine camera_rotate_vec3d(vec)
    implicit none

    !* This automatically handles PI2 wrapping.

    type(vec3d), intent(in), value :: vec

    camera_position = camera_position + vec

    call wrap_camera_rotation()
  end subroutine camera_rotate_vec3d


  !* This creates the raw data of the camera matrix then uploads it into OpenGL.
  subroutine camera_update()
    use :: glfw, only: glfw_get_aspect_ratio
    use :: math_helpers, only: to_radians_f32
    use :: shader, only: shader_get_uniform
    use :: opengl, only: gl_uniform_mat4f
    implicit none

    !* So the trick is, the camera actually never moves, but the world moves around it.
    !* This maintains as much precision as possible where you can see it.

    type(mat4f) :: camera_matrix

    call camera_matrix%identity()

    call camera_matrix%perspective(to_radians_f32(fov_degrees), glfw_get_aspect_ratio(), z_near, z_far)

    call camera_matrix%rotate_y(camera_rotation%y_f32())
    call camera_matrix%rotate_x(camera_rotation%x_f32())
    call camera_matrix%rotate_z(camera_rotation%z_f32())

    call gl_uniform_mat4f(shader_get_uniform("main", "camera_matrix"), camera_matrix)
  end subroutine camera_update


  !* This creates the raw data of the object matrix then uploads it into OpenGL.
  subroutine camera_set_object_matrix_f32(position_x, position_y, position_z, rotation_x, rotation_y, rotation_z, scale_x, scale_y, scale_z)
    use :: math_helpers, only: into_f32
    use :: shader, only: shader_get_uniform
    use :: opengl, only: gl_uniform_mat4f
    implicit none

    real(c_float), intent(in), value :: position_x, position_y, position_z, rotation_x, rotation_y, rotation_z, scale_x, scale_y, scale_z
    type(mat4f) :: object_matrix

    call object_matrix%identity()

    call object_matrix%translate( &
      into_f32(position_x - camera_position%x), &
      into_f32(position_y - camera_position%y), &
      into_f32(position_z - camera_position%z) &
      )

    call object_matrix%rotate_y(-rotation_y)
    call object_matrix%rotate_x(-rotation_x)
    call object_matrix%rotate_z(-rotation_z)

    call object_matrix%scale(&
      scale_x, &
      scale_y, &
      scale_z &
      )

    call gl_uniform_mat4f(shader_get_uniform("main", "object_matrix"), object_matrix)
  end subroutine camera_set_object_matrix_f32


  !* This creates the raw data of the object matrix then uploads it into OpenGL.
  subroutine camera_set_object_matrix_f64(position_x, position_y, position_z, rotation_x, rotation_y, rotation_z, scale_x, scale_y, scale_z)
    use :: math_helpers, only: into_f32
    use :: shader, only: shader_get_uniform
    use :: opengl, only: gl_uniform_mat4f
    implicit none

    real(c_double), intent(in), value :: position_x, position_y, position_z, rotation_x, rotation_y, rotation_z, scale_x, scale_y, scale_z
    type(mat4f) :: object_matrix

    call object_matrix%identity()

    call object_matrix%translate( &
      into_f32(position_x - camera_position%x), &
      into_f32(position_y - camera_position%y), &
      into_f32(position_z - camera_position%z) &
      )

    call object_matrix%rotate_y(into_f32(-rotation_y))
    call object_matrix%rotate_x(into_f32(-rotation_x))
    call object_matrix%rotate_z(into_f32(-rotation_z))

    call object_matrix%scale(&
      into_f32(scale_x), &
      into_f32(scale_y), &
      into_f32(scale_z) &
      )

    call gl_uniform_mat4f(shader_get_uniform("main", "object_matrix"), object_matrix)
  end subroutine camera_set_object_matrix_f64


  !* Internal only.


  subroutine wrap_camera_rotation()
    use :: constants, only: PI_TIMES_2_F64
    implicit none

    if (camera_position%x < 0.0d0 .or. camera_position%x > PI_TIMES_2_F64) then
      camera_position%x = mod(camera_position%x, PI_TIMES_2_F64)
    end if
    if (camera_position%y < 0.0d0 .or. camera_position%y > PI_TIMES_2_F64) then
      camera_position%y = mod(camera_position%y, PI_TIMES_2_F64)
    end if
    if (camera_position%z < 0.0d0 .or. camera_position%z > PI_TIMES_2_F64) then
      camera_position%z = mod(camera_position%z, PI_TIMES_2_F64)
    end if
  end subroutine wrap_camera_rotation


end module camera
