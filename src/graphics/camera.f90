module camera
  use :: matrix_4f
  use :: vector_3f
  use :: vector_3d
  use :: math_helpers, only: into_f32
  use, intrinsic :: iso_c_binding, only: c_float, c_double
  implicit none


  private


  public :: camera_update_3d
  public :: camera_update_2d
  public :: camera_set_position
  public :: camera_set_position_vec3d
  public :: camera_set_object_matrix_f32
  public :: camera_set_object_matrix_f64
  public :: camera_freecam_hackjob


  real(c_float), parameter :: MIN_FOV = 50.0
  real(c_float), parameter :: MAX_FOV = 120.0

  real(c_float) :: fov_degrees = 72.0

  real(c_float) :: z_near_3d = 0.01
  real(c_float) :: z_far_3d = 100.0

  real(c_float) :: z_near_2d = -1.0
  real(c_float) :: z_far_2d = 1.0


  !? Position is not translation, translation is the inverse of position!
  type(vec3d) :: camera_position
  type(vec3d) :: camera_rotation
  ! Camera is always at scale 1, 1, 1.


contains

  subroutine camera_set_position(x, y, z)
    implicit none

    real(c_double), intent(in), value :: x, y, z

    camera_position%x = x
    camera_position%y = y
    camera_position%z = z
  end subroutine camera_set_position


  subroutine camera_set_position_vec3d(position_new)
    implicit none

    type(vec3d), intent(in), value :: position_new

    camera_position = position_new
  end subroutine camera_set_position_vec3d


  subroutine camera_rotate(x, y, z)
    implicit none

    !* This automatically handles PI2 wrapping.

    real(c_double), intent(in), value :: x, y, z

    camera_rotation%x = camera_rotation%x + x
    camera_rotation%y = camera_rotation%y + y
    camera_rotation%z = camera_rotation%z + z

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
  !* This sets the camera into "3D mode"
  subroutine camera_update_3d()
    use :: glfw, only: glfw_get_aspect_ratio
    use :: math_helpers, only: to_radians_f32
    use :: shader
    use :: opengl, only: gl_uniform_mat4f, gl_depth_range_f
    implicit none

    !* So the trick is, the camera actually never moves, but the world moves around it.
    !* This maintains as much precision as possible where you can see it.

    type(mat4f) :: camera_matrix

    call camera_matrix%identity()

    call camera_matrix%perspective(to_radians_f32(fov_degrees), glfw_get_aspect_ratio(), z_near_3d, z_far_3d)

    call camera_matrix%rotate_y(camera_rotation%y_f32())
    call camera_matrix%rotate_x(camera_rotation%x_f32())
    call camera_matrix%rotate_z(camera_rotation%z_f32())

    !* This synchronizes the camera's depth matrix with OpenGL.
    call gl_depth_range_f(z_near_3d, z_far_3d)

    call gl_uniform_mat4f(UNIFORM_CAMERA_MATRIX, camera_matrix)
  end subroutine camera_update_3d


  !* This creates the raw data of the camera matrix then uploads it into OpenGL.
  !* This sets the camera into "2D mode"
  subroutine camera_update_2d()
    use :: glfw
    use :: math_helpers, only: to_radians_f32
    use :: shader
    use :: opengl, only: gl_uniform_mat4f, gl_depth_range_f
    implicit none

    type(mat4f) :: camera_matrix
    real(c_float) :: width, height

    !* So the trick is, the camera actually never moves, but the world moves around it.
    !* This maintains as much precision as possible where you can see it.

    width = glfw_get_window_width_f32() / 2.0
    height = glfw_get_window_height_f32() / 2.0

    call camera_matrix%identity()

    call camera_matrix%set_ortho_2d(-width, width, -height, height)

    call camera_matrix%rotate_y(camera_rotation%y_f32())
    call camera_matrix%rotate_x(camera_rotation%x_f32())
    call camera_matrix%rotate_z(camera_rotation%z_f32())

    !* This synchronizes the camera's depth matrix with OpenGL.
    call gl_depth_range_f(z_near_2d, z_far_2d)

    call gl_uniform_mat4f(UNIFORM_CAMERA_MATRIX, camera_matrix)
  end subroutine camera_update_2d


  !* This creates the raw data of the object matrix then uploads it into OpenGL.
  subroutine camera_set_object_matrix_f32(position_x, position_y, position_z, rotation_x, rotation_y, rotation_z, scale_x, scale_y, scale_z)
    use :: math_helpers, only: into_f32
    use :: shader
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

    call gl_uniform_mat4f(UNIFORM_OBJECT_MATRIX, object_matrix)
  end subroutine camera_set_object_matrix_f32


  !* This creates the raw data of the object matrix then uploads it into OpenGL.
  subroutine camera_set_object_matrix_f64(position_x, position_y, position_z, rotation_x, rotation_y, rotation_z, scale_x, scale_y, scale_z)
    use :: math_helpers, only: into_f32
    use :: shader
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

    call gl_uniform_mat4f(UNIFORM_OBJECT_MATRIX, object_matrix)
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


  !! This needs to be removed down the line !!
  subroutine camera_freecam_hackjob()
    use :: mouse
    use :: keyboard
    use :: vector_2d
    implicit none

    type(vec2d) :: mouse_delta

    mouse_delta = mouse_get_delta()

    call camera_rotate(mouse_delta%y, mouse_delta%x, 0.0d0)
  end subroutine camera_freecam_hackjob

end module camera
