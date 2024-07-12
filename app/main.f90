program main
  use glfw
  use opengl
  use string
  use shader
  use files
  use orient_class

  use, intrinsic ::  iso_c_binding
  implicit none

  real :: color = 0.0
  type(orientation) :: test

  test = new_orientation()

  print"(A)",test%get_string()

  ! This stops the program.
  ! test = 5


  !! BEGIN WARNING: This is only to be used for when developing libraries.
  if (.true.) then
    return
  end if
  !! END WARNING.

  call glfw_set_error_callback()

  ! Try to create a GLFW context.
  if (.not. glfw_init()) then
    return
  end if

  !! Need this flag to have OpenGL debugging available!
  call glfw_window_hint(GLFW_OPENGL_DEBUG_CONTEXT, GL_TRUE)
  call glfw_window_hint(GLFW_CONTEXT_VERSION_MAJOR, 4)
  call glfw_window_hint(GLFW_CONTEXT_VERSION_MINOR, 2)
  call glfw_window_hint(GLFW_OPENGL_FORWARD_COMPAT, GL_TRUE)

  ! Try to initialize the Window.
  if (.not. glfw_create_window(640,480, "Fortran Game Engine")) then
    return
  end if

  call glfw_make_context_current()

  call gl_get_version()

  !! This allows OpenGL debugging.
  call gl_enable(GL_DEBUG_OUTPUT_SYNCHRONOUS)
  call gl_set_debug_message_callback()

  !! This resets the gl_get_error integer back to 0.
  call gl_clear_error_data()

  ! This can fail. We will gracefully exit when it does.
  if (.not. create_shader("main", "./shaders/vertex.vert", "./shaders/fragment.frag")) then
    return
  end if

  !! This is debugging for functions!
  if (.false.) then
    do while(.not. glfw_window_should_close())

      ! call blah(color)

      call gl_clear_color(0.0, color, color)

      call gl_clear_color_buffer()

      call glfw_swap_buffers()

      call glfw_poll_events()

    end do
  end if


  call glfw_destroy_window()

  call glfw_terminate()

end program main
