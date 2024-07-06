
program main
  use glfw
  use opengl
  use ye
  use, intrinsic ::  iso_c_binding
  implicit none

  real :: color = 0.0


  call glfw_set_error_callback()


  ! Try to create a GLFW context.
  if (glfw_init()) then
    print *,"worked"
  else
    print *,"failed"
    return
  end if


  ! Try to initialize the Window.
  if (glfw_create_window(640,480, "hi")) then
    print *,"GLFW: Window created successfully."
  else
    print *,"GLFW: Failed to create window."
    call glfw_terminate()
    return
  end if

  call glfw_make_context_current()


  do while(.not. glfw_window_should_close())

    call blah(color)

    call gl_clear_color(color, color, color)

    call glfw_get_error()

    call gl_clear_color_buffer()

    call glfw_swap_buffers()

    call glfw_poll_events()

  end do


  call glfw_destroy_window()

  call glfw_terminate()

  call glfw_get_error()
  

end
