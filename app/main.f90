program main
  use glfw
  use, intrinsic ::  iso_c_binding
  implicit none

  ! Try to create a GLFW context.
  if (glfw_init() .eqv. .true.) then
    print *,"worked"
  else
    print *,"failed"
    return
  end if

  ! Try to initialize the Window.
  if (glfw_create_window(400,400, "hi") .eqv. .true.) then
    print *,"Created window successfully."
  else
    print *,"Failed to create window."
    call glfw_terminate()
    return
  end if

  call glfw_make_context_current()




  do while (glfw_window_should_close() .eqv. .false.)

    call glfw_get_error()

    call clear_color_buffer()

    call glfw_swap_buffers()

    call glfw_poll_events()

  end do

  call glfw_destroy_window()


  call glfw_terminate()

end
