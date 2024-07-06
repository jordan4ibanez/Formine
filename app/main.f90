
program main
  use glfw
  use opengl
  use string
  use ye
  use, intrinsic ::  iso_c_binding
  implicit none

  real :: color = 0.0
  integer :: shader_program_id

  call glfw_set_error_callback()


  ! Try to create a GLFW context.
  if (glfw_init()) then
    print *,"GLFW: Successfully initialized."
  else
    print *,"GLFW: Failed to initialize."
    return
  end if

  ! Try to initialize the Window.
  if (glfw_create_window(640,480, "Fortran Game Engine")) then
    print *,"GLFW: Window created successfully."
  else
    print *,"GLFW: Failed to create window."
    call glfw_terminate()
    return
  end if

  call glfw_make_context_current()

  !! This allows OpenGL debugging.
  call gl_set_debug_message_callback()
  call gl_enable(GL_DEBUG_OUTPUT_SYNCHRONOUS)

  !** BEGIN TESTING SHADER
  shader_program_id = gl_create_program()
  print*,"Shader ID: "//int_to_string(shader_program_id)


  !** END TESTING SHADER

  !! This is debugging for functions!
  if (.false.) then
    do while(.not. glfw_window_should_close())

      call blah(color)

      call gl_clear_color(0.0, color, color)

      call gl_clear_color_buffer()

      call glfw_swap_buffers()

      call glfw_poll_events()

    end do
  end if


  call glfw_destroy_window()

  call glfw_terminate()


end
