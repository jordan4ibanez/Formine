
program main
  use glfw
  use opengl
  use string
  use ye
  use, intrinsic ::  iso_c_binding
  implicit none

  real :: color = 0.0
  integer :: shader_program_id
  integer :: shader_id

  call glfw_set_error_callback()


  ! Try to create a GLFW context.
  if (glfw_init()) then
    print *,"[GLFW]: Successfully initialized."
  else
    print *,"[GLFW] Error: Failed to initialize."
    return
  end if

  !! Need this flag to have OpenGL debugging available!
  call glfw_window_hint(GLFW_OPENGL_DEBUG_CONTEXT, GL_TRUE)

  ! Try to initialize the Window.
  if (glfw_create_window(640,480, "Fortran Game Engine")) then
    print *,"[GLFW]: Window created successfully."
  else
    print *,"[GLFW] Error: Failed to create window."
    call glfw_terminate()
    return
  end if

  call glfw_make_context_current()

  call gl_get_version()

  !! This allows OpenGL debugging.
  call gl_enable(GL_DEBUG_OUTPUT_SYNCHRONOUS)
  call gl_set_debug_message_callback()


  !** BEGIN TESTING SHADER
  !! OpenGL is a state machine :D

  shader_program_id = gl_create_program()
  print*,"Shader Program ID: "//int_to_string(shader_program_id)

  shader_id = gl_create_shader(GL_VERTEX_SHADER)
  print*,"Shader ID: "//int_to_string(shader_id)

  call gl_shader_source(shader_id, " ")

  call gl_compile_shader(1)

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
