program main
  use glfw
  use opengl
  use string
  use shader
  use files
  use, intrinsic ::  iso_c_binding
  implicit none

  real :: color = 0.0

  integer :: program_id
  integer :: vertex_shader_id
  integer :: fragment_shader_id

  !! BEGIN WARNING: This is only to be used for when developing libraries.
  ! if (.true.) then
  !   return
  ! end if
  !! END WARNING.

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
  call glfw_window_hint(GLFW_CONTEXT_VERSION_MAJOR, 4)
  call glfw_window_hint(GLFW_CONTEXT_VERSION_MINOR, 2)
  call glfw_window_hint(GLFW_OPENGL_FORWARD_COMPAT, GL_TRUE)

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

  !? Note: needs a negative check.

  program_id = gl_create_program()
  print*,"Shader Program ID: "//int_to_string(program_id)

  ! Vertex shader
  vertex_shader_id = gl_create_shader(GL_VERTEX_SHADER)
  print*,"Vertex Shader ID: "//int_to_string(vertex_shader_id)
  call gl_shader_source(vertex_shader_id, "./shaders/vertex.vert")
  call gl_compile_shader(vertex_shader_id)

  ! Fragment shader
  fragment_shader_id = gl_create_shader(GL_FRAGMENT_SHADER)
  print*,"Fragment Shader ID: "//int_to_string(fragment_shader_id)
  call gl_shader_source(fragment_shader_id, "./shaders/fragment.frag")
  call gl_compile_shader(fragment_shader_id)

  ! Now we attach and link.
  call gl_attach_shader(program_id, vertex_shader_id)
  call gl_attach_shader(program_id, fragment_shader_id)
  call gl_link_program(program_id)

  !** END TESTING SHADER

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

end
