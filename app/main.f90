program main
  use glfw
  use opengl
  use string
  use shader
  use files
  use mesh
  use, intrinsic ::  iso_c_binding
  implicit none


  !! BEGIN WARNING: This is only to be used for when developing libraries.
  ! if (.true.) then
  !   return
  ! end if
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


  ! Set up all shader components.
  call shader_create("main", "./shaders/vertex.vert", "./shaders/fragment.frag")

  call shader_create_attribute_locations("main", heap_string_array("position", "color"))

  ! call shader_create_uniform_locations("main", heap_string_array("camera_matrix","object_matrix"))

  call gl_use_program(1)

  call create_mesh()

  !! This is debugging for functions!
  if (.true.) then
    do while(.not. glfw_window_should_close())

      ! call blah(color)

      call gl_clear_color(1.0, 1.0, 1.0)

      call gl_clear_color_buffer()

      !? DRAW TEST ?!

      call gl_bind_vertex_array(1)

      call gl_draw_elements(GL_TRIANGLES, 3, GL_UNSIGNED_INT)

      call gl_bind_vertex_array(0)

      !? END DRAW TEST ?!


      call glfw_swap_buffers()

      call glfw_poll_events()

    end do
  end if


  call glfw_destroy_window()

  ! GLFW shared library will rarely crash on termination if you run make too fast. :D
  call glfw_terminate()

end program main
