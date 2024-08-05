program main
  use :: glfw
  use :: opengl
  use :: string
  use :: shader
  use :: files
  use :: mesh
  use :: camera
  use :: delta
  use :: texture
  use :: font
  use, intrinsic ::  iso_c_binding
  implicit none

  real(c_float) :: rotation

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

  call delta_initialize()

  call glfw_set_window_size_callback()

  call glfw_make_context_current()

  call gl_get_version()

  call glfw_swap_interval(0)

  !! This allows OpenGL debugging.
  call gl_enable(GL_DEBUG_OUTPUT_SYNCHRONOUS)
  call gl_set_debug_message_callback()

  !! This enabled depth testing.
  call gl_depth_mask(.true.)
  call gl_enable(GL_DEPTH_TEST)
  call gl_depth_func(GL_LESS)

  !! This enables backface culling.
  ! call gl_enable(GL_CULL_FACE)

  !! This synchronizes the camera's depth matrix with OpenGL.
  call gl_depth_range_f(camera_get_z_near(), camera_get_z_far())

  !! This enables alpha blending.
  call gl_enable(GL_BLEND)
  call gl_blend_equation(GL_FUNC_ADD)
  call gl_blend_func(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
  !? I don't know what the difference is between gl_blend_func and gl_blend_func_separate so disable this until someone tells me.
  ! call gl_blend_func_separate(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA, GL_ONE, GL_ONE);


  !! This resets the gl_get_error integer back to 0.
  call gl_clear_error_data()


  ! Set up all shader components.
  call shader_create("main", "./shaders/vertex.vert", "./shaders/fragment.frag")

  call shader_create_attribute_locations("main", heap_string_array("position", "texture_coordinate", "color"))

  call shader_create_uniform_locations("main", heap_string_array("camera_matrix","object_matrix"))

  call font_create("./fonts/font_forgotten.png")

  call texture_create("./textures/fortran_logo_512x512.png")
  ! call texture_create("./fonts/font_forgotten.png")

  !! Old texture coordinates:



  rotation = 0.0


  !! This is debugging for functions!
  if (.true.) then
    do while(.not. glfw_window_should_close())

      call delta_tick()

      rotation = rotation + get_delta_f32() * 3.0


      call gl_clear_color_scalar(1.0)

      ! Shader needs to start before the camera is updated.
      call shader_start("main")
      ! call gl_use_program(1)

      call camera_update()

      call gl_clear_color_and_depth_buffer()

      !? DRAW TEST ?!

      call mesh_create_3d( &
        "main", &
        "debug", &
        [ &
        -0.5,  0.5, 0.0, &
        -0.5, -0.5, 0.0, &
        0.5,  -0.5, 0.0, &
        0.5,   0.5, 0.0 &
        ], &
        [ &
        0.0, 0.0, &
        0.0, 1.0, &
        1.0, 1.0, &
        1.0, 0.0 &
        ], &
        [ &
        1.0, 1.0, 1.0, &
        1.0, 1.0, 1.0, &
        1.0, 1.0, 1.0, &
        1.0, 1.0, 1.0 &
        ], &
        [0,1,2, 2,3,0] &
        )

      call camera_set_object_matrix_f32(0.0, 0.0, -5.0, 0.0, 0.0, 0.0, 7.0, 7.0, 7.0)

      call texture_use("fortran_logo_512x512.png")

      call mesh_draw("debug")

      call mesh_delete("debug")

      call camera_set_object_matrix_f32(0.0, -0.25, -3.0, 0.0, rotation, 0.0, 0.5, 0.5, 0.5)

      call texture_use("font")

      call font_generate_text("hello", 1.0, "Debugging is cool!", center = .true.)

      call mesh_draw("hello")

      call mesh_delete("hello")


      !? END DRAW TEST ?!


      call glfw_swap_buffers()

      call glfw_poll_events()

    end do
  end if


  call texture_clear_database()

  call mesh_clear_database()

  call shader_clear_database()

  call glfw_destroy_window()

  ! GLFW shared library will rarely crash on termination if you run make too fast. :D
  call glfw_terminate()

end program main
