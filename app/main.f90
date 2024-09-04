program main
  use :: glfw
  use :: opengl
  use :: string
  use :: shader
  use :: files
  use :: mesh
  use :: camera
  use :: delta_time
  use :: texture
  use :: font
  use :: vector_2f
  use :: api
  use :: texture_atlas
  use :: texture_atlas
  use :: fast_noise_lite
  use :: chunk_generator
  use :: mouse
  use :: keyboard
  use :: camera
  use :: chunk_handler
  use :: thread
  use, intrinsic ::  iso_c_binding
  implicit none

  real(c_float) :: rotation !, min_x, min_y, max_x, max_y
  type(vec2f) :: text_size
  real(c_float), parameter :: FONT_SIZE = 25.0
  real(c_float) :: floating_font_size
  integer(c_int) :: fps_new, old_fps, x, y, i
  character(len = :, kind = c_char), allocatable :: position_text_debug
  type(pthread_t), dimension(16) :: test_thread
  character(len = :, kind = c_char), allocatable, target :: test_data

  fps_new = 0
  old_fps = -1


  call thread_initialize()

  do i = 1,1
    test_data = "hi there from Fortran!"//achar(0)

    call thread_create_detached(c_funloc(test_threading_implementation), c_loc(test_data))

    ! call thread_set_name(test_thread(i), "Formine testing")
  end do

  ! print*,"thread name: ["//thread_get_name(test_thread)//"]"

  ! print*, "now we wait for the thread."

  !! was here
  do while(thread_process_detached_thread_queue())
  end do

  call sleep(10)


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

  ! Get portable function pointers.
  call forglad_load_gl(c_funloc(glfw_get_proc_address))

  call mouse_initialize()

  call mouse_lock()

  call keyboard_initialize()

  call delta_initialize()

  call glfw_set_window_size_callback()

  call gl_get_version()

  call glfw_swap_interval(1)

  !! This allows OpenGL debugging.
  call gl_enable(GL_DEBUG_OUTPUT_SYNCHRONOUS)
  call gl_set_debug_message_callback()

  !! This enabled depth testing.
  call gl_depth_mask(.true.)
  call gl_enable(GL_DEPTH_TEST)
  call gl_depth_func(GL_LESS)

  !! This enables backface culling.
  ! call gl_enable(GL_CULL_FACE)

  !! This enables alpha blending.
  call gl_enable(GL_BLEND)
  call gl_blend_equation(GL_FUNC_ADD)
  call gl_blend_func(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)

  !? I don't know what the difference is between gl_blend_func and gl_blend_func_separate so disable this until someone tells me.
  ! call gl_blend_func_separate(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA, GL_ONE, GL_ONE);

  call texture_atlas_initialize()

  !! This resets the gl_get_error integer back to 0.
  call gl_clear_error_data()

  ! Set up all shader components.
  call shader_create("main", "./shaders/vertex.vert", "./shaders/fragment.frag")

  call font_create("./fonts/font_forgotten.png")

  call texture_create("./textures/fortran_logo_512x512.png")

  !* If we cannot initalize the API properly, we give up.
  call api_initialize()


  print*,"START"
  do x = -5,5
    do y = -5,5
      call chunk_generator_new_chunk(x,y)
    end do
  end do
  print*,"END"

  rotation = 0.0

  ! Move the camera back.
  ! call camera_set_position_f32(0.0, 0.0, -3.0)

  !! This is debugging for functions!
  if (.true.) then
    do while(.not. glfw_window_should_close())

      call delta_tick()

      rotation = rotation + delta_get_f32() * 3.0

      call camera_freecam_hackjob()


      !? DRAW TEST ?!

      call gl_clear_color_scalar(1.0)

      call gl_clear_color_buffer()


      !* Shader needs to start before the camera is updated.
      call shader_start("main")

      call camera_update_3d()

      call gl_clear_depth_buffer()



      ! call camera_set_object_matrix_f32(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 7.0, 7.0, 7.0)

      ! tex_rect = texture_atlas_debug()


      ! min_x = tex_rect%min_x
      ! min_y = tex_rect%min_y
      ! max_x = tex_rect%max_x
      ! max_y = tex_rect%max_y


      ! call mesh_create_3d( &
      !   "debug", &
      !   [ &
      !   -0.5,  0.5, 0.0, &
      !   -0.5, -0.5, 0.0, &
      !   0.5,  -0.5, 0.0, &
      !   0.5,   0.5, 0.0 &
      !   ], &
      !   [ &
      !   min_x, min_y, &
      !   min_x, max_y, &
      !   max_x, max_y, &
      !   max_x, min_y &
      !   ], &
      !   [ &
      !   1.0, 1.0, 1.0, &
      !   1.0, 1.0, 1.0, &
      !   1.0, 1.0, 1.0, &
      !   1.0, 1.0, 1.0 &
      !   ], &
      !   [0,1,2, 2,3,0] &
      !   )

      ! call texture_use("TEXTURE_ATLAS")

      ! call mesh_draw("debug_block")

      ! call mesh_delete("debug")

      call chunk_handler_draw_chunks()


      !* Move into "2D mode"

      call gl_clear_depth_buffer()

      ! call gl_clear_color_buffer()

      call camera_update_2d()

      ! Select font.

      call texture_use("font")

      ! Process first text.

      fps_new = get_fps()

      floating_font_size = FONT_SIZE * glfw_get_window_gui_scale_f32()

      call camera_set_gui_matrix_f32((-glfw_get_window_width_f32() / 2.0) + 4, ((glfw_get_window_height_f32() / 2.0) - floating_font_size) - 4, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0, 1.0)
      if (fps_new /= old_fps) then
        call mesh_delete("fps_counter")
        call font_generate_text("fps_counter", floating_font_size, "FPS: "//int_to_string(get_fps()), center = .false., size = text_size)
      end if

      call mesh_draw("fps_counter")

      ! XYZ TEXT.

      call mesh_delete("x_pos")
      position_text_debug = f32_to_string(camera_get_pos_x())
      call camera_set_gui_matrix_f32((-glfw_get_window_width_f32() / 2.0) + 4, ((glfw_get_window_height_f32() / 2.0) - (floating_font_size * 2.5)) - 4, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0, 1.0)
      call font_generate_text("x_pos", floating_font_size, "X: "//position_text_debug, size = text_size)
      call mesh_draw("x_pos")

      call mesh_delete("y_pos")
      position_text_debug = f32_to_string(camera_get_pos_y())
      call camera_set_gui_matrix_f32((-glfw_get_window_width_f32() / 2.0) + 4, ((glfw_get_window_height_f32() / 2.0) - (floating_font_size * 4.0)) - 4, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0, 1.0)
      call font_generate_text("y_pos", floating_font_size, "Y: "//position_text_debug, size = text_size)
      call mesh_draw("y_pos")

      call mesh_delete("z_pos")
      position_text_debug = f32_to_string(camera_get_pos_z())
      call camera_set_gui_matrix_f32((-glfw_get_window_width_f32() / 2.0) + 4, ((glfw_get_window_height_f32() / 2.0) - (floating_font_size * 5.5)) - 4, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0, 1.0)
      call font_generate_text("z_pos", floating_font_size, "Z: "//position_text_debug, size = text_size)
      call mesh_draw("z_pos")


      ! Process second text.

      ! call camera_set_object_matrix_f32((-glfw_get_window_width_f32() / 2.0) + 4, ((glfw_get_window_height_f32() / 2.0) - (text_size%y * 2.1)) - 4, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0, 1.0)

      ! call mesh_draw("hello_fortran")


      !? END DRAW TEST ?!

      call mouse_update()

      call glfw_swap_buffers()

      call glfw_poll_events()

    end do
  end if

  call texture_atlas_destroy()

  call font_clear_database()

  call api_destroy()

  call texture_clear_database()

  call mesh_clear_database()

  call shader_clear_database()

  call glfw_destroy_window()

  call glfw_terminate()

end program main
