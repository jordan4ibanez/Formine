program main
  use :: glfw
  use :: opengl
  use :: string_f90
  use :: terminal
  use :: shader
  use :: files_f90
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
  use :: chunk_handler
  use :: chunk_update_controller
  use :: mouse
  use :: keyboard
  use :: camera
  use :: thread_handler
  use :: version_info
  use :: raw_c
  use :: world_data
  use, intrinsic ::  iso_c_binding
  implicit none

  real(c_float) :: rotation, gui_scale!, min_x, min_y, max_x, max_y
  type(vec2f) :: text_size
  real(c_float), parameter :: FONT_SIZE = 25.0
  real(c_float) :: floating_font_size, old_floating_font_size

  character(len = :, kind = c_char), allocatable :: position_text_debug
  integer(c_int) :: x, y, new_fps, old_fps, x_vao, y_vao, z_vao, fps_vao
  logical(c_bool) :: launched_thread
  ! character(len = :, kind = c_char), pointer :: test_data
  ! integer(c_int), pointer :: test_data

  x = 0
  y = 0
  x_vao = 0
  y_vao = 0
  z_vao = 0
  fps_vao = 0
  new_fps = 0
  old_fps = -1
  launched_thread = .false.
  rotation = 0.0


  ! !! BEGIN WARNING: This is only to be used for when developing libraries.
  ! if (.true.) then
  !   return
  ! end if
  ! !! END WARNING.


  call glfw_set_error_callback()

  ! Try to create a GLFW context.
  if (.not. glfw_init()) then
    return
  end if

  !! Need this flag to have OpenGL debugging available!
  call glfw_window_hint(GLFW_OPENGL_DEBUG_CONTEXT, GL_TRUE)
  call glfw_window_hint(GLFW_CONTEXT_VERSION_MAJOR, 4)
  call glfw_window_hint(GLFW_CONTEXT_VERSION_MINOR, 1)
  call glfw_window_hint(GLFW_OPENGL_FORWARD_COMPAT, GL_TRUE)
  call glfw_window_hint(GLFW_SCALE_FRAMEBUFFER, GLFW_TRUE)

  ! Try to initialize the Window.
  if (.not. glfw_create_window(1920, 1080, "Formine - "//FORMINE_VERSION_STRING)) then
    return
  end if

  call glfw_make_context_current()

  ! Get portable function pointers.
  call forglad_load_gl(c_funloc(glfw_get_proc_address))

  call glfw_trigger_viewport_update()

  !! SET WINDOW ICON.
  call glfw_set_window_icon("./textures/formine_logo_512.png")
  !! END SET WINDOW ICON.

  call texture_module_initialize()

  call mesh_module_initialize()

  ! Only initialize thread components when we're sure the game starts up.
  call thread_handler_intialization()

  call mouse_initialize()

  call mouse_lock()

  call keyboard_module_initialize()

  call delta_initialize()

  call glfw_set_window_size_callback()

  call glfw_set_content_scale_callback()

  call gl_get_version()

  call glfw_swap_interval(0)

  call chunk_handler_module_initalize()

  !! This allows OpenGL debugging. (But not on Mac OS)
  if (forglad_gpu_supports_gl_debugging()) then
    call gl_enable(GL_DEBUG_OUTPUT_SYNCHRONOUS)
    call gl_set_debug_message_callback()
  end if

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
  call shader_module_initialize()

  call shader_create("main", "./shaders/vertex.vert", "./shaders/fragment.frag")

  call font_create("./fonts/font_forgotten.png")

  !! Draw the loading screen.

  call texture_create("./textures/formine_loading_icon.png")

  call mesh_create_3d_named( &
    "loading_mesh", &
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

  !? This is needed for KDE's glide animation.
  call glfw_swap_buffers()

  call shader_start("main")

  call glfw_poll_events()

  call gl_clear_color_scalar(0.5)

  call gl_clear_color_buffer()

  call gl_clear_depth_buffer()

  call texture_use("formine_loading_icon.png")

  call camera_update_2d()

  gui_scale = (glfw_get_window_gui_scale_f32() * 350.0)

  call camera_set_gui_matrix_f32(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0 * gui_scale, 1.25 * gui_scale, 1.0 * gui_scale)

  call mesh_draw_by_name("loading_mesh")

  call glfw_swap_buffers()

  !! END LOADING SCREEN.


  !* If we cannot initalize the API properly, we give up.
  call api_initialize()


  call world_data_set_world_seed(12345)

  print*,"[BASE GENERATION]: START"
  do x = -8,7
    do y = -8,7
      ! This launches 8 threads.
      call chunk_generator_new_chunk(x,y)
    end do
  end do
  print*,"[BASE GENERATION]: END"

  !! DELETE LOADING SCREEN

  call mesh_delete_by_name("loading_mesh")

  call texture_delete("formine_loading_icon.png")

  !! END DELETE LOADING SCREEN

  rotation = 0.0

  ! call mouse_debug_lock_toggle()

  ! Move the camera back.
  call camera_set_position_f32(15.0, 70.0, 8.0)

  !! This is debugging for functions!
  if (.true.) then
    do while(.not. glfw_window_should_close())

      call delta_tick()

      rotation = rotation + delta_get_f32() * 3.0

      call camera_freecam_hackjob()

      !? DRAW TEST ?!

      call gl_clear_color(135.0 / 255.0, 206.0 / 255.0, 235.0 / 255.0)

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



      ! call mesh_draw("debug_block")

      ! call mesh_delete("debug")



      call chunk_handler_draw_chunks()


      ! !* Move into "2D mode"

      call gl_clear_depth_buffer()


      !! THE DEBUG STARTS HERE ============================================

      call camera_update_2d()

      ! Select font.

      call texture_use("font")

      ! Process first text.

      new_fps = get_fps()

      floating_font_size = FONT_SIZE * glfw_get_window_gui_scale_f32()

      call camera_set_gui_matrix_f32((-glfw_get_window_width_f32() / 2.0) + 4, ((glfw_get_window_height_f32() / 2.0) - floating_font_size) - 4, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0, 1.0)

      if (new_fps /= old_fps .or. floating_font_size /= old_floating_font_size) then
        if (fps_vao /= 0) then
          call mesh_delete(fps_vao)
        end if
        fps_vao = font_generate_text(floating_font_size, "FPS: "//int_to_string(get_fps()), center = .false., size = text_size)

        old_fps = new_fps
        old_floating_font_size = floating_font_size
      end if

      call mesh_draw(fps_vao)

      ! XYZ TEXT.

      if (x_vao /= 0) then
        call mesh_delete(x_vao)
      end if
      position_text_debug = f32_to_string(camera_get_pos_x())
      call camera_set_gui_matrix_f32((-glfw_get_window_width_f32() / 2.0) + 4, ((glfw_get_window_height_f32() / 2.0) - (floating_font_size * 2.5)) - 4, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0, 1.0)
      x_vao = font_generate_text(floating_font_size, "X: "//position_text_debug, size = text_size)
      call mesh_draw(x_vao)


      if (y_vao /= 0) then
        call mesh_delete(y_vao)
      end if
      position_text_debug = f32_to_string(camera_get_pos_y())
      call camera_set_gui_matrix_f32((-glfw_get_window_width_f32() / 2.0) + 4, ((glfw_get_window_height_f32() / 2.0) - (floating_font_size * 4.0)) - 4, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0, 1.0)
      y_vao = font_generate_text(floating_font_size, "Y: "//position_text_debug, size = text_size)
      call mesh_draw(y_vao)

      if (z_vao /= 0) then
        call mesh_delete(z_vao)
      end if
      position_text_debug = f32_to_string(camera_get_pos_z())
      call camera_set_gui_matrix_f32((-glfw_get_window_width_f32() / 2.0) + 4, ((glfw_get_window_height_f32() / 2.0) - (floating_font_size * 5.5)) - 4, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0, 1.0)
      z_vao = font_generate_text(floating_font_size, "Z: "//position_text_debug, size = text_size)
      call mesh_draw(z_vao)

      deallocate(position_text_debug)



      gui_scale = (glfw_get_window_gui_scale_f32() * 350.0)

      call camera_set_gui_matrix_f32(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0 * gui_scale, 1.25 * gui_scale, 1.0 * gui_scale)


      ! Process second text.

      ! call camera_set_object_matrix_f32((-glfw_get_window_width_f32() / 2.0) + 4, ((glfw_get_window_height_f32() / 2.0) - (text_size%y * 2.1)) - 4, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0, 1.0)

      ! call mesh_draw_by_name("hello_fortran")


      !? END DRAW TEST ?!

      call mouse_update()

      call glfw_swap_buffers()

      call glfw_poll_events()

      call thread_handler_run()

    end do
  end if

  call texture_atlas_destroy()

  call font_destroy_database()

  call api_destroy()

  call texture_destroy_database()

  call mesh_destroy_database()

  call shader_destroy_database()

  call glfw_destroy_window()

  call glfw_terminate()

  call print_color(NOTIFICATION, "[jordan4ibanez]: Thank you so much for helping with my project by testing!")

end program main
