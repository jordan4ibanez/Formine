module glfw
  use, intrinsic :: iso_c_binding
  use :: vector_2f
  use :: vector_2i
  implicit none


  private


  ! C side.

  type(c_ptr) :: window_pointer
  type(vec2f) :: window_scale


  ! Fortran side.

  character(len = :), allocatable :: window_title
  type(vec2i) :: window_size

  logical(c_bool) :: framebuffer_scaling_enabled = .false.


  ! GUI components. (For the future)
  ! Base on standard HD resolution.

  real(c_double), parameter :: STANDARD_WINDOW_WIDTH = 1920.0d0
  real(c_double), parameter :: STANDARD_WINDOW_HEIGHT = 1080.0d0

  real(c_double) :: window_gui_scale = 1.0d0


  ! What we want exposed.

  public :: glfw_get_proc_address
  public :: glfw_init
  public :: glfw_terminate
  public :: glfw_create_window
  public :: glfw_make_context_current
  public :: glfw_get_error
  public :: glfw_trigger_viewport_update
  public :: glfw_window_should_close
  public :: glfw_swap_buffers
  public :: glfw_poll_events
  public :: glfw_destroy_window
  public :: glfw_set_error_callback
  public :: glfw_window_hint
  public :: glfw_set_window_size_callback
  public :: glfw_get_aspect_ratio
  public :: glfw_swap_interval
  public :: glfw_get_window_width
  public :: glfw_get_window_height
  public :: glfw_get_window_width_f32
  public :: glfw_get_window_height_f32
  public :: glfw_get_window_width_f64
  public :: glfw_get_window_height_f64
  public :: glfw_set_key_callback
  public :: glfw_close_window
  public :: glfw_set_cursor_pos_callback
  public :: glfw_set_input_mode
  public :: glfw_set_cursor_pos
  public :: glfw_get_window_gui_scale_f32
  public :: glfw_get_window_gui_scale_f64
  ! public :: glfw_get_window_scale_width_f32
  ! public :: glfw_get_window_scale_width_f64
  ! public :: glfw_get_window_scale_height_f32
  ! public :: glfw_get_window_scale_height_f64
  public :: glfw_set_window_icon
  public :: glfw_set_content_scale_callback


  public :: glfw_image


  ! Constants.

  integer, parameter, public :: GLFW_CONTEXT_DEBUG = int(z"22007")
  integer, parameter, public :: GLFW_OPENGL_DEBUG_CONTEXT = int(z"22007")
  integer, parameter, public :: GLFW_CONTEXT_VERSION_MAJOR = int(z"00022002")
  integer, parameter, public :: GLFW_CONTEXT_VERSION_MINOR = int(z"00022003")
  integer, parameter, public :: GLFW_OPENGL_FORWARD_COMPAT = int(z"00022006")
  ! This is needed for wayland.
  integer, parameter, public :: GLFW_SCALE_FRAMEBUFFER = int(z"0002200D")

  ! Booleans.

  integer(c_int), parameter, public :: GLFW_TRUE = 1
  integer(c_int), parameter, public :: GLFW_FALSE = 0


  ! Platforms.

  integer, parameter, public :: GLFW_ANY_PLATFORM   = int(z"00060000")
  integer, parameter, public :: GLFW_PLATFORM_WIN32 = int(z"00060001")
  integer, parameter, public :: GLFW_PLATFORM_COCOA = int(z"00060002")
  integer, parameter, public :: GLFW_PLATFORM_WAYLAND = int(z"00060003")
  integer, parameter, public :: GLFW_PLATFORM_X11 = int(z"00060004")
  integer, parameter, public :: GLFW_PLATFORM_NULL = int(z"00060005")


  ! Key actions.

  integer(c_int), parameter, public :: GLFW_RELEASE = 0
  integer(c_int), parameter, public :: GLFW_PRESS = 1
  integer(c_int), parameter, public :: GLFW_REPEAT = 2


  ! Printable keys.

  integer(c_int), parameter, public :: GLFW_KEY_SPACE = 32
  integer(c_int), parameter, public :: GLFW_KEY_APOSTROPHE = 39  ! '
  integer(c_int), parameter, public :: GLFW_KEY_COMMA = 44  ! ,
  integer(c_int), parameter, public :: GLFW_KEY_MINUS = 45  ! -
  integer(c_int), parameter, public :: GLFW_KEY_PERIOD = 46  ! .
  integer(c_int), parameter, public :: GLFW_KEY_SLASH = 47  ! /
  integer(c_int), parameter, public :: GLFW_KEY_0 = 48
  integer(c_int), parameter, public :: GLFW_KEY_1 = 49
  integer(c_int), parameter, public :: GLFW_KEY_2 = 50
  integer(c_int), parameter, public :: GLFW_KEY_3 = 51
  integer(c_int), parameter, public :: GLFW_KEY_4 = 52
  integer(c_int), parameter, public :: GLFW_KEY_5 = 53
  integer(c_int), parameter, public :: GLFW_KEY_6 = 54
  integer(c_int), parameter, public :: GLFW_KEY_7 = 55
  integer(c_int), parameter, public :: GLFW_KEY_8 = 56
  integer(c_int), parameter, public :: GLFW_KEY_9 = 57
  integer(c_int), parameter, public :: GLFW_KEY_SEMICOLON = 59  ! ;
  integer(c_int), parameter, public :: GLFW_KEY_EQUAL = 61  ! =
  integer(c_int), parameter, public :: GLFW_KEY_A = 65
  integer(c_int), parameter, public :: GLFW_KEY_B = 66
  integer(c_int), parameter, public :: GLFW_KEY_C = 67
  integer(c_int), parameter, public :: GLFW_KEY_D = 68
  integer(c_int), parameter, public :: GLFW_KEY_E = 69
  integer(c_int), parameter, public :: GLFW_KEY_F = 70
  integer(c_int), parameter, public :: GLFW_KEY_G = 71
  integer(c_int), parameter, public :: GLFW_KEY_H = 72
  integer(c_int), parameter, public :: GLFW_KEY_I = 73
  integer(c_int), parameter, public :: GLFW_KEY_J = 74
  integer(c_int), parameter, public :: GLFW_KEY_K = 75
  integer(c_int), parameter, public :: GLFW_KEY_L = 76
  integer(c_int), parameter, public :: GLFW_KEY_M = 77
  integer(c_int), parameter, public :: GLFW_KEY_N = 78
  integer(c_int), parameter, public :: GLFW_KEY_O = 79
  integer(c_int), parameter, public :: GLFW_KEY_P = 80
  integer(c_int), parameter, public :: GLFW_KEY_Q = 81
  integer(c_int), parameter, public :: GLFW_KEY_R = 82
  integer(c_int), parameter, public :: GLFW_KEY_S = 83
  integer(c_int), parameter, public :: GLFW_KEY_T = 84
  integer(c_int), parameter, public :: GLFW_KEY_U = 85
  integer(c_int), parameter, public :: GLFW_KEY_V = 86
  integer(c_int), parameter, public :: GLFW_KEY_W = 87
  integer(c_int), parameter, public :: GLFW_KEY_X = 88
  integer(c_int), parameter, public :: GLFW_KEY_Y = 89
  integer(c_int), parameter, public :: GLFW_KEY_Z = 90
  integer(c_int), parameter, public :: GLFW_KEY_LEFT_BRACKET = 91  ! [
  integer(c_int), parameter, public :: GLFW_KEY_BACKSLASH = 92  ! \
  integer(c_int), parameter, public :: GLFW_KEY_RIGHT_BRACKET = 93  ! ]
  integer(c_int), parameter, public :: GLFW_KEY_GRAVE_ACCENT = 96  ! `
  integer(c_int), parameter, public :: GLFW_KEY_WORLD_1 = 161 ! non-US #1
  integer(c_int), parameter, public :: GLFW_KEY_WORLD_2 = 162 ! non-US #2


  ! Function keys.

  integer(c_int), parameter, public :: GLFW_KEY_ESCAPE = 256
  integer(c_int), parameter, public :: GLFW_KEY_ENTER = 257
  integer(c_int), parameter, public :: GLFW_KEY_TAB = 258
  integer(c_int), parameter, public :: GLFW_KEY_BACKSPACE = 259
  integer(c_int), parameter, public :: GLFW_KEY_INSERT = 260
  integer(c_int), parameter, public :: GLFW_KEY_DELETE = 261
  integer(c_int), parameter, public :: GLFW_KEY_RIGHT = 262
  integer(c_int), parameter, public :: GLFW_KEY_LEFT = 263
  integer(c_int), parameter, public :: GLFW_KEY_DOWN = 264
  integer(c_int), parameter, public :: GLFW_KEY_UP = 265
  integer(c_int), parameter, public :: GLFW_KEY_PAGE_UP = 266
  integer(c_int), parameter, public :: GLFW_KEY_PAGE_DOWN = 267
  integer(c_int), parameter, public :: GLFW_KEY_HOME = 268
  integer(c_int), parameter, public :: GLFW_KEY_END = 269
  integer(c_int), parameter, public :: GLFW_KEY_CAPS_LOCK = 280
  integer(c_int), parameter, public :: GLFW_KEY_SCROLL_LOCK = 281
  integer(c_int), parameter, public :: GLFW_KEY_NUM_LOCK = 282
  integer(c_int), parameter, public :: GLFW_KEY_PRINT_SCREEN = 283
  integer(c_int), parameter, public :: GLFW_KEY_PAUSE = 284
  integer(c_int), parameter, public :: GLFW_KEY_F1 = 290
  integer(c_int), parameter, public :: GLFW_KEY_F2 = 291
  integer(c_int), parameter, public :: GLFW_KEY_F3 = 292
  integer(c_int), parameter, public :: GLFW_KEY_F4 = 293
  integer(c_int), parameter, public :: GLFW_KEY_F5 = 294
  integer(c_int), parameter, public :: GLFW_KEY_F6 = 295
  integer(c_int), parameter, public :: GLFW_KEY_F7 = 296
  integer(c_int), parameter, public :: GLFW_KEY_F8 = 297
  integer(c_int), parameter, public :: GLFW_KEY_F9 = 298
  integer(c_int), parameter, public :: GLFW_KEY_F10 = 299
  integer(c_int), parameter, public :: GLFW_KEY_F11 = 300
  integer(c_int), parameter, public :: GLFW_KEY_F12 = 301
  integer(c_int), parameter, public :: GLFW_KEY_F13 = 302
  integer(c_int), parameter, public :: GLFW_KEY_F14 = 303
  integer(c_int), parameter, public :: GLFW_KEY_F15 = 304
  integer(c_int), parameter, public :: GLFW_KEY_F16 = 305
  integer(c_int), parameter, public :: GLFW_KEY_F17 = 306
  integer(c_int), parameter, public :: GLFW_KEY_F18 = 307
  integer(c_int), parameter, public :: GLFW_KEY_F19 = 308
  integer(c_int), parameter, public :: GLFW_KEY_F20 = 309
  integer(c_int), parameter, public :: GLFW_KEY_F21 = 310
  integer(c_int), parameter, public :: GLFW_KEY_F22 = 311
  integer(c_int), parameter, public :: GLFW_KEY_F23 = 312
  integer(c_int), parameter, public :: GLFW_KEY_F24 = 313
  integer(c_int), parameter, public :: GLFW_KEY_F25 = 314
  integer(c_int), parameter, public :: GLFW_KEY_KP_0 = 320
  integer(c_int), parameter, public :: GLFW_KEY_KP_1 = 321
  integer(c_int), parameter, public :: GLFW_KEY_KP_2 = 322
  integer(c_int), parameter, public :: GLFW_KEY_KP_3 = 323
  integer(c_int), parameter, public :: GLFW_KEY_KP_4 = 324
  integer(c_int), parameter, public :: GLFW_KEY_KP_5 = 325
  integer(c_int), parameter, public :: GLFW_KEY_KP_6 = 326
  integer(c_int), parameter, public :: GLFW_KEY_KP_7 = 327
  integer(c_int), parameter, public :: GLFW_KEY_KP_8 = 328
  integer(c_int), parameter, public :: GLFW_KEY_KP_9 = 329
  integer(c_int), parameter, public :: GLFW_KEY_KP_DECIMAL = 330
  integer(c_int), parameter, public :: GLFW_KEY_KP_DIVIDE = 331
  integer(c_int), parameter, public :: GLFW_KEY_KP_MULTIPLY = 332
  integer(c_int), parameter, public :: GLFW_KEY_KP_SUBTRACT = 333
  integer(c_int), parameter, public :: GLFW_KEY_KP_ADD = 334
  integer(c_int), parameter, public :: GLFW_KEY_KP_ENTER = 335
  integer(c_int), parameter, public :: GLFW_KEY_KP_EQUAL = 336
  integer(c_int), parameter, public :: GLFW_KEY_LEFT_SHIFT = 340
  integer(c_int), parameter, public :: GLFW_KEY_LEFT_CONTROL = 341
  integer(c_int), parameter, public :: GLFW_KEY_LEFT_ALT = 342
  integer(c_int), parameter, public :: GLFW_KEY_LEFT_SUPER = 343
  integer(c_int), parameter, public :: GLFW_KEY_RIGHT_SHIFT = 344
  integer(c_int), parameter, public :: GLFW_KEY_RIGHT_CONTROL = 345
  integer(c_int), parameter, public :: GLFW_KEY_RIGHT_ALT = 346
  integer(c_int), parameter, public :: GLFW_KEY_RIGHT_SUPER = 347
  integer(c_int), parameter, public :: GLFW_KEY_MENU = 348

  integer(c_int), parameter, public :: GLFW_NO_API = 0
  integer(c_int), parameter, public :: GLFW_OPENGL_API = int(z"00030001")
  integer(c_int), parameter, public :: GLFW_OPENGL_ES_API = int(z"00030002")

  integer(c_int), parameter, public :: GLFW_NO_ROBUSTNESS = 0
  integer(c_int), parameter, public :: GLFW_NO_RESET_NOTIFICATION = int(z"00031001")
  integer(c_int), parameter, public :: GLFW_LOSE_CONTEXT_ON_RESET = int(z"00031002")

  integer(c_int), parameter, public ::  GLFW_OPENGL_PROFILE = int(z"00022008")

  integer(c_int), parameter, public :: GLFW_OPENGL_ANY_PROFILE = 0
  integer(c_int), parameter, public :: GLFW_OPENGL_CORE_PROFILE = int(z"00032001")
  integer(c_int), parameter, public :: GLFW_OPENGL_COMPAT_PROFILE = int(z"00032002")

  integer(c_int), parameter, public :: GLFW_CURSOR = int(z"00033001")
  integer(c_int), parameter, public :: GLFW_STICKY_KEYS = int(z"00033002")
  integer(c_int), parameter, public :: GLFW_STICKY_MOUSE_BUTTONS = int(z"00033003")
  integer(c_int), parameter, public :: GLFW_LOCK_KEY_MODS = int(z"00033004")
  integer(c_int), parameter, public :: GLFW_RAW_MOUSE_MOTION = int(z"00033005")
  integer(c_int), parameter, public :: GLFW_UNLIMITED_MOUSE_BUTTONS = int(z"00033006")

  integer(c_int), parameter, public :: GLFW_CURSOR_NORMAL = int(z"00034001")
  integer(c_int), parameter, public :: GLFW_CURSOR_HIDDEN = int(z"00034002")
  integer(c_int), parameter, public :: GLFW_CURSOR_DISABLED = int(z"00034003")
  integer(c_int), parameter, public :: GLFW_CURSOR_CAPTURED = int(z"00034004")

  integer(c_int), parameter, public :: GLFW_ANY_RELEASE_BEHAVIOR = 0
  integer(c_int), parameter, public :: GLFW_RELEASE_BEHAVIOR_FLUSH = int(z"00035001")
  integer(c_int), parameter, public :: GLFW_RELEASE_BEHAVIOR_NONE = int(z"00035002")

  integer(c_int), parameter, public :: GLFW_NATIVE_CONTEXT_API = int(z"00036001")
  integer(c_int), parameter, public :: GLFW_EGL_CONTEXT_API = int(z"00036002")
  integer(c_int), parameter, public :: GLFW_OSMESA_CONTEXT_API = int(z"00036003")

  integer(c_int), parameter, public :: GLFW_ANGLE_PLATFORM_TYPE_NONE = int(z"00037001")
  integer(c_int), parameter, public :: GLFW_ANGLE_PLATFORM_TYPE_OPENGL = int(z"00037002")
  integer(c_int), parameter, public :: GLFW_ANGLE_PLATFORM_TYPE_OPENGLES = int(z"00037003")
  integer(c_int), parameter, public :: GLFW_ANGLE_PLATFORM_TYPE_D3D9 = int(z"00037004")
  integer(c_int), parameter, public :: GLFW_ANGLE_PLATFORM_TYPE_D3D11 = int(z"00037005")
  integer(c_int), parameter, public :: GLFW_ANGLE_PLATFORM_TYPE_VULKAN = int(z"00037007")
  integer(c_int), parameter, public :: GLFW_ANGLE_PLATFORM_TYPE_METAL = int(z"00037008")

  integer(c_int), parameter, public :: GLFW_WAYLAND_PREFER_LIBDECOR = int(z"00038001")
  integer(c_int), parameter, public :: GLFW_WAYLAND_DISABLE_LIBDECOR = int(z"00038002")

  integer(c_int), parameter, public :: GLFW_ANY_POSITION = int(z"80000000")

  integer(c_int), parameter, public :: GLFW_ARROW_CURSOR = int(z"00036001")
  integer(c_int), parameter, public :: GLFW_IBEAM_CURSOR = int(z"00036002")
  integer(c_int), parameter, public :: GLFW_CROSSHAIR_CURSOR = int(z"00036003")
  integer(c_int), parameter, public :: GLFW_POINTING_HAND_CURSOR = int(z"00036004")

  integer(c_int), parameter, public :: GLFW_RESIZE_EW_CURSOR = int(z"00036005")
  integer(c_int), parameter, public :: GLFW_RESIZE_NS_CURSOR = int(z"00036006")
  integer(c_int), parameter, public :: GLFW_RESIZE_NWSE_CURSOR = int(z"00036007")
  integer(c_int), parameter, public :: GLFW_RESIZE_NESW_CURSOR = int(z"00036008")
  integer(c_int), parameter, public :: GLFW_RESIZE_ALL_CURSOR = int(z"00036009")
  integer(c_int), parameter, public :: GLFW_NOT_ALLOWED_CURSOR = int(z"0003600A")
  integer(c_int), parameter, public :: GLFW_HRESIZE_CURSOR = GLFW_RESIZE_EW_CURSOR
  integer(c_int), parameter, public :: GLFW_VRESIZE_CURSOR = GLFW_RESIZE_NS_CURSOR
  integer(c_int), parameter, public :: GLFW_HAND_CURSOR = GLFW_POINTING_HAND_CURSOR

  integer(c_int), parameter, public :: GLFW_CONNECTED = int(z"00040001")
  integer(c_int), parameter, public ::  GLFW_DISCONNECTED = int(z"00040002")

  ! This is so you can set the window icon. :)
  type, bind(c) :: glfw_image
    integer(c_int) :: width
    integer(c_int) :: height
    ! This is: unsigned char* pixels;
    type(c_ptr) :: pixels = c_null_ptr
  end type glfw_image


  ! Here I'm binding to the C glfw shared library.


  interface


    !! NEVER USE THIS OUTSIDE OF INITIALIZATION !!
    function glfw_get_proc_address(procname) result(address) bind(c, name = "glfwGetProcAddress")
      use, intrinsic :: iso_c_binding
      implicit none

      character(len = 1, kind = c_char), intent(in), optional :: procname
      type(c_funptr) :: address
    end function glfw_get_proc_address


    logical(c_bool) function internal_glfw_init() result(success) bind(c, name="glfwInit")
      use, intrinsic :: iso_c_binding
      implicit none

    end function internal_glfw_init


    subroutine internal_glfw_terminate() bind(c, name="glfwTerminate")
      use, intrinsic :: iso_c_binding
      implicit none

    end subroutine internal_glfw_terminate


    function internal_glfw_create_window(width, height, title, monitor, share) result(new_window_pointer) bind(c, name = "glfwCreateWindow")
      use, intrinsic :: iso_c_binding
      implicit none
      integer(c_int), intent(in), value :: width, height
      character(len = 1, kind = c_char), intent(in), optional :: title
      type(c_ptr), intent(in), optional :: monitor, share
      type(c_ptr) :: new_window_pointer
    end function internal_glfw_create_window


    subroutine internal_glfw_make_context_current(new_window_pointer) bind(c, name = "glfwMakeContextCurrent")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: new_window_pointer
    end subroutine internal_glfw_make_context_current


    integer(c_int) function internal_glfw_get_error(char_pointer) result(error_type) bind(c, name = "glfwGetError")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in) :: char_pointer
    end function internal_glfw_get_error


    logical(c_bool) function internal_glfw_window_should_close(current_window_pointer) result(should_close) bind(c, name = "glfwWindowShouldClose")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: current_window_pointer
    end function internal_glfw_window_should_close


    subroutine internal_glfw_swap_buffers(current_window_pointer) bind(c, name = "glfwSwapBuffers")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: current_window_pointer
    end subroutine internal_glfw_swap_buffers


    subroutine internal_glfw_poll_events(current_window_pointer) bind(c, name = "glfwPollEvents")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: current_window_pointer
    end subroutine internal_glfw_poll_events


    subroutine internal_glfw_destroy_window(current_window_pointer) bind(c, name = "glfwDestroyWindow")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: current_window_pointer
    end subroutine internal_glfw_destroy_window


    subroutine internal_glfw_set_error_callback(func) bind(c, name = "glfwSetErrorCallback")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_funptr), intent(in), value :: func
    end subroutine internal_glfw_set_error_callback


    subroutine glfw_window_hint(hint, value) bind(c, name = "glfwWindowHint")
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: hint, value
    end subroutine glfw_window_hint


    subroutine internal_glfw_set_window_size_callback(window, callback) bind(c, name = "glfwSetWindowSizeCallback")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: window
      type(c_funptr), intent(in), value :: callback
      !! We will just ignore the pointer return cause I don't really care tbh.
    end subroutine internal_glfw_set_window_size_callback


    subroutine glfw_swap_interval(interval) bind(c, name = "glfwSwapInterval")
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: interval
    end subroutine glfw_swap_interval


    subroutine internal_glfw_set_key_callback(window, callback) bind(c, name = "glfwSetKeyCallback")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: window
      type(c_funptr), intent(in), value :: callback
    end subroutine internal_glfw_set_key_callback


    subroutine internal_glfw_set_window_should_close(window, value) bind(c, name = "glfwSetWindowShouldClose")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: window
      integer(c_int), intent(in), value :: value
    end subroutine internal_glfw_set_window_should_close


    subroutine internal_glfw_set_cursor_pos_callback(window, callback) bind(c, name = "glfwSetCursorPosCallback")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: window
      type(c_funptr), intent(in), value :: callback
      !? This is written as a subroutine because I do not care about the previous callback.
    end subroutine internal_glfw_set_cursor_pos_callback


    subroutine internal_glfw_set_input_mode(window, mode, value) bind(c, name = "glfwSetInputMode")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: window
      integer(c_int), intent(in), value :: mode, value
    end subroutine internal_glfw_set_input_mode


    subroutine internal_glfw_set_cursor_pos(window, xpos, ypos) bind(c, name = "glfwSetCursorPos")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: window
      real(c_double), intent(in), value :: xpos, ypos
    end subroutine internal_glfw_set_cursor_pos


    subroutine internal_glfw_set_window_icon(window, count, images) bind(c, name = "glfwSetWindowIcon")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: window, images
      integer(c_int), intent(in), value :: count
    end subroutine internal_glfw_set_window_icon


    subroutine internal_glfw_set_window_content_scale_callback(window, window_content_scale_callback) bind(c, name = "glfwSetWindowContentScaleCallback")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: window
      type(c_funptr), intent(in), value :: window_content_scale_callback
    end subroutine internal_glfw_set_window_content_scale_callback


    subroutine internal_glfw_get_window_content_scale(window, x_scale, y_scale) bind(c, name = "glfwGetWindowContentScale")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: window
      real(c_float), intent(inout) :: x_scale, y_scale
    end subroutine


    function internal_glfw_platform_supported(plat) result(support) bind(c, name = "glfwPlatformSupported")
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: plat
      logical(c_bool) :: support
    end function internal_glfw_platform_supported


    function internal_glfw_get_platform() result(plat) bind(c, name = "glfwGetPlatform")
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int) :: plat
    end function internal_glfw_get_platform


  end interface


contains

  ! Here I'm just kind of using glfw the way I want to use it.

  logical function glfw_init() result(success)
    implicit none

    success = internal_glfw_init()

    if (success) then
      print"(A)","[GLFW]: Successfully initialized."
    else
      print"(A)","[GLFW] Error: Failed to initialize."
    end if
  end function glfw_init


  subroutine glfw_terminate()
    implicit none

    call internal_glfw_terminate()
    print"(A)","[GLFW]: Successfully terminated."
  end subroutine glfw_terminate


  subroutine glfw_get_error()
    use, intrinsic :: iso_c_binding
    use :: string_f90
    implicit none

    ! C side.
    type(c_ptr) :: c_string
    ! Fortran side.
    integer(c_int) :: error_result
    character(len = :, kind = c_char), pointer :: error_result_text

    error_result = internal_glfw_get_error(c_string)

    error_result_text => string_from_c(c_string)

    if (len(error_result_text) > 0) then
      print"(A)","[GLFW] Gotten Error: "//error_result_text//"."
      ! else if (error_result == 0) then
      !   print"(A)","no glfw error :)"
      ! else
      !   print"(A)",error_result
    end if
    !! Calling c_free() on c_string will just crash here because this is stack memory.
  end subroutine glfw_get_error


  logical function glfw_create_window(width, height, title) result(success)
    use, intrinsic :: iso_c_binding
    use :: string_f90
    implicit none

    integer(c_int) :: width, height
    character(len = *, kind = c_char) :: title

    window_title = into_c_string(title)

    window_pointer = internal_glfw_create_window(width, height, window_title, null(), null())

    ! Then we check if the window pointer is null.
    success = c_associated(window_pointer)

    call internal_glfw_get_window_content_scale(window_pointer, window_scale%x, window_scale%y)

    select case(internal_glfw_get_platform())
     case (GLFW_PLATFORM_COCOA, GLFW_PLATFORM_WAYLAND)
      print"(A)","[GLFW]: Framebuffer scaling supported. Enabling."
      framebuffer_scaling_enabled = .true.
     case default
      print"(A)","[GLFW]: Framebuffer scaling unsupported. Disabling."
    end select

    ! Finally, output information on this and automatically terminate this if it fails.
    if (success) then
      if (framebuffer_scaling_enabled) then
        window_size%x = nint(real(width) * window_scale%x)
        window_size%y = nint(real(height) * window_scale%y)
      else
        window_size%x = width
        window_size%y = height
      end if
      print"(A)","[GLFW]: Window created successfully."
    else
      print"(A)","[GLFW] Error: Failed to create window."
      call glfw_terminate()
    end if

    call glfw_update_window_gui_scale()
  end function glfw_create_window


  subroutine glfw_make_context_current()
    implicit none

    call internal_glfw_make_context_current(window_pointer)
  end subroutine glfw_make_context_current


  subroutine glfw_trigger_viewport_update()
    use :: opengl, only: gl_view_port
    implicit none

    call gl_view_port(0,0, window_size%x, window_size%y)
  end subroutine glfw_trigger_viewport_update


  logical function glfw_window_should_close() result(should_close)
    implicit none

    should_close = internal_glfw_window_should_close(window_pointer) .eqv. .true.
  end function glfw_window_should_close


  subroutine glfw_swap_buffers()
    implicit none

    call internal_glfw_swap_buffers(window_pointer)
  end subroutine glfw_swap_buffers


  subroutine glfw_poll_events()
    implicit none

    call internal_glfw_poll_events(window_pointer)
  end


  subroutine glfw_destroy_window()
    implicit none

    call internal_glfw_destroy_window(window_pointer)
    print"(A)","[GLFW]: Window destroyed successfully."
  end subroutine glfw_destroy_window


  !* NOTE: C is passing Fortran data here!
  !* NOTE: This function passed into C as a pointer!
  subroutine error_callback(i, char_pointer)
    use, intrinsic :: iso_c_binding
    use :: string_f90
    implicit none

    integer(c_int), intent(in), value :: i
    type(c_ptr), intent(in), value :: char_pointer
    character(len = :), pointer :: error_text
    character(len = :), allocatable :: error_value_string

    error_text => string_from_c(char_pointer)
    error_value_string = int_to_string(i)

    if (len(error_text) > 0) then
      !? We put a period at the end because I think that looks nice.
      print"(A)","[GLFW] Error: ("//error_value_string//") "//error_text//"."
    end if
    !! char_pointer is on the stack. Calling c_free() on it will crash the program.
  end subroutine error_callback


  subroutine glfw_set_error_callback()
    implicit none

    call internal_glfw_set_error_callback(c_funloc(error_callback))
  end subroutine glfw_set_error_callback


  subroutine size_callback(window, width, height)
    use, intrinsic :: iso_c_binding
    use :: string_f90, only: int_to_string
    use :: opengl, only: gl_view_port
    implicit none

    type(c_ptr), intent(in), optional :: window
    integer(c_int), intent(in), value :: width, height

    if (.false.) then
      print*,window
    end if

    print"(A)", "[Window] Resize: ["//int_to_string(width)//", "//int_to_string(height)//"]"

    if (framebuffer_scaling_enabled) then
      window_size%x = nint(real(width) * window_scale%x)
      window_size%y = nint(real(height) * window_scale%y)
    else
      window_size%x = width
      window_size%y = height
    end if

    call gl_view_port(0,0, window_size%x, window_size%y)

    call glfw_update_window_gui_scale()
  end subroutine size_callback


  subroutine glfw_set_window_size_callback()
    implicit none

    call internal_glfw_set_window_size_callback(window_pointer, c_funloc(size_callback))
  end subroutine glfw_set_window_size_callback


  !* This is totally a glfw function, trust me bro.
  real(c_float) function glfw_get_aspect_ratio() result(ratio)
    implicit none

    ratio = real(window_size%x) / real(window_size%y)
  end function glfw_get_aspect_ratio


  !* Get the window width.
  integer(c_int) function glfw_get_window_width() result(width)
    implicit none

    width = window_size%x
  end function glfw_get_window_width


  !* Get the window height.
  integer(c_int) function glfw_get_window_height() result(height)
    implicit none

    height = window_size%y
  end function glfw_get_window_height


  !* Get the window width floating point.
  real(c_float) function glfw_get_window_width_f32() result(width)
    implicit none

    width = real(window_size%x)
  end function glfw_get_window_width_f32


  !* Get the window height floating point.
  real(c_float) function glfw_get_window_height_f32() result(height)
    implicit none

    height = real(window_size%y)
  end function glfw_get_window_height_f32


  !* Get the window width double floating point.
  real(c_double) function glfw_get_window_width_f64() result(width)
    implicit none

    width = real(window_size%x, kind = c_double)
  end function glfw_get_window_width_f64


  !* Get the window height double floating point.
  real(c_double) function glfw_get_window_height_f64() result(height)
    implicit none

    height = real(window_size%y, kind = c_double)
  end function glfw_get_window_height_f64


  !* Set the key callback function pointer.
  subroutine glfw_set_key_callback(callback_function_pointer)
    implicit none

    type(c_funptr), intent(in), value :: callback_function_pointer

    call internal_glfw_set_key_callback(window_pointer, callback_function_pointer)
  end subroutine glfw_set_key_callback


  !* Close the game's window.
  subroutine glfw_close_window()
    implicit none

    call internal_glfw_set_window_should_close(window_pointer, GLFW_TRUE)
  end subroutine glfw_close_window


  !* Set the cursor position callback function pointer.
  subroutine glfw_set_cursor_pos_callback(callback_function_pointer)
    implicit none

    type(c_funptr), intent(in), value :: callback_function_pointer

    call internal_glfw_set_cursor_pos_callback(window_pointer, callback_function_pointer)
  end subroutine glfw_set_cursor_pos_callback


  !* Set an input mode.
  subroutine glfw_set_input_mode(mode, value)
    implicit none

    integer(c_int), intent(in), value :: mode, value

    call internal_glfw_set_input_mode(window_pointer, mode, value)
  end subroutine glfw_set_input_mode


  !* Set the cursor position.
  subroutine glfw_set_cursor_pos(x_pos, y_pos)
    implicit none

    real(c_double), intent(in), value :: x_pos, y_pos

    call internal_glfw_set_cursor_pos(window_pointer, x_pos, y_pos)
  end subroutine glfw_set_cursor_pos


  !* Automatically updates the window GUI scale to match the window size.
  !* This will avoid any GUI element overlap.
  subroutine glfw_update_window_gui_scale()
    implicit none

    real(c_double) :: x, y

    x = window_size%x / STANDARD_WINDOW_WIDTH
    y = window_size%y / STANDARD_WINDOW_HEIGHT

    if (x < y) then
      window_gui_scale = x
    else
      window_gui_scale = y
    end if
  end subroutine glfw_update_window_gui_scale


  !* Get the GUI scale of the window in floating point.
  real(c_float) function glfw_get_window_gui_scale_f32() result(gui_scale)
    implicit none

    gui_scale = real(window_gui_scale, c_float)
  end function glfw_get_window_gui_scale_f32


  !* Get the GUI scale of the window in double floating point.
  real(c_double) function glfw_get_window_gui_scale_f64() result(gui_scale)
    implicit none

    gui_scale = window_gui_scale
  end function glfw_get_window_gui_scale_f64


  !* Get the window content scale width in floating point.
  real(c_float) function glfw_get_window_scale_width_f32() result(w_scale)
    implicit none

    w_scale = window_scale%x
  end function glfw_get_window_scale_width_f32


  !* Get the window content scale width in double floating point.
  real(c_double) function glfw_get_window_scale_width_f64() result(w_scale)
    implicit none

    w_scale = window_scale%x
  end function glfw_get_window_scale_width_f64


  !* Get the window content scale width in floating point.
  real(c_float) function glfw_get_window_scale_height_f32() result(w_scale)
    implicit none

    w_scale = window_scale%y
  end function glfw_get_window_scale_height_f32


  !* Get the window content scale width in double floating point.
  real(c_double) function glfw_get_window_scale_height_f64() result(w_scale)
    implicit none

    w_scale = window_scale%y
  end function glfw_get_window_scale_height_f64

  !* Set the window icon.
  subroutine glfw_set_window_icon(path)
    use :: stb_image
    implicit none

    character(len = *, kind = c_char), intent(in) :: path
    type(glfw_image), dimension(:), pointer :: icon
    integer(c_int) :: channels
    integer(1), dimension(:), allocatable, target :: raw_data

    allocate(icon(1))
    raw_data = stbi_load(path, icon(1)%width, icon(1)%height, channels, 4)
    icon(1)%pixels = c_loc(raw_data)

    call internal_glfw_set_window_icon(window_pointer, 1, c_loc(icon))

    deallocate(icon)
  end subroutine glfw_set_window_icon


  !* Set content scaling awareness for wayland and macos.
  subroutine glfw_set_content_scale_callback()
    implicit none

    call internal_glfw_set_window_content_scale_callback(window_pointer, c_funloc(content_scale_callback))
  end subroutine


  !* And this is the actual callback.
  subroutine content_scale_callback(window, x_scale, y_scale)
    implicit none

    type(c_ptr), intent(in), value :: window
    real(c_float), intent(in), value :: x_scale, y_scale

    if (.false.) then
      print*,window
    end if

    window_scale%x = x_scale
    window_scale%y = y_scale
  end subroutine content_scale_callback


end module glfw
