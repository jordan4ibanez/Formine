module glfw
  use, intrinsic :: iso_c_binding
  implicit none


  private


  ! C side.


  type(c_ptr) :: window_pointer


  ! Fortran side.


  character(len = :), allocatable :: window_title
  integer(c_int) :: window_width
  integer(c_int) :: window_height


  ! Constants.


  public :: GLFW_CONTEXT_DEBUG
  public :: GLFW_OPENGL_DEBUG_CONTEXT
  public :: GLFW_CONTEXT_VERSION_MAJOR
  public :: GLFW_CONTEXT_VERSION_MINOR
  public :: GLFW_OPENGL_FORWARD_COMPAT

  integer, parameter :: GLFW_CONTEXT_DEBUG = int(z"22007")
  integer, parameter :: GLFW_OPENGL_DEBUG_CONTEXT = int(z"22007")
  integer, parameter :: GLFW_CONTEXT_VERSION_MAJOR = int(z"00022002")
  integer, parameter :: GLFW_CONTEXT_VERSION_MINOR = int(z"00022003")
  integer, parameter :: GLFW_OPENGL_FORWARD_COMPAT = int(z"00022006")


  ! What we want exposed.

  public :: glfw_get_proc_address
  public :: glfw_init
  public :: glfw_terminate
  public :: glfw_create_window
  public :: glfw_make_context_current
  public :: glfw_get_error
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
  public :: glfw_set_key_callback

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


    function internal_glfw_create_window(width, height, title, monitor, share) result(window_pointer_new) bind(c, name = "glfwCreateWindow")
      use, intrinsic :: iso_c_binding
      implicit none
      integer(c_int), intent(in), value :: width, height
      character(len = 1, kind = c_char), intent(in), optional :: title
      type(c_ptr), intent(in), optional :: monitor, share
      type(c_ptr) :: window_pointer_new
    end function internal_glfw_create_window


    subroutine internal_glfw_make_context_current(window_pointer_new) bind(c, name = "glfwMakeContextCurrent")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: window_pointer_new
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
    use :: string
    implicit none

    ! C side.
    type(c_ptr) :: c_string
    ! Fortran side.
    integer(c_int) :: error_result
    character(len = :, kind = c_char), allocatable :: error_result_text

    error_result = internal_glfw_get_error(c_string)

    error_result_text = string_from_c(c_string, 512)

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
    use :: string
    implicit none

    integer(c_int) :: width, height
    character(len = *, kind = c_char) :: title

    window_title = into_c_string(title)

    window_pointer = internal_glfw_create_window(width, height, window_title, null(), null())

    ! Then we check if the window pointer is null.
    success = c_associated(window_pointer)

    ! Finally, output information on this and automatically terminate this if it fails.
    if (success) then
      window_width = width
      window_height = height
      print"(A)","[GLFW]: Window created successfully."
    else
      print"(A)","[GLFW] Error: Failed to create window."
      call glfw_terminate()
    end if
  end function glfw_create_window


  subroutine glfw_make_context_current()
    implicit none

    call internal_glfw_make_context_current(window_pointer)
  end subroutine glfw_make_context_current


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
    use :: string
    implicit none

    integer(c_int), intent(in), value :: i
    type(c_ptr), intent(in), value :: char_pointer
    character(len = :), allocatable :: error_text, error_value_string

    error_text = string_from_c(char_pointer, 512)
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
    use :: string, only: int_to_string
    use :: opengl, only: gl_view_port
    implicit none

    type(c_ptr), intent(in), optional :: window
    integer(c_int), intent(in), value :: width, height

    if (.false.) then
      print*,window
    end if

    print"(A)", "[Window] Resize: ["//int_to_string(width)//", "//int_to_string(height)//"]"
    window_width = width
    window_height = height
    call gl_view_port(0,0, width, height)
  end subroutine size_callback


  subroutine glfw_set_window_size_callback()
    implicit none

    call internal_glfw_set_window_size_callback(window_pointer, c_funloc(size_callback))
  end subroutine glfw_set_window_size_callback


  !* This is totally a glfw function, trust me bro.
  real(c_float) function glfw_get_aspect_ratio() result(ratio)
    implicit none

    ratio = real(window_width, kind = c_float) / real(window_height, kind = c_float)
  end function glfw_get_aspect_ratio


  !* Get the window width.
  integer(c_int) function glfw_get_window_width() result(width)
    implicit none

    width = window_width
  end function glfw_get_window_width


  !* Get the window height.
  integer(c_int) function glfw_get_window_height() result(height)
    implicit none

    height = window_height
  end function glfw_get_window_height


  !* Get the window width floating point.
  real(c_float) function glfw_get_window_width_f32() result(width)
    implicit none

    width = real(window_width, kind = c_float)
  end function glfw_get_window_width_f32


  !* Get the window height floating point.
  real(c_float) function glfw_get_window_height_f32() result(height)
    implicit none

    height = real(window_height, kind = c_float)
  end function glfw_get_window_height_f32


end module glfw
