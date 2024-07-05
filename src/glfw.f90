module glfw
  use, intrinsic :: iso_c_binding
  implicit none

  private

  ! Module fields.

  ! C side.
  type(c_ptr) :: c_window_pointer

  !** Fields for getting the error string out of C and into Fortran.
  ! C side.
  type(c_ptr) :: c_string
  ! Fortran side.
  character, pointer :: error_result_text(:)
  integer(c_int) :: error_result

  ! What we want exposed.

  public :: glfw_init
  public :: glfw_terminate
  public :: glfw_create_window
  public :: glfw_make_context_current
  public :: glfw_get_error
  public :: glfw_window_should_close
  public :: glfw_swap_buffers
  public :: glfw_poll_events
  public :: glfw_destroy_window

  ! Here I'm binding to the C glfw shared library.
  interface
    logical(c_bool) function glfw_init() result(success) bind(c, name="glfwInit")
      use, intrinsic :: iso_c_binding
      implicit none
    end function glfw_init

    subroutine glfw_terminate() bind(c, name="glfwTerminate")
      use, intrinsic :: iso_c_binding
      implicit none
    end subroutine glfw_terminate

    function internal_glfw_create_window(width, height, title, monitor, share) result(new_window_pointer) bind(c, name = "glfwCreateWindow")
      use, intrinsic :: iso_c_binding
      implicit none
      integer(c_int), intent(in) :: width
      integer(c_int), intent(in) :: height
      character(kind = c_char) :: title
      type(c_ptr), intent(in), optional :: monitor
      type(c_ptr), intent(in), optional :: share
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

  end interface

contains

  ! So here I'm just kind of using glfw the way I want to use it.

  subroutine glfw_get_error
    use, intrinsic :: iso_c_binding
    implicit none
    error_result = internal_glfw_get_error(c_string)

    call c_f_pointer(c_string, error_result_text, [512])

    if (associated(error_result_text)) then
      print*,error_result_text
    else if (error_result == 0) then
      print*,"no glfw error :)"
    else
      print*,error_result
    end if

  end subroutine glfw_get_error

  logical function glfw_create_window(width, height, title) result(success)
    use, intrinsic :: iso_c_binding
    implicit none
    integer(c_int) :: width
    integer(c_int) :: height
    character(kind = c_char) :: title

    c_window_pointer = internal_glfw_create_window(width, height, title, null(), null())

    print*,c_window_pointer

    ! Then we check if the window pointer is null.
    success = c_associated(c_window_pointer)
  end function glfw_create_window

  subroutine glfw_make_context_current
    implicit none
    call internal_glfw_make_context_current(c_window_pointer)
  end subroutine glfw_make_context_current

  logical function glfw_window_should_close() result(should_close)
    implicit none
    should_close = internal_glfw_window_should_close(c_window_pointer) .eqv. .true.
  end function glfw_window_should_close


  subroutine glfw_swap_buffers
    implicit none
    call internal_glfw_swap_buffers(c_window_pointer)
  end subroutine glfw_swap_buffers

  subroutine glfw_poll_events
    implicit none
    call internal_glfw_poll_events(c_window_pointer)
  end

  subroutine glfw_destroy_window
    implicit none
    call internal_glfw_destroy_window(c_window_pointer)
  end subroutine glfw_destroy_window

end module glfw
