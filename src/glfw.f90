module glfw
  use, intrinsic :: iso_c_binding
  implicit none

  private

  ! Module fields.
  ! ** Fields for getting the window pointer out of C and into Fortran.
  ! C side.
  type(c_ptr) :: c_window_pointer
  ! Fortran side.
  ! type(c_ptr), pointer :: window_pointer

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
      integer(c_int) :: width
      integer(c_int) :: height
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
      type(c_ptr) :: char_pointer
    end function internal_glfw_get_error

  end interface

contains

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

  ! So here I'm just kind of using glfw the way I want to use it.

  logical function glfw_create_window(width, height, title) result(success)
    use, intrinsic :: iso_c_binding
    implicit none
    integer(c_int) :: width
    integer(c_int) :: height
    character(kind = c_char) :: title

    c_window_pointer = internal_glfw_create_window(width, height, title, null(), null())

    ! call c_f_pointer(c_window_pointer, window_pointer)

    print*,c_window_pointer
    ! print *,window_pointer
    ! Then we check if the window pointer is null.
    success = c_associated(c_window_pointer)
  end

  subroutine glfw_make_context_current
    implicit none
    call internal_glfw_make_context_current(c_window_pointer)
  end subroutine glfw_make_context_current


end module glfw
