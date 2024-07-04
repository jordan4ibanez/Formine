module glfw
  use, intrinsic :: iso_c_binding
  implicit none

  private

  type(c_ptr) :: window_pointer

  public :: glfw_init
  public :: glfw_create_window



  interface

    logical(c_bool) function glfw_init() result(success) bind(c, name="glfwInit")
      use, intrinsic :: iso_c_binding
      implicit none
    end function glfw_init

    type(c_ptr) function internal_glfw_create_window(width, height, title, monitor, share) result(new_window_pointer) bind(c, name = "glfwCreateWindow")
      use, intrinsic :: iso_c_binding
      implicit none
      integer(c_int) :: width
      integer(c_int) :: height
      character(kind = c_char) :: title
      type(c_ptr), optional :: monitor
      type(c_ptr), optional :: share

    end function internal_glfw_create_window

  end interface

contains
  logical function glfw_create_window(width, height, title) result(success)
    use, intrinsic :: iso_c_binding
    implicit none
    integer(c_int) :: width
    integer(c_int) :: height
    character(kind = c_char) :: title
    window_pointer = internal_glfw_create_window(width, height, title, null(), null())
    print "('Window address: ', I0)",transfer(window_pointer, 0_c_long)
    ! Then we check if the window pointer is null.
    success = c_associated(window_pointer)
  end



end module glfw
