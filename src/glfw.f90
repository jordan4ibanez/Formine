module glfw
  use, intrinsic :: iso_c_binding
  implicit none

  private

  public :: glfw_init



  interface

    logical(c_bool) function glfw_init() result(success) bind(C, name="glfwInit")
      use, intrinsic :: iso_c_binding
      implicit none
    end function glfw_init

  end interface




end module glfw
