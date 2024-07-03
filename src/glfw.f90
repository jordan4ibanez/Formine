module glfw
  use, intrinsic :: iso_c_binding
  implicit none

  private

  public :: load_GLFW
  public :: addnums


  interface
    subroutine addnums(a, b) BIND(C)
      use, intrinsic :: iso_c_binding
      implicit none
      integer(c_int) :: a, b
    end subroutine addnums
  end interface


contains

  subroutine load_GLFW

  end subroutine load_GLFW

end module glfw
