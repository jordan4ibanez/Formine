module raw_c
  use, intrinsic :: iso_c_binding
  implicit none

  public :: c_free

  interface

    subroutine c_free(ptr) bind(c, name = "free")
      use, intrinsic :: iso_c_binding
      implicit none
      type(c_ptr), value :: ptr
    end subroutine c_free

  end interface
end module raw_c
