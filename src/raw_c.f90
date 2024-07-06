module raw_c
  use, intrinsic :: iso_c_binding
  implicit none

  public :: c_free
  public :: c_strlen

  interface

    subroutine c_free(ptr) bind(c, name = "free")
      use, intrinsic :: iso_c_binding
      implicit none
      type(c_ptr), intent(in), value :: ptr
    end subroutine c_free

    !! NOTE: This will corrupt the string !!
    integer(c_int) function c_strlen(ptr) result(length) bind(c, name = "strlen")
      use, intrinsic :: iso_c_binding
      implicit none
      type(c_ptr), intent(in), value :: ptr
    end function c_strlen

  end interface
end module raw_c
