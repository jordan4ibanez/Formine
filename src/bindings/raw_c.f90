module raw_c
  use, intrinsic :: iso_c_binding
  implicit none


  public :: c_free
  public :: malloc_trim
  public :: c_strlen
  public :: print_f


  interface


    subroutine c_free(ptr) bind(c, name = "free")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: ptr
    end subroutine c_free


    !! NOTE: This will corrupt the string, COPY FIRST !!
    integer(c_int) function c_strlen(ptr) result(length) bind(c, name = "strlen")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: ptr
    end function c_strlen


    subroutine internal_print_f(input_string) bind(c, name = "printf")
      use, intrinsic :: iso_c_binding
      !! Wrap the function internal to make raw binding.
      implicit none

      ! Value because it is const char *
      character(len = 1, kind = c_char), intent(in) :: input_string
    end subroutine internal_print_f


    subroutine malloc_trim(i) bind(c, name = "malloc_trim")
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: i
    end subroutine malloc_trim


  end interface


contains


  subroutine print_f(input_string)
    !! Use the STD C ISO printf function in fortran.
    implicit none

    character(len = *, kind = c_char), intent(in) :: input_string

    ! Null terminate the string with achar(0).
    call internal_print_f(input_string//achar(0))
  end subroutine print_f


end module raw_c
