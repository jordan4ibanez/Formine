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


    function c_strlen(c_str_ptr) result(length) bind(c, name = "strlen")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: c_str_ptr
      integer(c_size_t) :: length
    end function c_strlen


    subroutine internal_print_f(input_string) bind(c, name = "printf")
      use, intrinsic :: iso_c_binding
      !! Wrap the function internal to make raw binding.
      implicit none

      ! Value because it is const char *
      character(len = 1, kind = c_char), intent(in) :: input_string
    end subroutine internal_print_f


    function malloc_trim(i) result(success) bind(c, name = "malloc_trim")
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: i
      integer(c_int) :: success
    end function malloc_trim


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
