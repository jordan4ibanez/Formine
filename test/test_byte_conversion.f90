module test_suite_byte_conversion
  use, intrinsic :: iso_c_binding
  use :: testament
  implicit none

contains

  subroutine test_it()
    implicit none

    integer(1) :: output
    integer(c_int) :: input

    input = 128

    output = int(input, kind = 1)

    ! print*,output
  end subroutine test_it


  subroutine now_test_conversions()
    use :: math_helpers
    implicit none

    integer(c_int), dimension(256) :: i32
    integer(1), dimension(256) :: u8
    integer(c_int), dimension(256) :: conversion_output
    integer(c_int) :: i

    ! This is insane overkill.
    do i = 1,256
      i32(i) = i - 1
    end do

    u8 = int_to_c_uchar_array(i32)

    ! If you want to look at this, be my guest.
    ! print*,u8

    conversion_output = c_uchar_to_int_array(u8)

    do i = 1,256
      call assert_true(i32(i) == conversion_output(i))
    end do
  end subroutine now_test_conversions


end module test_suite_byte_conversion

program test_byte_conversion
  use :: test_suite_byte_conversion
  implicit none

  call test_it()

  call now_test_conversions()
end program test_byte_conversion
