module test_suite_byte_conversion
  implicit none

contains

  subroutine test_it()
    implicit none

    integer(1) :: output
    integer(4) :: input

    input = 128

    output = int(input, kind = 1)

    print*,output

  end subroutine test_it

end module test_suite_byte_conversion

program test_byte_conversion
  use :: test_suite_byte_conversion
  implicit none

  call test_it()

end program test_byte_conversion
