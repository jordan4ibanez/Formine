module test_suite_float
  use float_compare
  use, intrinsic :: iso_c_binding
  use testament
  implicit none

contains

  subroutine test_1
    implicit none

    real(c_float) :: a, b

    a = 0.00000010
    b = 0.00000010

    call assert_true(f32_is_equal(a,b))

  end subroutine test_1

  subroutine test_2
    implicit none

    real(c_float) :: a, b

    a = 0.00000011
    b = 0.00000010

    call assert_false(f32_is_equal(a,b))

  end subroutine test_2


end module test_suite_float


program test_float_compare
  use test_suite_float

  implicit none

  call test_1()

  call test_2()


end program test_float_compare
