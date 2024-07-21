module test_suite_float
  use float_compare
  use, intrinsic :: iso_c_binding
  use testament
  implicit none

contains

  subroutine f32_test_1
    implicit none

    real(c_float) :: a, b

    a = 0.00000010
    b = 0.00000010

    print*,"test 1"

    call assert_true(f32_is_equal(a,b))

  end subroutine f32_test_1

  subroutine f32_test_2
    implicit none

    real(c_float) :: a, b

    a = 0.00000011
    b = 0.00000010

    print*,"test 2"

    call assert_true(f32_is_equal(a,b))

  end subroutine f32_test_2


  subroutine f32_test_3
    implicit none

    real(c_float) :: a, b

    ! This is the actual amount of precision you have.
    a = 0.000001 ! <- here
    b = 0.0000001

    print*,"test 3"

    call assert_false(f32_is_equal(a,b))

  end subroutine f32_test_3

end module test_suite_float


program test_float_compare
  use test_suite_float

  implicit none

  call f32_test_1()

  call f32_test_2()

  call f32_test_3()

end program test_float_compare
