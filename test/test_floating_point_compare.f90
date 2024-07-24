module test_suite_float
  use float_compare
  use, intrinsic :: iso_fortran_env, only: real32, real64
  use testament
  implicit none

contains

  subroutine f32_test_1
    implicit none

    real(real32) :: a, b

    ! This is the actual amount of precision you have.
    ! [0.000_000] 6 points of precision. (6.92 according to IBM)
    a = 0.00000010
    b = 0.00000010

    call assert_true(r32_is_equal(a,b))

  end subroutine f32_test_1

  subroutine f32_test_2
    implicit none

    real(real32) :: a, b

    ! This is the actual amount of precision you have.
    ! [0.000_000] 6 points of precision. (6.92 according to IBM)
    a = 0.00000011
    b = 0.00000010

    call assert_true(r32_is_equal(a,b))

  end subroutine f32_test_2


  subroutine f32_test_3
    implicit none

    real(real32) :: a, b

    ! This is the actual amount of precision you have.
    ! [0.000_000] 6 points of precision. (6.92 according to IBM)
    a = 0.000001
    b = 0.000002

    call assert_false(r32_is_equal(a,b))

  end subroutine f32_test_3

end module test_suite_float


program test_float_compare
  use test_suite_float

  implicit none

  call f32_test_1()

  call f32_test_2()

  call f32_test_3()

end program test_float_compare
