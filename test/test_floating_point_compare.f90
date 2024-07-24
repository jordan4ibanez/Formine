module test_suite_float
  use float_compare
  use, intrinsic :: iso_fortran_env, only: real32, real64
  use testament
  implicit none

contains

  !* BEGIN 32 BIT.
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
    a = 0.00000011 ! <- That extra 1 is lost.
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


  !* BEGIN 64 BIT.
  subroutine f64_test_1
    implicit none

    real(real64) :: a, b

    ! This is the actual ammount of precision you have.
    ! 15 points of precision (15.95 according to wikipedia)
    ! 0.000_000_000_000_000
    a = 0.000000000000001
    b = 0.000000000000001

    call assert_true(r64_is_equal(a,b))
  end subroutine f64_test_1

  subroutine f64_test_2
    implicit none

    real(real64) :: a, b

    ! This is the actual ammount of precision you have.
    ! 15 points of precision (15.95 according to wikipedia)
    ! 0.000_000_000_000_000
    a = 0.0000000000000010 
    b = 0.0000000000000011 ! <- that extra 1 is lost.

    call assert_true(r64_is_equal(a,b))
  end subroutine f64_test_2

  subroutine f64_test_3
    implicit none

    real(real64) :: a, b

    ! This is the actual ammount of precision you have.
    ! 15 points of precision (15.95 according to wikipedia)
    ! 0.000_000_000_000_000
    a = 0.000000000000001 
    b = 0.000000000000002

    call assert_false(r64_is_equal(a,b))
  end subroutine f64_test_3

end module test_suite_float


program test_float_compare
  use test_suite_float
  implicit none


  !* 32 bit.
  call f32_test_1()

  call f32_test_2()

  call f32_test_3()

  !* 64 bit.
  call f64_test_1()

  call f64_test_2()

end program test_float_compare
