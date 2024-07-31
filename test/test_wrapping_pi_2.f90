module test_wrapping_pi_suite
  use, intrinsic :: iso_c_binding, only: c_double
  implicit none

contains

  subroutine begin_test
    use :: constants, only: PI_F64, PI_TIMES_2_F64
    implicit none

    real(c_double) :: unit

    unit = PI_F64 * 3d0

    print"(f0.15)", unit

    unit = mod(unit, PI_TIMES_2_F64)

    print"(f0.15)", unit - PI_F64

  end subroutine begin_test

end module test_wrapping_pi_suite

program test_wrapping_pi
  use :: test_wrapping_pi_suite
  implicit none

  call begin_test()
end program test_wrapping_pi
