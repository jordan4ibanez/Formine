module float_compare
  use, intrinsic :: iso_fortran_env, only: real32, real64
  implicit none

  private

  !** Notes:
  !** f32 has 6 points of precision comparison.
  !** f64 has [test it]

  public :: r32_is_equal
  public :: r64_is_equal

  real, parameter :: tolerence = epsilon(1.0d0)


contains


  logical function r32_is_equal(a, b) result(equality)
    implicit none

    real(real32), intent(in), value :: a,b

    equality = abs(a - b) <= tolerence
  end function r32_is_equal


  logical function r64_is_equal(a, b) result(equality)
    implicit none

    real(real64), intent(in), value :: a,b

    equality = abs(a - b) <= tolerence
  end function r64_is_equal

end module float_compare
