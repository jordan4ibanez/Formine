module float_compare
  use, intrinsic :: iso_c_binding, only: c_float, c_double
  implicit none

  private

  !* Notes:
  !* f32 has 6 points of precision comparison.
  !* f64 has [test it]

  public :: f32_is_equal
  public :: f64_is_equal

  real(c_float), parameter :: tolerence32 = epsilon(0.0)
  real(c_double), parameter :: tolerence64 = epsilon(0.0d0)


contains


  logical function f32_is_equal(a, b) result(equality)
    implicit none

    real(c_float), intent(in), value :: a,b

    equality = abs(a - b) <= tolerence32
  end function f32_is_equal


  logical function f64_is_equal(a, b) result(equality)
    implicit none

    real(c_double), intent(in), value :: a,b

    equality = abs(a - b) <= tolerence64
  end function f64_is_equal

end module float_compare
