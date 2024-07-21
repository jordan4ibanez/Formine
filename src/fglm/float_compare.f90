module float_compare
  implicit none

  private

  public :: f32_is_equal
  public :: f64_is_equal

  ! 9 points of precision, good enough.
  ! This is stupid anyways.
  integer, parameter :: precision = 1000000000

contains

  logical function f32_is_equal(a, b) result(equality)
    use, intrinsic :: iso_c_binding
    implicit none

    real(c_float), intent(in), value :: a,b

    equality = floor(abs(a - b) * precision) == 0
  end function f32_is_equal


  logical function f64_is_equal(a, b) result(equality)
    use, intrinsic :: iso_c_binding
    implicit none

    real(c_double), intent(in), value :: a,b

    equality = (abs(a - b) * precision) == 0
  end function f64_is_equal

end module float_compare
