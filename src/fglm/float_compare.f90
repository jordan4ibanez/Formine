module float_compare
  implicit none

  private

  public :: f32_is_equal
  public :: f64_is_equal

  real, parameter :: tolerence = epsilon(1.0)


contains


  logical function f32_is_equal(a, b) result(equality)
    use, intrinsic :: iso_c_binding
    implicit none

    real(c_float), intent(in), value :: a,b

    equality = abs(a - b) <= tolerence
  end function f32_is_equal


  logical function f64_is_equal(a, b) result(equality)
    use, intrinsic :: iso_c_binding
    implicit none

    real(c_double), intent(in), value :: a,b

    equality = abs(a - b) <= tolerence
  end function f64_is_equal

end module float_compare
