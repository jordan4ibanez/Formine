module float_compare
  implicit none

contains

  logical function f32_is_equal(a, b) result(equality)
    use, intrinsic :: iso_c_binding
    implicit none

    real(c_float), intent(in), value :: a,b

    equality = abs(a - b) <= epsilon(a)
  end function f32_is_equal


  logical function f64_is_equal(a, b) result(equality)
    use, intrinsic :: iso_c_binding
    implicit none

    real(c_double), intent(in), value :: a,b

    equality = abs(a - b) <= epsilon(a)
  end function f64_is_equal

end module float_compare
