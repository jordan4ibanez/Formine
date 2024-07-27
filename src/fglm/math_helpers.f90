module math_helpers
  use, intrinsic :: iso_c_binding, only: c_float, c_double
  implicit none


contains


  !* Translate degrees to radians. With 32 bit precision.
  real(c_float) function to_radians_f32(input) result(res)
    use :: constants, only: PI_F32
    implicit none

    real(c_float), intent(in), value :: input

    res = input * (PI_F32 / 180.0)
  end function to_radians_f32


  !* Translate degrees to radians. With 64 bit precision.
  real(c_double) function to_radians_f64(input) result(res)
    use :: constants, only: PI_F64
    implicit none

    real(c_double), intent(in), value :: input

    res = input * (PI_F64 / 180.0d0)
  end function to_radians_f64


end module math_helpers
