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


  !* This was translated from JOML. Original name: "cosFromSinInternal"
  real(c_float) function cos_from_sin_f32(sin, angle) result(cosine)
    use :: constants, only: PI_F32, PI_TIMES_2_F32, PI_OVER_2_F32
    implicit none

    real(c_float), intent(in), value :: sin, angle
    real(c_float) :: a, b

    cosine = sqrt(1.0 - (sin * sin))

    a = angle + PI_OVER_2_F32

    b = a - int(a / PI_TIMES_2_F32) * PI_TIMES_2_F32

    if (b < 0.0) then
      b = PI_TIMES_2_F32 + b
    end if

    if (b >= PI_F32) then
      cosine = -cosine
    end if
  end function cos_from_sin_f32


  !* This was translated from JOML. Original name: "cosFromSinInternal"
  real(c_double) function cos_from_sin_f64(sin, angle) result(cosine)
    use :: constants, only: PI_F64, PI_TIMES_2_F64, PI_OVER_2_F64
    implicit none

    real(c_double), intent(in), value :: sin, angle
    real(c_double) :: a, b

    cosine = sqrt(1.0 - (sin * sin))

    a = angle + PI_OVER_2_F64

    b = a - idint(a / PI_TIMES_2_F64) * PI_TIMES_2_F64

    if (b < 0.0) then
      b = PI_TIMES_2_F64 + b
    end if

    if (b >= PI_F64) then
      cosine = -cosine
    end if
  end function cos_from_sin_f64


end module math_helpers
