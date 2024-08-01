module math_helpers
  use, intrinsic :: iso_c_binding, only: c_float, c_double
  implicit none


contains


  !* Cast a double into a float explicitly
  real(c_float) function into_f32(input_f64) result(output_f32)
    implicit none

    real(c_double), intent(in), value :: input_f64

    output_f32 = real(input_f64, kind = c_float)
  end function into_f32


  !* Cast a float into a double explicitly
  real(c_double) function into_f64(input_f32) result(output_f64)
    implicit none

    real(c_float), intent(in), value :: input_f32

    output_f64 = real(input_f32, kind = c_double)
  end function into_f64


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


  !* This was translated from JOML. Original name: "fma"
  real(c_float) function fma_f32(a, b, c) result(res)
    implicit none

    real(c_float), intent(in), value :: a, b, c

    res = (a * b) + c
  end function fma_f32


  !* This is a hyper specific function which allows work on a 4 long array. This is commonly used in JOML from what I can see.
  function fma_f32_array_4(a, b, c) result(res)
    implicit none

    real(c_float), dimension(4), intent(in) :: a, b, c
    real(c_float), dimension(4) :: res

    res = (a * b) + c
  end function fma_f32_array_4


  !* This was translated from JOML. Original name: "fma"
  real(c_double) function fma_f64(a, b, c) result(res)
    implicit none

    real(c_double), intent(in), value :: a, b, c

    res = (a * b) + c
  end function fma_f64


  !* This is a hyper specific function which allows work on a 4 long array. This is commonly used in JOML from what I can see.
  function fma_f64_array_4(a, b, c) result(res)
    implicit none

    real(c_double), dimension(4), intent(in) :: a, b, c
    real(c_double), dimension(4) :: res

    res = (a * b) + c
  end function fma_f64_array_4


  !* This is a lazy way to convert a C uchar into a Fortran integer(c_int).
  function c_uchar_to_int(input) result(output)
    use, intrinsic :: iso_c_binding, only: c_int
    implicit none

    integer(1), intent(in), value :: input
    integer(c_int) :: intermidiate
    integer(c_int) :: output

    intermidiate = input

    ! I could bitshift it, but that would require work and thoughts.
    if (intermidiate < 0) then
      output = 127 + (129 - abs(intermidiate))
    else
      output = intermidiate
    end if
  end function c_uchar_to_int


  !* This is a lazy way to convert a C uchar array into a Fortran integer(c_int) array.
  function c_uchar_to_int_array(input_array) result(output_array)
    use, intrinsic :: iso_c_binding, only: c_int
    implicit none

    integer(1), dimension(:), intent(in) :: input_array
    integer(c_int), dimension(:), allocatable :: output_array
    integer :: array_length
    integer :: i

    array_length = size(input_array)

    allocate(output_array(array_length))

    do i = 1,array_length
      output_array(i) = c_uchar_to_int(input_array(i))
    end do
  end function c_uchar_to_int_array



end module math_helpers
