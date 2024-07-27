module constants
  use, intrinsic :: iso_c_binding
  implicit none


  !* Floating point.
  !? 32 bits. 4 bytes.
  integer(kind = c_int), parameter :: F32_SIZE = sizeof(c_float)
  !? 64 bits. 8 bytes.
  integer(kind = c_int), parameter :: F64_SIZE = sizeof(c_double)


  !* Integral
  !? 32 bits. 4 bytes.
  integer(kind = c_int), parameter :: I32_SIZE = sizeof(c_int32_t)
  !? 64 bits. 8 bytes.
  integer(kind = c_int), parameter :: I64_SIZE = sizeof(c_int64_t)


  !* Unsigned Integral
  !? 32 bits. 4 bytes.
  integer(kind = c_int), parameter :: U32_SIZE = sizeof(c_int32_t)
  !? 64 bits. 8 bytes.
  integer(kind = c_int), parameter :: U64_SIZE = sizeof(c_int64_t)


  !* Floating point.
  !? 32 bits. 4 bytes.
  real(c_float), parameter :: PI_F32 = 3.141592
  !? 64 bits. 8 bytes.
  real(c_double), parameter :: PI_F64 = 3.141592653589793


  !* Floating point.
  !? 32 bits. 4 bytes.
  real(c_float), parameter :: PI_TIMES_2_F32 = PI_F32 * 2.0
  !? 64 bits. 8 bytes.
  real(c_double), parameter :: PI_TIMES_2_F64 = PI_F64 * 2.0d0


  !* Floating point.
  !? 32 bits. 4 bytes.
  real(c_float), parameter :: PI_OVER_2_F32 = PI_F32 * 0.5
  !? 64 bits. 8 bytes.
  real(c_double), parameter :: PI_OVER_2_F64 = PI_F64 * 0.5d0


end module constants
