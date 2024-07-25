module constants
  use, intrinsic :: iso_c_binding
  implicit none

  !** Floating point.
  ! 32 bits. 4 bytes.
  integer(kind = c_int), parameter :: f32_size = sizeof(c_float)
  ! 64 bits. 8 bytes.
  integer(kind = c_int), parameter :: f64_size = sizeof(c_double)

  !** Integral
  ! 32 bits. 4 bytes.
  integer(kind = c_int), parameter :: i32_size = sizeof(c_int32_t)
  ! 64 bits. 8 bytes.
  integer(kind = c_int), parameter :: i64_size = sizeof(c_int64_t)

  !** Unsigned Integral
  ! 32 bits. 4 bytes.
  integer(kind = c_int), parameter :: u32_size = sizeof(c_int32_t)
  ! 64 bits. 8 bytes.
  integer(kind = c_int), parameter :: u64_size = sizeof(c_int64_t)

  !** Floating point.
  ! 32 bits. 4 bytes.
  real(c_float), parameter :: pi_32 = 3.141592
  ! 64 bits. 8 bytes.
  real(c_double), parameter :: pi_64 = 3.141592653589793

end module constants
