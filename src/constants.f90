module constants
  use, intrinsic :: iso_c_binding
  implicit none

  ! 32 bits. 4 bytes.
  integer(kind = c_int), parameter :: f32_size = sizeof(c_float)
  ! 64 bits. 8 bytes.
  integer(kind = c_int), parameter :: f64_size = sizeof(c_double)

end module constants
