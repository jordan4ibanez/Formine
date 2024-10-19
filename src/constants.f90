module constants
  use, intrinsic :: iso_c_binding
  implicit none


  private


  ! Placeholders to trigger a variable for analysis.
  real(c_float) :: f__32__
  real(c_double) :: f__64__
  integer(c_int) :: int__32__
  integer(c_int64_t) :: int__64__


  !* Integral min.
  !? 32 bits. 4 bytes.
  integer(c_int), public, parameter :: C_INT_MIN = -huge(int__32__)-1
  !? 64 bits. 8 bytes.
  integer(c_int64_t), public, parameter :: C_INT64_MIN = -huge(int__64__)-1

  !* Floating point min.
  !? 32 bits. 4 bytes.
  real(c_float), public, parameter :: C_F32_MIN = -huge(int__32__)-1
  !? 64 bits. 8 bytes.
  real(c_double), public, parameter :: C_F64_MIN = -huge(int__64__)-1


  !* Integral max.
  !? 32 bits. 4 bytes.
  integer(c_int), public, parameter :: C_INT_MAX = huge(int__32__)
  !? 64 bits. 8 bytes.
  integer(c_int64_t), public, parameter :: C_INT64_MAX = huge(int__64__)

  !* Floating point max.
  !? 32 bits. 4 bytes.
  real(c_float), public, parameter :: C_F32_MAX = huge(f__32__)
  !? 64 bits. 8 bytes.
  real(c_double), public, parameter :: C_F64_MAX = huge(f__64__)


  !* Floating point data width.
  !? 32 bits. 4 bytes.
  integer(c_int), public, parameter :: F32_SIZE = sizeof(f__32__)
  !? 64 bits. 8 bytes.
  integer(c_int), public, parameter :: F64_SIZE = sizeof(f__64__)

  !* Integral data width.
  !? 32 bits. 4 bytes.
  integer(c_int), public, parameter :: I32_SIZE = sizeof(int__32__)
  !? 64 bits. 8 bytes.
  integer(c_int), public, parameter :: I64_SIZE = sizeof(int__64__)

  !* Unsigned Integral data width.
  !? 32 bits. 4 bytes.
  integer(c_int), public, parameter :: U32_SIZE = sizeof(int__32__)
  !? 64 bits. 8 bytes.
  integer(c_int), public, parameter :: U64_SIZE = sizeof(int__64__)


  !* Floating point.
  !? 32 bits. 4 bytes.
  real(c_float), public, parameter :: PI_F32 = 3.141592
  !? 64 bits. 8 bytes.
  real(c_double), public, parameter :: PI_F64 = 3.141592653589793d0


  !* Floating point.
  !? 32 bits. 4 bytes.
  real(c_float), public, parameter :: PI_TIMES_2_F32 = PI_F32 * 2.0
  !? 64 bits. 8 bytes.
  real(c_double), public, parameter :: PI_TIMES_2_F64 = PI_F64 * 2.0d0


  !* Floating point.
  !? 32 bits. 4 bytes.
  real(c_float), public, parameter :: PI_OVER_2_F32 = PI_F32 * 0.5
  !? 64 bits. 8 bytes.
  real(c_double), public, parameter :: PI_OVER_2_F64 = PI_F64 * 0.5d0


end module constants
