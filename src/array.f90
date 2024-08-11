module array
  use, intrinsic :: iso_c_binding
  implicit none

  !* This is just a bunch of different array types.
  !* They can be used as scalars.

  type :: generic_array
    class(*), dimension(:), allocatable :: data
  end type generic_array

  type, extends(generic_array) :: int_array
    integer(c_int), dimension(:), allocatable :: data
  end type int_array




end module array
