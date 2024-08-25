module test_array_things_test_suite
  use :: array, only: array_i32_small_to_large_unique
  use, intrinsic :: iso_c_binding
  implicit none

contains

  subroutine testing()
    implicit none

    integer(c_int), dimension(:), allocatable :: unit_1, unit_2, unit_3

    unit_1 = (/5,2,3,4,5,5,1,2,7,42,5,1,2/)

    unit_1 = array_i32_small_to_large_unique(unit_1)
  end subroutine testing


end module test_array_things_test_suite

program test_array_things
  use :: test_array_things_test_suite
  implicit none

  call testing()

end program test_array_things
