module test_suite
  use string
  use testament
  implicit none

  !! Not private.


contains


  subroutine assign_test()
    implicit none

    type(heap_string) :: unit_1

    unit_1 = "hi this is a test"

    call assert_str_equal(unit_1%get(), "hi this is a test")
  end subroutine assign_test


  subroutine equality_test()
    implicit none

    type(heap_string) :: unit_1
    type(heap_string) :: unit_2
    character(len = :), allocatable :: comparitor_1

    unit_1 = "testing"
    unit_2 = "not testing"

    comparitor_1 = "testing"

    call assert_str_equal(unit_1%get(), comparitor_1)
    call assert_str_not_equal(unit_2%get(), comparitor_1)
    call assert_str_not_equal(unit_1%get(), unit_2%get())
  end subroutine equality_test


end module test_suite


program test_heap_string
  use test_suite
  implicit none

  call assign_test()

  call equality_test()


end program test_heap_string
