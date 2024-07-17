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

    ! Preliminary run up.
    call assert_str_equal(unit_1%get(), comparitor_1)
    call assert_str_not_equal(unit_2%get(), comparitor_1)
    call assert_str_not_equal(unit_1%get(), unit_2%get())

    ! Now test the operator.
    call assert_true(unit_1 == comparitor_1)
    call assert_false(unit_2 == comparitor_1)
    call assert_false(unit_1 == unit_2)
  end subroutine equality_test


  subroutine is_allocated_test()
    implicit none

    type(heap_string) :: unit_1

    call assert_false(unit_1%is_allocated())

    unit_1 = "allocation test"

    call assert_true(unit_1%is_allocated())
  end subroutine is_allocated_test


  subroutine get_test()
    implicit none

    type(heap_string) :: unit_1

    call assert_str_equal(unit_1%get(), "")

    unit_1 = "test"

    call assert_str_equal(unit_1%get(), "test")
  end subroutine get_test


  subroutine append_test()
    implicit none

    type(heap_string) :: unit_1

    unit_1 = "hi"

    call unit_1%append(" ")

    call assert_str_equal(unit_1%get(), "hi ")

    call unit_1%append("there")

    call assert_str_equal(unit_1%get(), "hi there")
  end subroutine append_test


  subroutine prepend_test()
    implicit none

    type(heap_string) :: unit_1

    unit_1 = "there"

    call unit_1%prepend(" ")

    call assert_str_equal(unit_1%get(), " there")

    call unit_1%prepend("hi")

    call assert_str_equal(unit_1%get(), "hi there")
  end subroutine prepend_test


  subroutine strip_test()
    implicit none

    type(heap_string) :: unit_1

    unit_1 = "   test   "

    call unit_1%strip()

    call assert_str_equal(unit_1%get(), "test")
  end subroutine strip_test


  subroutine cut_test()
    implicit none

    type(heap_string) :: unit_1

    unit_1 = "hello/there"

    call unit_1%cut("hello")

    call assert_str_equal(unit_1%get(), "test//there")

  end subroutine cut_test


end module test_suite


program test_heap_string
  use test_suite
  implicit none

  call assign_test()

  call equality_test()

  call is_allocated_test()

  call get_test()

  call append_test()

  call prepend_test()

  call strip_test()

  call cut_test()
end program test_heap_string
