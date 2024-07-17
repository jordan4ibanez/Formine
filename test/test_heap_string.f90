module test_suite
  use string
  use testament
  implicit none

  !! Not private.


contains


  subroutine assign_test()
    implicit none

    type(heap_string) :: blah

    blah = "hi this is a test"

    call assert_str_equal(blah%get(), "hi this is a test")
  end subroutine assign_test


  subroutine equality_test()
    implicit none

    
  end subroutine equality_test


end module test_suite


program test_heap_string
  use test_suite
  implicit none

  call assign_test()

  call equality_test()


end program test_heap_string
