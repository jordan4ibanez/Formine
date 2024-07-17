module test_suite
  use string
  implicit none

  !! Not private.

contains

  subroutine test_1
    implicit none

  end subroutine test_1

end module test_suite


program test_heap_string
  use test_suite
  implicit none

  call test_1()

end program test_heap_string
