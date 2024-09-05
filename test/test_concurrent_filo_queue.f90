module test_suite_concurrent_filo_queue
  use :: thread_lifo_queue
  implicit none


contains


  subroutine test_it()
    implicit none

  end subroutine test_it


end module test_suite_concurrent_filo_queue

program test_concurrent_filo_queue
  use :: test_suite_concurrent_filo_queue
  implicit none

  call test_it()
end program test_concurrent_filo_queue
