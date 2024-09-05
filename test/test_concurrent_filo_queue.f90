module test_suite_concurrent_filo_queue
  use :: thread_filo_queue
  use :: string
  use, intrinsic :: iso_c_binding
  implicit none


contains


  subroutine test_it_single_threaded()
    implicit none

    type(concurrent_linked_filo_queue) :: queue
    integer(c_int) :: i
    integer(c_int), pointer :: i_pointer
    character(len = :, kind = c_char), pointer :: s_pointer
    type(queue_data), pointer :: testing

    queue = concurrent_linked_filo_queue()

    do i = 1,1000

      allocate(i_pointer)
      i_pointer = i
      call queue%push(queue_data(i_pointer))

      allocate(character(12) :: s_pointer)
      s_pointer = "hi there!"//int_to_string(i)

      call queue%push(queue_data(s_pointer))
    end do

    call queue%destroy()

  end subroutine test_it_single_threaded


end module test_suite_concurrent_filo_queue

program test_concurrent_filo_queue
  use :: test_suite_concurrent_filo_queue
  implicit none

  call test_it_single_threaded()
end program test_concurrent_filo_queue
