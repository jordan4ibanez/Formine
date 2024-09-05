module test_suite_concurrent_filo_queue
  use :: thread_filo_queue
  use, intrinsic :: iso_c_binding
  implicit none


contains


  subroutine test_it_single_threaded()
    implicit none

    type(concurrent_linked_filo_queue) :: queue
    integer(c_int) :: i
    ! integer(c_int), pointer :: i_pointer
    character(len = :, kind = c_char), pointer :: s_pointer

    queue = concurrent_linked_filo_queue()

    do i = 1,10

      ! allocate(i_pointer)
      ! i_pointer = i
      ! call queue%push(i_pointer)

      ! allocate(character(12) :: s_pointer)
      ! s_pointer = "hi there!"

      ! call queue%push(s_pointer)


    end do
  end subroutine test_it_single_threaded


end module test_suite_concurrent_filo_queue

program test_concurrent_filo_queue
  use :: test_suite_concurrent_filo_queue
  implicit none

  call test_it_single_threaded()
end program test_concurrent_filo_queue
