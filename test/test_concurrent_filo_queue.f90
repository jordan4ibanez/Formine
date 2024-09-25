module test_suite_concurrent_filo_queue
  use :: thread_filo_queue_linked
  use :: thread_types
  use :: string
  use, intrinsic :: iso_c_binding
  implicit none


contains


  subroutine test_it_single_threaded()
    implicit none

    type(concurrent_linked_filo_queue) :: queue
    integer(c_int) :: i, y
    ! integer(c_int), pointer :: i_pointer
    character(len = :, kind = c_char), allocatable :: stringy
    ! type(queue_data), pointer :: testing
    class(*), pointer :: test_generic_pointer_option
    logical :: pop_it

    queue = concurrent_linked_filo_queue()

    print*,"begin test memory leak"

    pop_it = .false.

    do y = 1,10000

      ! This pushes ~4.5 GB into the queue.
      do i = 1,200
        stringy = "hi there!"
        call queue%push(stringy)
      end do

      if (pop_it) then
        do while(queue%pop(test_generic_pointer_option))
          deallocate(test_generic_pointer_option)
        end do
      else
        call queue%destroy()
      end if

      pop_it = .not. pop_it
    end do

    print*, "end test memory leak"
  end subroutine test_it_single_threaded


end module test_suite_concurrent_filo_queue

program test_concurrent_filo_queue
  use :: test_suite_concurrent_filo_queue
  implicit none

  call test_it_single_threaded()
end program test_concurrent_filo_queue
