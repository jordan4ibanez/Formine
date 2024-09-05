module test_suite_concurrent_filo_queue
  use :: thread_filo_queue
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
    class(*), pointer :: test_generic_pointer

    queue = concurrent_linked_filo_queue()

    print*,"begin test memory leak"

    y = 1

    do

      ! print*,"iteration: ",y

      y = y + 1

      do i = 1,10

        ! allocate(i_pointer)
        ! i_pointer = i
        ! call queue%push(queue_data(i_pointer))

        stringy = "hi there!"

        call queue%push(queue_data(stringy))
      end do

      i = 1
      do
        ! print*,"pop ", i

        i = i + 1

        test_generic_pointer => queue%pop()

        if (.not. associated(test_generic_pointer)) then
          exit
        end if

        deallocate(test_generic_pointer)

      end do

      ! call queue%destroy()

    end do



  end subroutine test_it_single_threaded


end module test_suite_concurrent_filo_queue

program test_concurrent_filo_queue
  use :: test_suite_concurrent_filo_queue
  implicit none

  call test_it_single_threaded()
end program test_concurrent_filo_queue
