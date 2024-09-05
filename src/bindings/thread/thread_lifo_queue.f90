module thread_lifo_queue
  use :: thread
  use, intrinsic :: iso_c_binding
  implicit none


  private


  public :: concurrent_linked_filo_queue


  type :: queue_node
    type(queue_node), pointer :: next => null()
    class(*), pointer :: data => null()
  end type queue_node


  type :: concurrent_linked_filo_queue
    type(queue_node), pointer :: head => null()
    type(queue_node), pointer :: tail => null()
    type(mutex_rwlock), pointer :: mutex => null()
    type(c_ptr) :: c_mutex_pointer = c_null_ptr
  contains
    procedure :: insert => concurrent_linked_filo_queue_insert
    procedure :: pop => concurrent_linked_filo_queue_pop
    procedure :: destroy => concurrent_linked_filo_queue_destroy
  end type concurrent_linked_filo_queue


  interface concurrent_linked_filo_queue
    module procedure :: constructor_concurrent_linked_filo_queue
  end interface concurrent_linked_filo_queue

contains


  function constructor_concurrent_linked_filo_queue() result(queue_new)
    implicit none

    type(concurrent_linked_filo_queue) :: queue_new

    queue_new%mutex = thread_create_mutex()
    queue_new%c_mutex_pointer = c_loc(queue_new%mutex)
  end function constructor_concurrent_linked_filo_queue


  subroutine concurrent_linked_filo_queue_insert(this, generic_pointer)
    implicit none

    class(concurrent_linked_filo_queue), intent(inout) :: this
    class(*), pointer :: generic_pointer
    integer(c_int) :: status
    type(queue_node), pointer :: node_new

    status = thread_write_lock(this%c_mutex_pointer)
    !! BEGIN SAFE OPERATION.

    allocate(node_new)
    node_new%data => generic_pointer

    ! If the head is null, this is the new head.
    if (.not. associated(this%head)) then
      this%head => node_new
    end if

    ! If we have a tail, it now points to the new node.
    ! The new node then becomes the tail.
    if (associated(this%tail)) then
      this%tail%next => node_new
      this%tail => node_new
    else
      ! If we do not have a tail, the new node is now the tail.
      this%tail => node_new
    end if

    !! END SAFE OPERATION.
    status = thread_unlock_lock(this%c_mutex_pointer)
  end subroutine concurrent_linked_filo_queue_insert


  function concurrent_linked_filo_queue_pop(this) result(generic_pointer)
    implicit none

    class(concurrent_linked_filo_queue), intent(inout) :: this
    class(*), pointer :: generic_pointer
    integer(c_int) :: status
    type(queue_node), pointer :: next_pointer

    status =  thread_write_lock(this%c_mutex_pointer)
    !! BEGIN SAFE OPERATION.

    generic_pointer => null()

    ! If we have a head, the output will become the head data.
    ! The head will now be shifted forward, and the old head will be cleaned up.
    if (associated(this%head)) then
      next_pointer => this%head%next

      generic_pointer => this%head%data

      deallocate(this%head)

      this%head => next_pointer
    end if

    !! END SAFE OPERATION.
    status = thread_unlock_lock(this%c_mutex_pointer)
  end function concurrent_linked_filo_queue_pop


  subroutine concurrent_linked_filo_queue_destroy(this)
    implicit none

    class(concurrent_linked_filo_queue), intent(inout) :: this
    type(queue_node), pointer :: current, next
    integer(c_int) :: status

    status =  thread_write_lock(this%c_mutex_pointer)
    !! BEGIN SAFE OPERATION.

    if (associated(this%head)) then

      current => this%head

      do
        next => current%next

        deallocate(current%data)
        deallocate(current)

        ! Pointing at nothing.
        if (.not. associated(next)) then
          exit
        end if

        current => next
      end do

    end if

    !! END SAFE OPERATION.
    status = thread_unlock_lock(this%c_mutex_pointer)
  end subroutine concurrent_linked_filo_queue_destroy

end module thread_lifo_queue
