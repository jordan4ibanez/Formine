module thread_lifo_queue
  use :: thread
  use, intrinsic :: iso_c_binding
  implicit none


  private


  type :: queue_node

  end type queue_node


  type :: concurrent_linked_filo_queue
    type(queue_node), pointer :: head => null()
    type(queue_node), pointer :: tail => null()
    type(mutex_rwlock), pointer :: mutex => null()
    type(c_ptr) :: c_mutex_pointer = c_null_ptr
  contains
    procedure :: insert => concurrent_linked_filo_queue_insert
    procedure :: pop => concurrent_linked_filo_queue_pop
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

    call thread_write_lock(this%c_mutex_pointer)
    !? BEGIN SAFE OPERATION.




    !? END SAFE OPERATION.
    call thread_unlock_lock(this%c_mutex_pointer)
  end subroutine concurrent_linked_filo_queue_insert


  function concurrent_linked_filo_queue_pop(this) result(generic_pointer)
    implicit none

    class(concurrent_linked_filo_queue), intent(inout) :: this
    class(*), pointer :: generic_pointer

    generic_pointer => null()

    call thread_write_lock(this%c_mutex_pointer)
    !? BEGIN SAFE OPERATION.




    !? END SAFE OPERATION.
    call thread_unlock_lock(this%c_mutex_pointer)
  end function concurrent_linked_filo_queue_pop


end module thread_lifo_queue
