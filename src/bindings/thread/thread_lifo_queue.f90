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
  end function constructor_concurrent_linked_filo_queue


  subroutine concurrent_linked_filo_queue_insert(this)
    implicit none

    class(concurrent_linked_filo_queue), intent(inout) :: this
  end subroutine concurrent_linked_filo_queue_insert


  subroutine concurrent_linked_filo_queue_pop(this)
    implicit none

    class(concurrent_linked_filo_queue), intent(inout) :: this
  end subroutine concurrent_linked_filo_queue_pop


end module thread_lifo_queue
