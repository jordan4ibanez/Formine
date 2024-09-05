module thread_filo_queue
  use :: thread
  use, intrinsic :: iso_c_binding
  implicit none


  private


  public :: concurrent_linked_filo_queue


  integer(c_int), parameter :: QUEUE_NONE = 0
  integer(c_int), parameter :: QUEUE_I32 = 1
  integer(c_int), parameter :: QUEUE_I64 = 2
  integer(c_int), parameter :: QUEUE_F32 = 3
  integer(c_int), parameter :: QUEUE_F64 = 4
  integer(c_int), parameter :: QUEUE_BOOL = 5
  integer(c_int), parameter :: QUEUE_STRING = 6
  integer(c_int), parameter :: QUEUE_GENERIC = 7



  type :: queue_data
    !* Basic types.
    integer(c_int), pointer :: i32 => null()
    integer(c_int64_t), pointer :: i64 => null()
    real(c_float), pointer :: f32 => null()
    real(c_double), pointer :: f64 => null()
    logical(c_bool), pointer :: bool => null()
    !* String.
    character(len = :, kind = c_char), pointer :: string => null()
    !* Completely polymorphic.
    class(*), pointer :: generic => null()
    !* Designate the type of the element.
    integer(c_int) :: type = QUEUE_NONE
  end type queue_data


  type :: queue_node
    type(queue_node), pointer :: next => null()
    type(queue_data), pointer :: data => null()
  end type queue_node


  type :: concurrent_linked_filo_queue
    type(queue_node), pointer :: head => null()
    type(queue_node), pointer :: tail => null()
    type(mutex_rwlock), pointer :: mutex => null()
    type(c_ptr) :: c_mutex_pointer = c_null_ptr
  contains
    procedure :: push => concurrent_linked_filo_queue_push
    procedure :: pop => concurrent_linked_filo_queue_pop
    procedure :: destroy => concurrent_linked_filo_queue_destroy
  end type concurrent_linked_filo_queue


  interface concurrent_linked_filo_queue
    module procedure :: constructor_concurrent_linked_filo_queue
  end interface concurrent_linked_filo_queue


  interface queue_create_element
    module procedure :: queue_create_element_i32
    module procedure :: queue_create_element_i64
    module procedure :: queue_create_element_f32
    module procedure :: queue_create_element_f64

  end interface queue_create_element

contains


  function constructor_concurrent_linked_filo_queue() result(queue_new)
    implicit none

    type(concurrent_linked_filo_queue) :: queue_new

    queue_new%mutex => thread_create_mutex_pointer()
    queue_new%c_mutex_pointer = c_loc(queue_new%mutex)
  end function constructor_concurrent_linked_filo_queue


  !* Push an element into the end of a queue.
  subroutine concurrent_linked_filo_queue_push(this, generic_pointer)
    implicit none

    class(concurrent_linked_filo_queue), intent(inout) :: this
    type(queue_data), intent(in), pointer :: generic_pointer
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
  end subroutine concurrent_linked_filo_queue_push


  !* Pop the first element off the queue.
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


  !* Destroy all data in a queue.
  !! This will not destroy the mutex. You are still required to do that.
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

        !! THIS IS DEBUGGIN ONLY !!

        select case(current%data%type)
         case (QUEUE_NONE)
         case (QUEUE_I32)
         case (QUEUE_I64)
         case (QUEUE_F32)
         case (QUEUE_F64)
         case (QUEUE_BOOL)
         case (QUEUE_STRING)
         case (QUEUE_GENERIC)
        end select

        !! END DEBUGGING !!

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


!* BEGIN QUEUE ELEMENT GENERIC.


  function queue_create_element_i32(i32) result(new_queue_element_pointer)
    implicit none

    type(queue_data), pointer :: new_queue_element_pointer
    integer(c_int), intent(in), value :: i32

    allocate(new_queue_element_pointer%i32)
    new_queue_element_pointer%i32 = i32
  end function queue_create_element_i32


  function queue_create_element_i64(i64) result(new_queue_element_pointer)
    implicit none

    type(queue_data), pointer :: new_queue_element_pointer
    integer(c_int64_t), intent(in), value :: i64

    allocate(new_queue_element_pointer%i64)
    new_queue_element_pointer%i64 = i64
  end function queue_create_element_i64


  function queue_create_element_f32(f32) result(new_queue_element_pointer)
    implicit none

    type(queue_data), pointer :: new_queue_element_pointer
    real(c_float), intent(in), value :: f32

    allocate(new_queue_element_pointer%f32)
    new_queue_element_pointer%f32 = f32
  end function queue_create_element_f32


  function queue_create_element_f64(f64) result(new_queue_element_pointer)
    implicit none

    type(queue_data), pointer :: new_queue_element_pointer
    real(c_double), intent(in), value :: f64

    allocate(new_queue_element_pointer%f64)
    new_queue_element_pointer%f64 = f64
  end function queue_create_element_f64


end module thread_filo_queue
