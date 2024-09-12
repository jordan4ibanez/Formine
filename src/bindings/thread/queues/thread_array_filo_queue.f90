module thread_filo_queue_array
  use :: thread_types
  use :: thread_mutex
  use, intrinsic :: iso_c_binding
  implicit none


  private


  public :: concurrent_array_filo_queue


  type :: queue_element
    class(*), pointer :: data => null()
  end type queue_element


  type :: concurrent_array_filo_queue
    private
    type(queue_element), dimension(:), allocatable :: data
    type(mutex_rwlock), pointer :: mutex => null()
    type(c_ptr) :: c_mutex_pointer = c_null_ptr
    integer(c_int) :: items = 0
  contains
    procedure :: push => concurrent_array_filo_queue_push
    procedure :: pop => concurrent_array_filo_queue_pop
    procedure :: destroy => concurrent_array_filo_queue_destroy
    procedure :: is_empty => concurrent_array_filo_queue_is_empty
    procedure :: get_size => concurrent_array_filo_queue_get_size
  end type concurrent_array_filo_queue

  interface concurrent_array_filo_queue
    module procedure :: constructor_concurrent_array_filo_queue
  end interface concurrent_array_filo_queue


contains


  function constructor_concurrent_array_filo_queue() result(new_queue)
    implicit none

    type(concurrent_array_filo_queue) :: new_queue

    allocate(new_queue%data(0))
    new_queue%mutex => thread_create_mutex_pointer()
    new_queue%c_mutex_pointer = c_loc(new_queue%mutex)
  end function constructor_concurrent_array_filo_queue


  !* Push an element into the end of a queue.
  subroutine concurrent_array_filo_queue_push(this, generic_pointer)
    implicit none

    class(concurrent_array_filo_queue), intent(inout) :: this
    class(*), intent(in), target :: generic_pointer
    integer(c_int) :: status, old_size
    type(queue_element), dimension(:), allocatable :: new_data

    status = thread_write_lock(this%c_mutex_pointer)
    !! BEGIN SAFE OPERATION.

    old_size = this%items
    this%items = this%items + 1
    allocate(new_data(this%items))
    new_data(1:old_size) = this%data(1:old_size)
    new_data(this%items)%data => generic_pointer

    call move_alloc(new_data, this%data)

    !! END SAFE OPERATION.
    status = thread_unlock_lock(this%c_mutex_pointer)
  end subroutine concurrent_array_filo_queue_push


  !* Pop the first element off the queue.
  function concurrent_array_filo_queue_pop(this, generic_pointer_option) result(some)
    implicit none

    class(concurrent_array_filo_queue), intent(inout) :: this
    class(*), intent(inout), pointer :: generic_pointer_option
    logical(c_bool) :: some
    integer(c_int) :: status, new_size
    type(queue_element), dimension(:), allocatable :: new_data

    status = thread_write_lock(this%c_mutex_pointer)
    !! BEGIN SAFE OPERATION.

    some = .false.

    generic_pointer_option => null()

    if (this%items == 0) then
      !? EARLY RETURN END SAFE OPERATION.
      status = thread_unlock_lock(this%c_mutex_pointer)
      return
    end if

    generic_pointer_option => this%data(1)%data

    new_size = this%items - 1
    allocate(new_data(new_size))
    if (new_size > 0) then
      new_data(1:new_size) = this%data(2:this%items)
    end if

    this%items = new_size

    call move_alloc(new_data, this%data)

    some = .true.

    !! END SAFE OPERATION.
    status = thread_unlock_lock(this%c_mutex_pointer)
  end function concurrent_array_filo_queue_pop


  !* Destroy all data in a queue.
  !! This will not destroy the mutex. You are still required to do that.
  subroutine concurrent_array_filo_queue_destroy(this)
    implicit none

    class(concurrent_array_filo_queue), intent(inout) :: this
    integer(c_int) :: status, i
    type(queue_element), dimension(:), allocatable :: new_data

    status = thread_write_lock(this%c_mutex_pointer)
    !! BEGIN SAFE OPERATION.

    do i = 1,this%items
      deallocate(this%data)
    end do

    this%items = 0
    allocate(new_data(0))
    call move_alloc(new_data, this%data)

    !! END SAFE OPERATION.
    status = thread_unlock_lock(this%c_mutex_pointer)
  end subroutine concurrent_array_filo_queue_destroy


  !* Check if the queue is empty.
  function concurrent_array_filo_queue_is_empty(this) result(empty)
    implicit none

    class(concurrent_array_filo_queue), intent(inout) :: this
    logical(c_bool) :: empty
    integer(c_int) :: status

    status = thread_write_lock(this%c_mutex_pointer)
    !! BEGIN SAFE OPERATION.

    empty = this%items == 0

    !! END SAFE OPERATION.
    status = thread_unlock_lock(this%c_mutex_pointer)
  end function concurrent_array_filo_queue_is_empty


  !* Check number of items in the queue.
  function concurrent_array_filo_queue_get_size(this) result(item_count)
    implicit none

    class(concurrent_array_filo_queue), intent(inout) :: this
    integer(c_int) :: item_count
    integer(c_int) :: status

    status = thread_write_lock(this%c_mutex_pointer)
    !! BEGIN SAFE OPERATION.

    item_count = this%items

    !! END SAFE OPERATION.
    status = thread_unlock_lock(this%c_mutex_pointer)
  end function concurrent_array_filo_queue_get_size

  subroutine concurrent_array_filo_queue_destroy_mutex(this)
    implicit none

    class(concurrent_array_filo_queue), intent(inout) :: this

    call thread_destroy_mutex_pointer(this%mutex)
    this%c_mutex_pointer = c_null_ptr
  end subroutine concurrent_array_filo_queue_destroy_mutex

end module thread_filo_queue_array
