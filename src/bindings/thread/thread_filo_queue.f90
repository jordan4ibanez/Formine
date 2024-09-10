module thread_filo_queue
  use :: thread_types
  use :: thread_mutex
  use, intrinsic :: iso_c_binding
  implicit none


  private


  public :: concurrent_linked_filo_queue
  public :: queue_data


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

  interface queue_data
    module procedure :: queue_data_constructor
  end interface queue_data



  type :: queue_node
    type(queue_node), pointer :: next => null()
    type(queue_data), pointer :: data => null()
  end type queue_node


  type :: concurrent_linked_filo_queue
    private
    type(queue_node), pointer :: head => null()
    type(queue_node), pointer :: tail => null()
    type(mutex_rwlock), pointer :: mutex => null()
    type(c_ptr) :: c_mutex_pointer = c_null_ptr
    integer(c_int) :: items = 0
  contains
    procedure :: push => concurrent_linked_filo_queue_push
    procedure :: pop => concurrent_linked_filo_queue_pop
    procedure :: destroy => concurrent_linked_filo_queue_destroy
    procedure :: is_empty => concurrent_linked_filo_queue_is_empty
    procedure :: get_size => concurrent_linked_filo_queue_get_size
  end type concurrent_linked_filo_queue


  interface concurrent_linked_filo_queue
    module procedure :: constructor_concurrent_linked_filo_queue
  end interface concurrent_linked_filo_queue


contains


  function constructor_concurrent_linked_filo_queue() result(new_queue)
    implicit none

    type(concurrent_linked_filo_queue) :: new_queue

    new_queue%mutex => thread_create_mutex_pointer()
    new_queue%c_mutex_pointer = c_loc(new_queue%mutex)
  end function constructor_concurrent_linked_filo_queue


  !* Push an element into the end of a queue.
  subroutine concurrent_linked_filo_queue_push(this, generic_pointer)
    implicit none

    class(concurrent_linked_filo_queue), intent(inout) :: this
    type(queue_data), intent(in), pointer :: generic_pointer
    integer(c_int) :: status
    type(queue_node), pointer :: new_node

    status = thread_write_lock(this%c_mutex_pointer)
    !! BEGIN SAFE OPERATION.

    if (.not. associated(generic_pointer)) then
      error stop "[Thread FILO Queue] Error: Received a null pointer."
    end if

    allocate(new_node)
    new_node%data => generic_pointer

    ! If the head is null, this is the new head.
    if (.not. associated(this%head)) then
      this%head => new_node
    end if

    ! If we have a tail, it now points to the new node.
    ! The new node then becomes the tail.
    if (associated(this%tail)) then
      this%tail%next => new_node
      this%tail => new_node
    else
      ! If we do not have a tail, the new node is now the tail.
      this%tail => new_node
    end if

    this%items = this%items + 1

    !! END SAFE OPERATION.
    status = thread_unlock_lock(this%c_mutex_pointer)
  end subroutine concurrent_linked_filo_queue_push


  !* Pop the first element off the queue.
  function concurrent_linked_filo_queue_pop(this, generic_pointer_option) result(some)
    implicit none

    class(concurrent_linked_filo_queue), intent(inout) :: this
    class(*), intent(inout), pointer :: generic_pointer_option
    logical(c_bool) :: some
    integer(c_int) :: status
    type(queue_node), pointer :: next_pointer

    status = thread_write_lock(this%c_mutex_pointer)
    !! BEGIN SAFE OPERATION.

    some = .false.

    generic_pointer_option => null()

    ! If we have a head, the output will become the head data.
    ! The head will now be shifted forward, and the old head will be cleaned up.
    if (associated(this%head)) then

      some = .true.

      next_pointer => this%head%next

      ! First we unshell the data.
      select case(this%head%data%type)
       case (QUEUE_NONE)
        error stop "QUEUE FAILURE!"
       case (QUEUE_I32)
        generic_pointer_option => this%head%data%i32
       case (QUEUE_I64)
        generic_pointer_option => this%head%data%i64
       case (QUEUE_F32)
        generic_pointer_option => this%head%data%f32
       case (QUEUE_F64)
        generic_pointer_option => this%head%data%f64
       case (QUEUE_BOOL)
        generic_pointer_option => this%head%data%bool
       case (QUEUE_STRING)
        generic_pointer_option => this%head%data%string
       case (QUEUE_GENERIC)
        generic_pointer_option => this%head%data%generic
      end select

      ! Then we deallocate.
      deallocate(this%head%data)
      deallocate(this%head)

      this%head => next_pointer

      this%items = this%items - 1
    end if

    !* If the head was pointed to null, we must nullify the tail.
    if (.not. associated(this%head)) then
      this%tail => null()
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

    status = thread_write_lock(this%c_mutex_pointer)
    !! BEGIN SAFE OPERATION.

    if (associated(this%head)) then

      current => this%head

      do
        next => current%next

        select case(current%data%type)
         case (QUEUE_NONE)
          error stop "QUEUE FAILURE!"
         case (QUEUE_I32)
          deallocate(current%data%i32)
         case (QUEUE_I64)
          deallocate(current%data%i64)
         case (QUEUE_F32)
          deallocate(current%data%f32)
         case (QUEUE_F64)
          deallocate(current%data%f64)
         case (QUEUE_BOOL)
          deallocate(current%data%bool)
         case (QUEUE_STRING)
          deallocate(current%data%string)
         case (QUEUE_GENERIC)
          deallocate(current%data%generic)
        end select

        deallocate(current%data)
        deallocate(current)

        ! Pointing at nothing.
        if (.not. associated(next)) then
          exit
        end if

        current => next
      end do

    end if

    this%head => null()
    this%tail => null()

    this%items = 0

    !! END SAFE OPERATION.
    status = thread_unlock_lock(this%c_mutex_pointer)
  end subroutine concurrent_linked_filo_queue_destroy


  function queue_data_constructor(generic) result(new_queue_element_pointer)
    implicit none

    type(queue_data), pointer :: new_queue_element_pointer
    class(*), intent(in), target :: generic

    allocate(new_queue_element_pointer)

    select type(generic)
     type is (integer(c_int))
      new_queue_element_pointer%type = QUEUE_I32
      allocate(new_queue_element_pointer%i32)
      new_queue_element_pointer%i32 = generic

     type is (integer(c_int64_t))
      new_queue_element_pointer%type = QUEUE_I64
      allocate(new_queue_element_pointer%i64)
      new_queue_element_pointer%i64 = generic

     type is (real(c_float))
      new_queue_element_pointer%type = QUEUE_F32
      allocate(new_queue_element_pointer%f32)
      new_queue_element_pointer%f32 = generic

     type is (real(c_double))
      new_queue_element_pointer%type = QUEUE_F64
      allocate(new_queue_element_pointer%f64)
      new_queue_element_pointer%f64 = generic

     type is (logical)
      new_queue_element_pointer%type = QUEUE_BOOL
      allocate(new_queue_element_pointer%bool)
      new_queue_element_pointer%bool = generic

     type is (character(len = *, kind = c_char))
      !? You can thank klausler for helping me debug an issue here.
      new_queue_element_pointer%type = QUEUE_STRING
      associate (string_len => len(generic))
        allocate(character(len = string_len, kind = c_char) :: new_queue_element_pointer%string)
        new_queue_element_pointer%string = generic
      end associate

     class default
      !? We will check if this thing is a pointer.
      !! If it's not, it's going to blow up.
      new_queue_element_pointer%type = QUEUE_GENERIC
      new_queue_element_pointer%generic => generic
    end select
  end function queue_data_constructor


  !* Check if the queue is empty.
  function concurrent_linked_filo_queue_is_empty(this) result(empty)
    implicit none

    class(concurrent_linked_filo_queue), intent(inout) :: this
    logical(c_bool) :: empty
    integer(c_int) :: status

    status = thread_write_lock(this%c_mutex_pointer)
    !! BEGIN SAFE OPERATION.

    empty = this%items == 0

    !! END SAFE OPERATION.
    status = thread_unlock_lock(this%c_mutex_pointer)
  end function concurrent_linked_filo_queue_is_empty


  !* Check number of items in the queue.
  function concurrent_linked_filo_queue_get_size(this) result(item_count)
    implicit none

    class(concurrent_linked_filo_queue), intent(inout) :: this
    integer(c_int) :: item_count
    integer(c_int) :: status

    status = thread_write_lock(this%c_mutex_pointer)
    !! BEGIN SAFE OPERATION.

    item_count = this%items

    !! END SAFE OPERATION.
    status = thread_unlock_lock(this%c_mutex_pointer)
  end function concurrent_linked_filo_queue_get_size


end module thread_filo_queue
