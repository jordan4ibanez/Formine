module thread_filo_queue
  use :: thread
  use, intrinsic :: iso_c_binding
  implicit none


  private


  public :: concurrent_linked_filo_queue
  public :: queue_data
  public :: queue_data_constructor


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
          error stop
         case (QUEUE_I32)
          print*,current%data%i32
         case (QUEUE_I64)
          print*,current%data%i64
         case (QUEUE_F32)
          print*,current%data%f32
         case (QUEUE_F64)
          print*,current%data%f64
         case (QUEUE_BOOL)
          print*,current%data%bool
         case (QUEUE_STRING)
          print*,current%data%string
         case (QUEUE_GENERIC)
          print*,"NO IDEA!"
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

    this%head => null()
    this%tail => null()

    !! END SAFE OPERATION.
    status = thread_unlock_lock(this%c_mutex_pointer)
  end subroutine concurrent_linked_filo_queue_destroy


  function queue_data_constructor(generic) result(new_queue_element_pointer)
    implicit none

    type(queue_data), pointer :: new_queue_element_pointer
    class(*), intent(in), target :: generic
    character(len = :, kind = c_char), allocatable :: temp

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
      new_queue_element_pointer%type = QUEUE_STRING
      ! I am working around a GCC bug.
      temp = generic
      associate (string_len => len(temp))
        allocate(character(len = string_len, kind = c_char) :: new_queue_element_pointer%string)
        new_queue_element_pointer%string(1:string_len) = temp(1:string_len)
      end associate

     class default
      !? We will check if this thing is a pointer.
      !! If it's not, it's going to blow up.
      new_queue_element_pointer%type = QUEUE_GENERIC
      new_queue_element_pointer%generic => generic
    end select
  end function queue_data_constructor


end module thread_filo_queue
