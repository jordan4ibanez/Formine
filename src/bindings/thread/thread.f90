module thread
  use :: thread_types
  use :: thread_mutex
  use :: thread_filo_queue
  use :: vector_3i
  use, intrinsic :: iso_c_binding
  implicit none


  ! https://www.cs.cmu.edu/afs/cs/academic/class/15492-f07/www/pthreads.html
  ! https://hpc-tutorials.llnl.gov/posix/what_is_a_thread/
  ! https://docs.oracle.com/cd/E19455-01/806-5257/6je9h032u/index.html
  ! https://pubs.opengroup.org/onlinepubs/007908799/xsh/pthread_rwlock_unlock.html
  ! Also: All of the Linux man pages LOL.
  !
  !* Implementation note:
  !* This has been HEAVILY modified to be easy to work with in Fortran.
  !
  ! Detached thread runthrough:
  !
  ! 1.) thread_create_detached()
  ! Goes into queue.
  !
  ! 2.) thread_process_detached_thread_queue()
  ! Calls pop_thread_queue()
  !
  ! 3.) thread_process_detached_thread()
  ! Will delete old thread attributes.
  ! Passes required data pointers into slots.
  !
  !


  private

  public :: pthread_t
  public :: thread_queue_element
  public :: mutex_rwlock

  public :: thread_write_lock
  public :: thread_read_lock
  public :: thread_unlock_lock

  public :: thread_initialize
  public :: thread_create_mutex_pointer
  public :: thread_destroy_mutex_pointer
  public :: thread_create_joinable
  public :: thread_set_name
  public :: thread_get_name
  public :: thread_wait_for_joinable
  public :: thread_create_detached
  public :: thread_process_detached_thread_queue
  public :: thread_detached_queue_is_empty
  public :: thread_await_all_thread_completion
  public :: test_threading_implementation

  integer(c_int), parameter :: THREAD_OK = 0
  integer(c_int), parameter :: THREAD_DOES_NOT_EXIST = 3

  integer(c_int) :: THREAD_DETACH

  integer(c_int) :: CPU_THREADS = 0

  type(mutex_rwlock), pointer :: module_mutex

  type(pthread_t), dimension(:), pointer :: available_threads
  type(thread_argument), dimension(:), pointer :: thread_arguments
  logical(c_bool), dimension(:), pointer :: thread_active

  type(concurrent_linked_filo_queue) :: master_thread_queue


  interface


    function internal_pthread_create(thread, attr, start_routine, arg) result(status) bind(c, name = "pthread_create")
      use :: thread_types
      use, intrinsic :: iso_c_binding
      implicit none

      type(pthread_t), intent(inout) :: thread
      type(c_ptr), intent(in), value :: attr
      type(c_funptr), intent(in), value :: start_routine
      type(c_ptr), intent(in), value :: arg
      integer(c_int) :: status
    end function internal_pthread_create


    function pthread_cancel(thread) result(status) bind(c, name = "pthread_cancel")
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int64_t), intent(in), value :: thread
      integer(c_int) :: status
    end function pthread_cancel


    function internal_pthread_setname_np(thread, name) result(status) bind(c, name = "pthread_setname_np")
      use :: thread_types
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int64_t), intent(in), value :: thread
      character(len = 1, kind = c_char), intent(in) :: name
      integer(c_int) :: status
    end function internal_pthread_setname_np


    function internal_pthread_getname_np(thread, name, len) result(status) bind(c, name = "pthread_getname_np")
      use :: thread_types
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int64_t), intent(in), value :: thread
      type(c_ptr), intent(in), value :: name
      integer(c_size_t), intent(in), value :: len
      integer(c_int) :: status
    end function internal_pthread_getname_np


    function internal_pthread_join(thread, retval) result(status) bind(c, name = "pthread_join")
      use :: thread_types
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int64_t), intent(in), value :: thread
      type(c_ptr), intent(in), value :: retval
      integer(c_int) :: status
    end function internal_pthread_join


!* THIS PART IS EXTREMELY COMPLEX.


    function pthread_detach(thread) result(status) bind(c, name = "pthread_detach")
      use :: thread_types
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int64_t), intent(in), value :: thread
      integer(c_int) :: status
    end function pthread_detach


    function internal_pthread_attr_init(attr) result(status) bind(c, name = "pthread_attr_init")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: attr
      integer(c_int) :: status
    end function internal_pthread_attr_init


    function internal_pthread_attr_destroy(attr) result(status) bind(c, name = "pthread_attr_destroy")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: attr
      integer(c_int) :: status
    end function internal_pthread_attr_destroy


    function internal_pthread_attr_setdetachstate(attr, detachstate) result(status) bind(c, name = "pthread_attr_setdetachstate")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: attr
      integer(c_int), intent(in), value :: detachstate
      integer(c_int) :: status
    end function


    function internal_pthread_attr_getdetachstate(attr, detachstate) result(status) bind(c, name = "pthread_attr_getdetachstate")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: attr
      integer(c_int), intent(inout) :: detachstate
      integer(c_int) :: status
    end function internal_pthread_attr_getdetachstate



!* BEGIN FUNCTION BLUEPRINTS.


    recursive subroutine thread_function_c_interface(arg) bind(c)
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: arg
    end subroutine thread_function_c_interface


  end interface


contains


  !* Fire up the module.
  subroutine thread_initialize()
    implicit none

    THREAD_DETACH = for_p_thread_get_pthread_create_detached_id()
    CPU_THREADS = for_p_thread_get_cpu_threads()

    allocate(module_mutex)
    module_mutex => thread_create_mutex_pointer()

    allocate(available_threads(CPU_THREADS))
    allocate(thread_arguments(CPU_THREADS))
    allocate(thread_active(CPU_THREADS))
    master_thread_queue = concurrent_linked_filo_queue()
  end subroutine thread_initialize




  !* Destroy a mutex pointer.
  subroutine thread_destroy_mutex_pointer(input_mutex_pointer)
    implicit none

    type(mutex_rwlock), intent(inout), pointer :: input_mutex_pointer
    integer(c_int) :: status

    status = internal_pthread_rwlock_destroy(c_loc(input_mutex_pointer), c_null_ptr)

    deallocate(input_mutex_pointer%raw_data_pointer)
    deallocate(input_mutex_pointer)
  end subroutine thread_destroy_mutex_pointer


  !* Create a new joinable thread.
  !* Returns you the thread struct.
  function thread_create_joinable(subroutine_procedure_pointer, argument_pointer) result(joinable_thread_new) bind(c)
    use :: string, only: int_to_string
    implicit none

    type(c_funptr), intent(in), value :: subroutine_procedure_pointer
    type(c_ptr), intent(in), value :: argument_pointer
    type(pthread_t) :: joinable_thread_new
    integer(c_int) :: status

    status = internal_pthread_create(joinable_thread_new, c_null_ptr, subroutine_procedure_pointer, argument_pointer)

    if (status /= THREAD_OK) then
      error stop "[Thread] Error: Failed to create a joinable thread. Error status: ["//int_to_string(status)//"]"
    end if
  end function thread_create_joinable


  !* Set a thread's name.
  subroutine thread_set_name(thread, name) bind(c)
    use :: string, only: int_to_string, string_from_c
    implicit none

    type(pthread_t), intent(inout) :: thread
    character(len = *, kind = c_char), intent(in) :: name
    character(len = :, kind = c_char), allocatable, target :: c_name
    integer(c_int) :: status

    !* Implementation note:
    !* We ignore the status because this thread could have already finished by the time we get here.

    c_name = name//achar(0)
    status = internal_pthread_setname_np(thread%tid, c_name)
  end subroutine thread_set_name


  !* Set a thread's name.
  !* If the thread does not exist, this will return "".
  function thread_get_name(thread) result(thread_name)
    use :: string, only: int_to_string, string_from_c
    implicit none

    type(pthread_t), intent(in), value :: thread
    character(len = :, kind = c_char), allocatable :: thread_name
    type(c_ptr) :: c_string_pointer
    integer(c_int) :: status

    status = internal_pthread_getname_np(thread%tid, c_string_pointer, 128_8)

    if (status /= THREAD_OK) then
      thread_name = ""
      return
    end if

    thread_name = string_from_c(c_string_pointer, 128)
  end function thread_get_Name


  !* Wait for a thread to be finished then reclaim it's data and get it's return.
  subroutine thread_wait_for_joinable(joinable_thread, return_val_pointer) bind(c)
    use :: string, only: int_to_string
    implicit none

    type(pthread_t), intent(in), value :: joinable_thread
    type(c_ptr), intent(in), value :: return_val_pointer
    integer(c_int) :: status

    status = internal_pthread_join(joinable_thread%tid, return_val_pointer)

    if (status /= THREAD_OK) then
      error stop "[joinable_thread] Error: Tried to join non-existent joinable_thread! Error status: ["//int_to_string(status)//"]"
    end if
  end subroutine thread_wait_for_joinable


  !* Custom hack job to allocate a pthread union into memory.
  function allocate_raw_pthread_attr_t() result(thread_new_attr)
    implicit none

    type(pthread_attr_t) :: thread_new_attr

    allocate(thread_new_attr%raw_data_pointer(for_p_thread_get_pthread_attr_t_width()))
  end function allocate_raw_pthread_attr_t


  !* Queue up a thread to be run.
  subroutine thread_create_detached(subroutine_procedure_pointer, argument_pointer)
    implicit none

    type(c_funptr), intent(in), value :: subroutine_procedure_pointer
    type(c_ptr), intent(in), value :: argument_pointer
    type(thread_queue_element), pointer :: element_new

    allocate(element_new)
    element_new%subroutine_pointer = subroutine_procedure_pointer
    element_new%data_to_send = argument_pointer

    call master_thread_queue%push(queue_data(element_new))
  end subroutine thread_create_detached


  !* Process all the queued threads limited by cpu threads available.
  function thread_process_detached_thread_queue() result(is_empty)
    implicit none

    logical(c_bool) :: is_empty
    integer(c_int) :: queue_size, i, thread_to_use, status
    class(*), pointer :: generic_pointer
    type(thread_queue_element), pointer :: optional_thread_queue_element_pointer

    is_empty = .false.

    if (master_thread_queue%is_empty()) then
      is_empty = .true.
      return
    end if

    queue_size = master_thread_queue%get_size()

    ! Don't attempt to go past available threads.
    if (queue_size > CPU_THREADS) then
      queue_size = CPU_THREADS
    end if

    do i = 1,queue_size

      thread_to_use = find_free_thread()

      ! If there's no available threads, stop.
      if (thread_to_use == 0) then
        exit
      end if

      if (master_thread_queue%pop(generic_pointer)) then

        select type (generic_pointer)
         type is (thread_queue_element)
          optional_thread_queue_element_pointer => generic_pointer
         class default
          error stop "[Thread] Error: Wrong data type in the queue."
        end select

        status = thread_write_lock(c_loc(module_mutex))

        thread_active(thread_to_use) = .true.
        thread_arguments(thread_to_use)%mutex_pointer = c_loc(module_mutex)

        status = thread_unlock_lock(c_loc(module_mutex))

        thread_arguments(thread_to_use)%active_flag => thread_active(thread_to_use)
        thread_arguments(thread_to_use)%sent_data = optional_thread_queue_element_pointer%data_to_send

        call thread_process_detached_thread(optional_thread_queue_element_pointer%subroutine_pointer, c_loc(thread_arguments(thread_to_use)), thread_to_use)

        deallocate(optional_thread_queue_element_pointer)
      else
        ! Nothing left to get.
        exit
      end if
    end do
  end function thread_process_detached_thread_queue


  !* Simply searches for a free thread to dispatch.
  !* This is a very naive implementation.
  function find_free_thread() result(thread_index)
    implicit none

    integer(c_int) :: thread_index, i, status

    thread_index = 0

    status = thread_read_lock(c_loc(module_mutex))
    do i = 1,CPU_THREADS
      if (.not. thread_active(i)) then
        thread_index = i
        ! status = thread_unlock_lock(c_loc(thread_mutex))
        exit
      end if
    end do
    status = thread_unlock_lock(c_loc(module_mutex))
  end function find_free_thread


  !* Check if the thread queue is empty.
  !* This is primarily used for debugging.
  function thread_detached_queue_is_empty() result(is_empty)
    implicit none

    logical(c_bool) :: is_empty

    is_empty = master_thread_queue%is_empty()
  end function thread_detached_queue_is_empty


  !* Process a thread and send it into action.
  subroutine thread_process_detached_thread(subroutine_procedure_pointer, argument_pointer, thread_index) bind(c)
    use :: string, only: int_to_string
    implicit none

    type(c_funptr), intent(in), value :: subroutine_procedure_pointer
    type(c_ptr), intent(in), value :: argument_pointer
    integer(c_int), intent(in), value :: thread_index
    type(pthread_t) :: detached_thread_new
    type(pthread_attr_t) :: thread_attributes
    integer(c_int) :: status

    thread_attributes = allocate_raw_pthread_attr_t()
    status = internal_pthread_attr_init(c_loc(thread_attributes%raw_data_pointer))

    if (status /= THREAD_OK) then
      error stop "[Thread] Error: Failed to create a thread attribute. Error status: ["//int_to_string(status)//"]"
    end if

    status = internal_pthread_attr_setdetachstate(c_loc(thread_attributes%raw_data_pointer), THREAD_DETACH)

    if (status /= THREAD_OK) then
      error stop "[Thread] Error: Failed to set thread attribute [detachstate]. Error status: ["//int_to_string(status)//"]"
    end if

    available_threads(thread_index) = detached_thread_new

    status = internal_pthread_create(available_threads(thread_index), c_loc(thread_attributes%raw_data_pointer), subroutine_procedure_pointer, argument_pointer)

    if (status /= THREAD_OK) then
      error stop "[Thread] Error: Failed to create a detached thread. Error status: ["//int_to_string(status)//"]"
    end if

    ! Clean up attribute data.
    status = internal_pthread_attr_destroy(c_loc(thread_attributes%raw_data_pointer))

    if (status /= THREAD_OK) then
      error stop "[Thread] Error: Failed to destroy a detached thread. Error status: ["//int_to_string(status)//"]"
    end if

    deallocate(thread_attributes%raw_data_pointer)

  end subroutine thread_process_detached_thread


  !* And the end of the program, wait for all threads to complete until continuing.
  function thread_await_all_thread_completion() result(still_processing)
    implicit none

    logical(c_bool) :: still_processing
    integer(c_int) :: i, status

    still_processing = .true.

    status = thread_read_lock(c_loc(module_mutex))
    do i = 1,CPU_THREADS
      if (thread_active(i)) then
        status = thread_unlock_lock(c_loc(module_mutex))
        call sleep(0)
        return
      end if
    end do
    status = thread_unlock_lock(c_loc(module_mutex))

    still_processing = .false.
  end function thread_await_all_thread_completion


  !! TESTING/EXAMPLE ONLY !!
  recursive function test_threading_implementation(c_arg_pointer) result(void_pointer) bind(c)
    use :: string
    use :: raw_c
    implicit none

    type(c_ptr), intent(in), value :: c_arg_pointer
    type(thread_argument), pointer :: arguments
    type(c_ptr) :: void_pointer
    character(len = 128, kind = c_char), pointer :: input_string
    ! integer(c_int), pointer :: input_data
    integer(c_int) :: status

    if (.not. c_associated(c_arg_pointer)) then
      print*,"thread association failure"
      return
    end if

    call c_f_pointer(c_arg_pointer, arguments)


    call c_f_pointer(arguments%sent_data, input_string)

    ! print*,c_loc(input_string)

    ! print*,input_string

    deallocate(input_string)

    status = thread_write_lock(arguments%mutex_pointer)

    arguments%active_flag = .false.

    status = thread_unlock_lock(arguments%mutex_pointer)

    void_pointer = c_null_ptr
  end function test_threading_implementation


end module thread
