module thread
  use :: thread_types
  use :: thread_mutex
  use :: thread_filo_queue
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
  public :: thread_argument
  public :: thread_queue_element
  public :: mutex_rwlock
  public :: concurrent_linked_filo_queue

  public :: thread_write_lock
  public :: thread_read_lock
  public :: thread_unlock_lock

  public :: thread_initialize
  public :: thread_create_mutex_pointer
  public :: thread_destroy_mutex_pointer
  public :: thread_create
  public :: thread_process_thread_queue
  public :: thread_queue_is_empty
  public :: thread_await_all_thread_completion

  integer(c_int), parameter :: THREAD_OK = 0
  integer(c_int), parameter :: THREAD_DOES_NOT_EXIST = 3

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


    function internal_pthread_join(thread, retval) result(status) bind(c, name = "pthread_join")
      use :: thread_types
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int64_t), intent(in), value :: thread
      type(c_ptr), intent(in), value :: retval
      integer(c_int) :: status
    end function internal_pthread_join


!* BEGIN FUNCTION BLUEPRINTS.


    recursive function thread_function_c_interface(arg_pointer) result(void_pointer) bind(c)
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: arg_pointer
      type(c_ptr) :: void_pointer
    end function thread_function_c_interface


    subroutine thread_garbage_collector_c_interface(old_data) bind(c)
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: old_data
    end subroutine thread_garbage_collector_c_interface


  end interface


contains


  !* Fire up the module.
  subroutine thread_initialize()
    implicit none

    integer(c_int) :: i

    CPU_THREADS = for_p_thread_get_cpu_threads()

    allocate(module_mutex)
    module_mutex => thread_create_mutex_pointer()

    allocate(available_threads(CPU_THREADS))
    allocate(thread_arguments(CPU_THREADS))

    allocate(thread_active(CPU_THREADS))

    do i = 1,CPU_THREADS
      thread_active(i) = .false.
    end do

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
  function create_joinable(subroutine_procedure_pointer, argument_pointer) result(new_joinable_thread) bind(c)
    use :: string, only: int_to_string
    implicit none

    type(c_funptr), intent(in), value :: subroutine_procedure_pointer
    type(c_ptr), intent(in), value :: argument_pointer
    type(pthread_t) :: new_joinable_thread
    integer(c_int) :: status

    status = internal_pthread_create(new_joinable_thread, c_null_ptr, subroutine_procedure_pointer, argument_pointer)

    if (status /= THREAD_OK) then
      error stop "[Thread] Error: Failed to create a joinable thread. Error status: ["//int_to_string(status)//"]"
    end if
  end function create_joinable


  !* Join a thread back into the main thread.
  subroutine thread_join(joinable_thread, return_val_pointer) bind(c)
    use :: string, only: int_to_string
    implicit none

    type(pthread_t), intent(in), value :: joinable_thread
    type(c_ptr), intent(in), value :: return_val_pointer
    integer(c_int) :: status

    status = internal_pthread_join(joinable_thread%tid, return_val_pointer)

    if (status /= THREAD_OK) then
      error stop "[Forthread] Error: Tried to join non-existent joinable_thread! Error status: ["//int_to_string(status)//"]"
    end if
  end subroutine thread_join


  !* Queue up a thread to be run.
  subroutine thread_create(subroutine_procedure_pointer, argument_pointer)
    implicit none

    type(c_funptr), intent(in), value :: subroutine_procedure_pointer
    type(c_ptr), intent(in), value :: argument_pointer
    type(thread_queue_element), pointer :: new_element

    allocate(new_element)
    new_element%subroutine_pointer = subroutine_procedure_pointer
    new_element%data_to_send = argument_pointer

    call master_thread_queue%push(new_element)
  end subroutine thread_create


  !* Process all the queued threads limited by cpu threads available.
  subroutine thread_process_thread_queue()
    implicit none

    integer(c_int) :: queue_size, i, thread_to_use, status
    class(*), pointer :: generic_pointer
    type(thread_queue_element), pointer :: new_element
    logical(c_bool) :: translator_bool
    type(c_funptr) :: function_pointer

    if (master_thread_queue%is_empty()) then
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
        ! print*,"FAILURE! iter", i
        exit
      end if

      if (master_thread_queue%pop(generic_pointer)) then

        select type (generic_pointer)
         type is (thread_queue_element)
          new_element => generic_pointer
         class default
          error stop "[Thread] Error: Wrong data type in the queue."
        end select

        ! Set the completion flag.
        status = thread_write_lock(c_loc(module_mutex))

        translator_bool = .true.
        thread_active(thread_to_use) = translator_bool

        thread_arguments(thread_to_use)%mutex_pointer = c_loc(module_mutex)

        status = thread_unlock_lock(c_loc(module_mutex))

        ! Set the raw data to send.
        thread_arguments(thread_to_use)%active_flag => thread_active(thread_to_use)
        thread_arguments(thread_to_use)%data = new_element%data_to_send

        function_pointer = new_element%subroutine_pointer

        ! Now clean up the shell.
        deallocate(new_element)

        ! Clean up old thread data.
        if (available_threads(thread_to_use)%tid /= 0) then
          call thread_join(available_threads(thread_to_use), c_null_ptr)
        end if

        ! Fire off the thread.
        available_threads(thread_to_use) = create_joinable(function_pointer, c_loc(thread_arguments(thread_to_use)))
      else
        ! Nothing left to get.
        exit
      end if
    end do
  end subroutine thread_process_thread_queue


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
        ! print*,"thread ", i, "free"
        ! status = thread_unlock_lock(c_loc(thread_mutex))
        exit
      end if
    end do
    status = thread_unlock_lock(c_loc(module_mutex))
  end function find_free_thread


  !* Check if the thread queue is empty.
  !* This is primarily used for debugging.
  function thread_queue_is_empty() result(is_empty)
    implicit none

    logical(c_bool) :: is_empty

    is_empty = master_thread_queue%is_empty()
  end function thread_queue_is_empty


  !* And the end of the program, wait for all threads to complete until continuing.
  function thread_await_all_thread_completion() result(keep_going)
    implicit none

    logical(c_bool) :: keep_going
    integer(c_int) :: i, status

    keep_going = .true.

    status = thread_read_lock(c_loc(module_mutex))

    do i = 1,CPU_THREADS
      if (thread_active(i)) then
        status = thread_unlock_lock(c_loc(module_mutex))
        return
      end if
    end do
    status = thread_unlock_lock(c_loc(module_mutex))

    keep_going = .false.
  end function thread_await_all_thread_completion


  !! EXAMPLE ONLY !!
  ! recursive function test_threading_example(c_arg_pointer) result(void_pointer) bind(c)
  !   implicit none

  !   type(c_ptr), intent(in), value :: c_arg_pointer
  !   type(thread_argument), pointer :: arguments
  !   type(c_ptr) :: void_pointer
  !   !? Implementation note:
  !   !? When working with strings in threads, they must be a defined size.
  !   character(len = 128, kind = c_char), pointer :: input_string
  !   ! integer(c_int), pointer :: input_data
  !   integer(c_int) :: status

  !   ! We will basically always return a null void pointer.
  !   void_pointer = c_null_ptr

  !   ! Thread passes in a thread_argument pointer.
  !   ! Check it.
  !   if (.not. c_associated(c_arg_pointer)) then
  !     error stop "[Thread] Fatal error: sent argument is null."
  !     return
  !   end if

  !   ! We shift it into Fortran.
  !   call c_f_pointer(c_arg_pointer, arguments)

  !   ! Check our string void pointer.
  !   !? Remember: This is a void pointer, it can be any type.
  !   !? If you're not sure what type is getting sent where, there is an
  !   !? implementation issue and it MUST be fixed.
  !   !?
  !   !? But you can also send in null pointers, you must expect them though.
  !   if (.not. c_associated(arguments%sent_data)) then
  !     error stop "[Thread] Fatal error: Sent data is null."
  !     return
  !   end if

  !   ! Shift the string pointer into Fortran.
  !   call c_f_pointer(arguments%sent_data, input_string)

  !   ! Print it.
  !   print*,input_string

  !   ! It's a pointer, deallocate it.
  !   deallocate(input_string)

  !   ! We lock the mutex to write that the thread has completed.
  !   !? Implementation note:
  !   !! THIS MUST BE DONE.
  !   !? If this is not done, no threads will be
  !   !? freed in the state machine!
  !   status = thread_write_lock(arguments%mutex_pointer)
  !   arguments%active_flag = .false.
  !   status = thread_unlock_lock(arguments%mutex_pointer)

  !   ! The null void pointer is returned.
  ! end function test_threading_example


end module thread
