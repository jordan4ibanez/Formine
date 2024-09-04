module thread
  use :: thread_types
  use :: vector_3i
  use, intrinsic :: iso_c_binding
  implicit none


  ! https://www.cs.cmu.edu/afs/cs/academic/class/15492-f07/www/pthreads.html
  ! https://hpc-tutorials.llnl.gov/posix/what_is_a_thread/
  ! Also: All of the Linux man pages LOL.
  !
  !* Implementation note:
  !* This has been HEAVILY modified to be easy to work with in Fortran.
  !
  ! todo: we need locks!


  private

  public :: pthread_t
  public :: thread_queue_element

  public :: thread_initialize
  public :: thread_create_joinable
  public :: thread_set_name
  public :: thread_get_name
  public :: thread_wait_for_joinable
  public :: thread_create_detached
  public :: test_threading_implementation

  integer(c_int), parameter :: THREAD_OK = 0
  integer(c_int), parameter :: THREAD_DOES_NOT_EXIST = 3

  integer(c_int) :: CPU_THREADS = 0

  type(pthread_t), dimension(:), pointer :: available_threads
  type(pthread_t), dimension(:), pointer :: thread_configurations
  logical(c_bool), dimension(:), pointer :: thread_active

  type(thread_queue_element), dimension(:), pointer :: thread_queue



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


    !* BEGIN CUSTOM C BINDINGS.


    function for_p_thread_get_pthread_attr_t_width() result(data_width) bind(c, name = "for_p_thread_get_pthread_attr_t_width")
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int) :: data_width
    end function


    function for_p_thread_get_pthread_create_detached_id() result(id) bind(c, name = "for_p_thread_get_pthread_create_detached_id")
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int) :: id
    end function


    function for_p_thread_get_cpu_threads() result(thread_count) bind(c, name = "for_p_thread_get_cpu_threads")
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int) :: thread_count
    end function for_p_thread_get_cpu_threads


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

    CPU_THREADS = for_p_thread_get_cpu_threads()

    allocate(available_threads(CPU_THREADS))
    allocate(thread_configurations(CPU_THREADS))
    allocate(thread_active(CPU_THREADS))
    allocate(thread_queue(0))
  end subroutine thread_initialize


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
    type(thread_queue_element) :: element_new
    type(thread_queue_element), dimension(:), pointer :: thread_queue_new
    integer(c_int) :: old_size, i

    element_new%subroutine_pointer = subroutine_procedure_pointer
    element_new%data_to_send = argument_pointer

    ! Now move things to a bigger queue.

    old_size = size(thread_queue)

    allocate(thread_queue_new(old_size + 1))

    do i = 1,old_size
      thread_queue_new(i) = thread_queue(i)
    end do

    deallocate(thread_queue)
    thread_queue => thread_queue_new
  end subroutine thread_create_detached

  


  !* Process a thread and send it into action.
  subroutine thread_process_detached_thread(subroutine_procedure_pointer, argument_pointer) bind(c)
    use :: string, only: int_to_string
    implicit none

    type(c_funptr), intent(in), value :: subroutine_procedure_pointer
    type(c_ptr), intent(in), value :: argument_pointer
    type(pthread_t) :: detached_thread_new
    type(pthread_attr_t) :: thread_attributes
    integer(c_int) :: status

    thread_attributes = allocate_raw_pthread_attr_t()
    status = internal_pthread_attr_init(c_loc(thread_attributes%raw_data_pointer))

    if (status /= THREAD_OK) then
      error stop "[Thread] Error: Failed to create a thread attribute. Error status: ["//int_to_string(status)//"]"
    end if

    status = internal_pthread_attr_setdetachstate(c_loc(thread_attributes%raw_data_pointer), for_p_thread_get_pthread_create_detached_id())

    if (status /= THREAD_OK) then
      error stop "[Thread] Error: Failed to set thread attribute [detachstate]. Error status: ["//int_to_string(status)//"]"
    end if

    status = internal_pthread_create(detached_thread_new, c_loc(thread_attributes%raw_data_pointer), subroutine_procedure_pointer, argument_pointer)

    if (status /= THREAD_OK) then
      error stop "[Thread] Error: Failed to create a detached thread. Error status: ["//int_to_string(status)//"]"
    end if

    !todo: FIX THE MEMORY LEAK WHEN IMPLEMENTING THE QUEUE!
  end subroutine thread_process_detached_thread














  recursive subroutine test_threading_implementation(arg) bind(c)
    use :: string
    use :: raw_c
    implicit none

    type(c_ptr), intent(in), value :: arg
    ! type(vec3i), pointer :: i
    character(len = :, kind = c_char), allocatable :: z
    integer(c_int) :: i, w

    if (.not. c_associated(arg)) then
      print*,"thread association failure"
      return
    end if

    z = string_from_c(arg, 128)

    print*,"input from fortran: ["//z//"]"

    w = 1

    do i = 1,21!47483646
      w = i + 1
    end do

    do i = 1,2147483646
      w = i + 1
    end do

    do i = 1,2147483646
      w = i + 1
    end do

    do i = 1,2147483646
      w = i + 1
    end do

    do i = 1,2147483646
      w = i + 1
    end do

    do i = 1,2147483646
      w = i + 1
    end do


    print*,"testing", w
  end subroutine test_threading_implementation


end module thread
