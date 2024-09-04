module thread
  use :: thread_types
  use, intrinsic :: iso_c_binding
  implicit none


  ! https://www.cs.cmu.edu/afs/cs/academic/class/15492-f07/www/pthreads.html
  ! https://hpc-tutorials.llnl.gov/posix/what_is_a_thread/
  !
  !* Implementation note:
  !* This has been HEAVILY modified to be easy to work with in Fortran.


  private


  public :: thread_create
  public :: test_threading_implementation

  !! Modifying this without a lock is A HORRIBLE IDEA !!
  integer :: hi_there


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


    function pthread_join(thread, retval) result(status) bind(c, name = "pthread_join")
      use :: thread_types
      use, intrinsic :: iso_c_binding
      implicit none

      type(pthread_t), intent(in), value :: thread
      type(c_ptr), intent(in), value :: retval
      integer(c_int) :: status

    end function pthread_join


!* BEGIN FUNCTION BLUEPRINTS.


    function thread_function_c_interface(arg) result(anything_pointer)
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: arg
      type(c_ptr) :: anything_pointer
    end function thread_function_c_interface


  end interface


contains


  subroutine thread_create(function_pointer, arg)
    implicit none

    type(c_funptr), intent(in), value :: function_pointer
    type(c_ptr), intent(in), value :: arg
    integer(c_int) :: thread_status
    character(len = :, kind = c_char), allocatable, target :: test_data
    type(pthread_t) :: thread
    type(c_ptr) :: retval

    test_data = "hi there"

    thread_status = internal_pthread_create(thread, c_null_ptr, function_pointer, c_loc(test_data))

    print*,thread%tid, thread_status

    thread_status = pthread_join(thread, c_null_ptr)

    call sleep(0)

  end subroutine thread_create


  subroutine test_threading_implementation(arg)
    use :: raw_c
    implicit none

    type(c_ptr), intent(in), value :: arg
    integer(c_int) :: status


    ! call print_f("hi there"//achar(10))
    print*,"hi from C thread!"

    print*,"arg location:", arg
    print*,"is null pointer:", .not. c_associated(arg)

    hi_there = 5

    status = 0

    return
  end subroutine test_threading_implementation


end module thread
