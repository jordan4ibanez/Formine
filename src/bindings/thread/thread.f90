module thread
  use :: thread_types
  use :: vector_3i
  use, intrinsic :: iso_c_binding
  implicit none


  ! https://www.cs.cmu.edu/afs/cs/academic/class/15492-f07/www/pthreads.html
  ! https://hpc-tutorials.llnl.gov/posix/what_is_a_thread/
  !
  !* Implementation note:
  !* This has been HEAVILY modified to be easy to work with in Fortran.
  !
  ! todo: we need locks!


  private


  public :: thread_create
  public :: test_threading_implementation

  integer :: thread_status = -1

  integer(c_int), parameter :: THREAD_OK = 0
  integer(c_int), parameter :: THREAD_DOES_NOT_EXIST = 3



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

      integer(c_int64_t), intent(in), value :: thread
      type(c_ptr), intent(in), value :: retval
      integer(c_int) :: status

    end function pthread_join


!* BEGIN FUNCTION BLUEPRINTS.


    recursive subroutine thread_function_c_interface(arg) bind(c)
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: arg
    end subroutine thread_function_c_interface


  end interface


contains


  subroutine thread_create(function_pointer, arg)
    use :: string
    implicit none

    type(c_funptr), intent(in), value :: function_pointer
    type(c_ptr), intent(in), value :: arg
    integer(c_int) :: local_thread_status
    character(len = :, kind = c_char), allocatable, target :: test_data
    type(pthread_t) :: thread

    type(vec3i), target :: i
    character(len = :, kind = c_char), allocatable, target :: z

    z = "hi there"//achar(0)

    local_thread_status = internal_pthread_create(thread, c_null_ptr, function_pointer, c_loc(z))

    print*,thread%tid

    print*, "we're doing stuff in fortran while we wait"

    if (pthread_join(thread%tid, c_null_ptr) /= THREAD_OK) then
      error stop "[Thread] Error: Created a non-existent thread!"
    end if

    print*,"continuing!"


  end subroutine thread_create


  recursive function test_threading_implementation(arg) result(status) bind(c)
    use :: string
    use :: raw_c
    implicit none

    type(c_ptr), intent(in), value :: arg
    integer(c_int) :: status
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

    do i = 1,2147483646
      w = i + 1
    end do

    print*,"testing", w

    status = 0
    return
  end function test_threading_implementation


end module thread
