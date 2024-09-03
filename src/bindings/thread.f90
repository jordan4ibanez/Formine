module thread
  use, intrinsic :: iso_c_binding
  implicit none
  ! https://www.cs.cmu.edu/afs/cs/academic/class/15492-f07/www/pthreads.html
  ! https://hpc-tutorials.llnl.gov/posix/what_is_a_thread/

  private

  public :: thread_create
  public :: test_threading

  integer :: hi_there


  ! https://ffmpeg.org/doxygen/3.1/os2threads_8h_source.html
  type, bind(c) :: pthread_t
    integer(c_int64_t) :: tid
    type(c_funptr) :: start_routine
    type(c_ptr) :: arg
    type(c_ptr) :: result
  end type pthread_t


  interface



    function internal_pthread_create(thread, attr, start_routine, arg) result(status) bind(c, name = "pthread_create")
      use, intrinsic :: iso_c_binding
      implicit none

      type, bind(c) :: pthread_t
        integer(c_int64_t) :: tid
        type(c_funptr) :: start_routine
        type(c_ptr) :: arg
        type(c_ptr) :: result
      end type pthread_t

      type(pthread_t), intent(inout) :: thread
      type(c_ptr), intent(in), value :: attr
      type(c_funptr), intent(in), value :: start_routine
      type(c_ptr), intent(in), value :: arg
      integer(c_int) :: status
    end function internal_pthread_create


    function pthread_join(thread, retval) result(status) bind(c, name = "pthread_join")
      use, intrinsic :: iso_c_binding
      implicit none

      type, bind(c) :: pthread_t
        integer(c_int64_t) :: tid
        type(c_funptr) :: start_routine
        type(c_ptr) :: arg
        type(c_ptr) :: result
      end type pthread_t

      type(pthread_t), intent(in), value :: thread
      type(c_ptr), intent(in), value :: retval
      integer(c_int) :: status


    end function pthread_join


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

    print*,1

    thread_status = internal_pthread_create(thread, c_null_ptr, function_pointer, c_loc(test_data))

    print*,2

    print*,thread%tid, thread_status

    print*,3

    thread_status = pthread_join(thread, c_null_ptr)
    print*,4

    call sleep(0)

    print*,hi_there

  end subroutine thread_create


  function test_threading(arg) result(status) bind(c)
    use :: raw_c
    implicit none

    type(c_ptr), intent(in), value :: arg
    integer(c_int) :: status


    call print_f("hi there"//achar(10))


    hi_there = 5

    status = 0

    return
  end function test_threading


end module thread
