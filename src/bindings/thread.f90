module thread
  use, intrinsic :: iso_c_binding
  implicit none
  ! https://www.cs.cmu.edu/afs/cs/academic/class/15492-f07/www/pthreads.html
  ! https://hpc-tutorials.llnl.gov/posix/what_is_a_thread/

  private


  interface


    function internal_pthread_create(thread_id, attr, start_routine, arg) result(status) bind(c, name = "pthread_create")
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int64_t), intent(inout) :: thread_id
      type(c_ptr), intent(in), value :: attr
      type(c_funptr), intent(in), value :: start_routine
      type(c_ptr), intent(inout) :: arg
      integer(c_int) :: status
    end function internal_pthread_create


  end interface


contains


end module thread
