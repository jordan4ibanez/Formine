module thread_types
  use, intrinsic :: iso_c_binding
  implicit none


  ! https://ffmpeg.org/doxygen/3.1/os2threads_8h_source.html
  type, bind(c) :: pthread_t
    integer(c_int64_t) :: tid
    type(c_funptr) :: start_routine
    type(c_ptr) :: arg
    type(c_ptr) :: result
  end type pthread_t


end module thread_types
