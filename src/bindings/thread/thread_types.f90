module thread_types
  use, intrinsic :: iso_c_binding
  implicit none


  ! Raw thread struct.
  ! https://ffmpeg.org/doxygen/3.1/os2threads_8h_source.html
  type, bind(c) :: pthread_t
    integer(c_int64_t) :: tid
    type(c_funptr) :: start_routine
    type(c_ptr) :: arg
    type(c_ptr) :: result
  end type pthread_t


  ! Raw thread configuration.
  type :: pthread_attr_t
    integer(1), dimension(:), pointer :: raw_data_pointer
  end type pthread_attr_t


  ! A raw thread queue element.
  type :: thread_queue_element
    type(c_funptr) :: subroutine_pointer
    type(c_ptr) :: data_to_send
  end type thread_queue_element


  ! What gets passed into the thread.
  type :: thread_argument
    logical(c_bool), pointer :: active_flag
    type(c_ptr) :: data_to_send
    ! todo: mutex pointer.
  end type thread_argument


end module thread_types
