module thread_mutex
  use :: thread_types
  implicit none

  public :: thread_write_lock
  public :: thread_read_lock
  public :: thread_unlock_lock


  interface


    function internal_pthread_rwlock_init(rwlock, attr) result(status) bind(c, name = "pthread_rwlock_init")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: rwlock
      type(c_ptr), intent(in), value :: attr
      integer(c_int) :: status
    end function internal_pthread_rwlock_init


    function internal_pthread_rwlock_destroy(rwlock, attr) result(status) bind(c, name = "pthread_rwlock_destroy")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: rwlock
      type(c_ptr), intent(in), value :: attr
      integer(c_int) :: status
    end function internal_pthread_rwlock_destroy


    function thread_write_lock(rwlock) result(status) bind(c, name = "pthread_rwlock_wrlock")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: rwlock
      integer(c_int) :: status
    end function thread_write_lock


    function thread_read_lock(rwlock) result(status) bind(c, name = "pthread_rwlock_rdlock")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: rwlock
      integer(c_int) :: status
    end function thread_read_lock


    function thread_unlock_lock(rwlock) result(status) bind(c, name = "pthread_rwlock_unlock")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: rwlock
      integer(c_int) :: status
    end function thread_unlock_lock

  end interface


contains

  !* Create a new mutex pointer.
  function thread_create_mutex_pointer() result(new_mutex_pointer)
    implicit none

    type(mutex_rwlock), pointer :: new_mutex_pointer
    integer(c_int) :: status

    allocate(new_mutex_pointer)
    allocate(new_mutex_pointer%raw_data_pointer(for_p_thread_get_pthread_mutex_t_width()))

    status = internal_pthread_rwlock_init(c_loc(new_mutex_pointer), c_null_ptr)
  end function thread_create_mutex_pointer

end module thread_mutex
