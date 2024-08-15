module dirent
  use, intrinsic :: iso_c_binding
  implicit none


  private

  public :: c_dirent
  public :: close_dir
  public :: open_dir
  public :: read_dir


  type, bind(c) :: c_dirent
    integer(c_size_t) :: d_ino
    type(c_ptr) :: d_name
  end type c_dirent


  interface


    function close_dir(dir_pointer) result(status) bind(c, name = "closedir")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: dir_pointer
      integer(c_int) :: status
    end function close_dir


    function internal_open_dir(path_name) result(dir_pointer) bind(c, name = "opendir")
      use, intrinsic :: iso_c_binding
      implicit none

      character(c_char), intent(in) :: path_name
      type(c_ptr) :: dir_pointer
    end function internal_open_dir


    function internal_read_dir(dir_pointer) result(dirent_pointer) bind(c, name = "readdir")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: dir_pointer
      type(c_ptr) :: dirent_pointer
    end function internal_read_dir

  end interface


contains

  function open_dir(path_name) result(dir_pointer)
    use, intrinsic :: iso_c_binding
    use :: string
    implicit none

    character(c_char), intent(in) :: path_name
    type(c_ptr) :: dir_pointer
    character(len = :, kind = c_char), allocatable :: c_path_name

    c_path_name = into_c_string(path_name)

    dir_pointer = internal_open_dir(c_path_name)
  end function open_dir

  subroutine read_dir(dir_pointer)
    implicit none

    type(c_ptr), intent(in), value :: dir_pointer

  end subroutine read_dir


end module dirent
