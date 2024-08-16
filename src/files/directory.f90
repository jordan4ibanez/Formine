module directory
  use :: string
  use, intrinsic :: iso_c_binding
  implicit none


  private

  public :: directory_reader


  !* This is to be maintained as synchronous.
  !* This pipe is named as a dotfile to prevent the user from seeing this constantly opening
  !* and closing as the directory reader works.
  !* Note: This file only exists in memory.
  character(len = 30, kind = c_char), parameter :: FIFO_PIPE = ".formine_fifo_operator_hackjob"


  type :: file_component
    type(heap_string) :: file_name
    logical :: is_folder
  end type file_component

  !* Ultra duct-taped together directory reader.
  !* We already have the tools on the system, use them.
  type :: directory_reader
    type(heap_string), dimension(:), allocatable :: files
  contains
    procedure :: read_directory
  end type directory_reader


  interface


    !* dirent.
    function internal_open_dir(path) result(dir_pointer) bind(c, name = "opendir")
      use, intrinsic :: iso_c_binding
      implicit none

      character(kind = c_char), intent(in) :: path
      type(c_ptr) :: dir_pointer
    end function internal_open_dir


    !* Custom built upon dirent.
    !* Basically, it will
    subroutine parse_directory_folders(dir_pointer) bind(c, name = "parse_directory_folders")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: dir_pointer
    end subroutine parse_directory_folders

  end interface


contains

  function open_directory(path) result(dir_pointer)
    use, intrinsic :: iso_c_binding
    implicit none

    character(kind = c_char), intent(in) :: path
    type(c_ptr) :: dir_pointer
    character(len = :, kind = c_char), allocatable :: c_path

    c_path = into_c_string(path)

    dir_pointer = internal_open_dir(c_path)
  end function open_directory


  subroutine read_directory(this, path)
    implicit none

    class(directory_reader), intent(inout) :: this
    character(len = *, kind = c_char), intent(in) :: path
    type(c_ptr) :: dir

    dir = open_directory("./")

    if (c_associated(dir)) then
      print*,"it's associated"
    end if

    ! call say_hello()

    ! call parse_directory_folders(dir)

  end subroutine read_directory


end module directory
