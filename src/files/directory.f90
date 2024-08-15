module directory
  use :: string
  use, intrinsic :: iso_c_binding
  implicit none


  private

  public :: directory_reader



  type :: file_component
    type(heap_string) :: file_name
    logical :: is_folder
  end type file_component

  !* Ultra duct-taped together directory reader.
  !* We already have the tools on the system, use them.
  type :: directory_reader
  contains
    procedure :: read_directory
  end type directory_reader


contains

  !* This is an extreme hack job because I don't feel like writing libc bindings. :D
  subroutine read_directory(this, path)
    implicit none

    class(directory_reader), intent(inout) :: this
    character(len = *, kind = c_char), intent(in) :: path



  end subroutine read_directory


end module directory
