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
  contains
    procedure :: read_directory
  end type directory_reader


contains

  !* This is an extreme hack job because I don't feel like writing libc bindings. :D
  subroutine read_directory(this, path)
    implicit none

    class(directory_reader), intent(inout) :: this
    logical :: pipe_exists
    character(len = *, kind = c_char), intent(in) :: path
    character(len = :, kind = c_char), allocatable :: message

    ! We are literally creating a custom pipe on posix to act as a virtual file.
    ! You can thank: janneb https://stackoverflow.com/a/53450193

    !! This needs a windows check.

    inquire(file = FIFO_PIPE, exist = pipe_exists)

    if (pipe_exists) then
      error stop "[Directory] Error: Never closed the FIFO!"
    end if

    call execute_command_line("ls -1 "//path//" > "//FIFO_PIPE, wait = .true.)

    inquire(file = FIFO_PIPE, exist = pipe_exists)

    if (pipe_exists) then
      error stop "[Directory] Error: Never closed the FIFO!"
    end if



    ! call get_command(command = message)

    ! wait(id = id_var)

    ! print*,message
  end subroutine read_directory


end module directory
