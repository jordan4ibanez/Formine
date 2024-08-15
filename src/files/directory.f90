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
    integer :: status

    ! We are literally creating a custom pipe on posix to act as a virtual file.
    !! DO NOT use this for high performance IO. I have no idea how fast this actually is lol.
    !! DO NOT use this for asynchronous IO!
    ! You can thank:
    ! janneb https://stackoverflow.com/a/53450193
    ! Alexander Vogt: https://stackoverflow.com/a/18671521

    !! This needs a windows check and a windows implementation. You can make named pipes on windows.

    ! todo: turn this into a module.
    ! Make sure we deleted this pipe last time.
    inquire(file = FIFO_PIPE, exist = pipe_exists)
    if (pipe_exists) then
      print"(A)", "[Directory] warning: FIFO was never closed, deleting."
      open(unit = 7023, iostat = status, file = FIFO_PIPE, status = "old")
      if (status == 0) then
        close(7023, status = "delete")
      else
        error stop "[Directory] Error: Failed to delete outdated FIFO."
      end if
    end if

    call execute_command_line("mkfifo "//FIFO_PIPE)

    inquire(file = FIFO_PIPE, exist = pipe_exists)
    if (.not. pipe_exists) then
      error stop "[Directory] Error: Failed to create the FIFO."
    end if


    call execute_command_line("ls -1 "//path//" > "//FIFO_PIPE, wait = .true.)



    ! call get_command(command = message)

    ! wait(id = id_var)

    ! print*,message
  end subroutine read_directory


end module directory
