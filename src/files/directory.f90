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


contains

  !* This is an extreme hack job because I don't feel like writing libc bindings. :D
  subroutine read_directory(this, path)
    implicit none

    class(directory_reader), intent(inout) :: this
    character(len = *, kind = c_char), intent(in) :: path
    logical :: pipe_exists
    character(len = :, kind = c_char), allocatable :: message
    integer :: file_count, status, file_io_identifier, i, message_length

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
      open(unit = file_io_identifier, iostat = status, file = FIFO_PIPE, status = "old")
      if (status == 0) then
        close(file_io_identifier, status = "delete")
      else
        error stop "[Directory] Error: Failed to delete outdated FIFO."
      end if
    end if

    ! Create the FIFO pipe.
    call execute_command_line("mkfifo "//FIFO_PIPE)

    inquire(file = FIFO_PIPE, exist = pipe_exists)
    if (.not. pipe_exists) then
      error stop "[Directory] Error: Failed to create the FIFO."
    end if

    ! Open up this file reader so we can trigger bash to start dumping in raw data.
    open(file = FIFO_PIPE, newunit = file_io_identifier, access = "sequential", action = "readwrite")

    ! We're going to use some hackjobery to get the number of files before hand.
    call execute_command_line("ls -1 "//path//"| wc -l >"//FIFO_PIPE, wait = .true.)
    read(file_io_identifier, "(i8)") file_count

    ! Nothing to do.
    if (file_count == 0) then
      print*,"bailing out."
      close(file_io_identifier, status = "delete")
    end if

    ! Now we will overwrite this thing and loop for X amount of file names.
    call execute_command_line("ls -1 "//path//" > "//FIFO_PIPE, wait = .true.)

    ! This should probably be 4096 since that's the linux limit.
    allocate(character(256) :: message)

    ! Finally, allocate self with X amount of file names.
    allocate(this%files(file_count))

    ! Scan through each entry, and dump it into the persistent data.
    do i = 1,file_count
      read(file_io_identifier, "(A)") message
      message_length = index(message, " ") - 1
      this%files(i) = message(1:message_length)
    end do

    ! Finally, delete the FIFO pipe.
    close(file_io_identifier, status = "delete")

    print*,"[", this%files, "]"

  end subroutine read_directory


end module directory
