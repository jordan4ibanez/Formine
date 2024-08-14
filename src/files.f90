!* To keep file io synchronous, only use this library to work with files.
module files
  use :: string
  implicit none


  private


  public :: file_reader


  !* This is your basic (file -> allocated string) reader. I think it's pretty neat. :)
  type :: file_reader
    logical :: exists
    ! Straight shot component.
    character(len = :), allocatable :: file_string
    ! By line components.
    type(heap_string), dimension(:), allocatable :: lines
    integer :: line_count = 0
  contains
    procedure :: read_file => file_reader_read_file
    procedure :: read_lines => file_reader_read_file_into_lines
  end type file_reader


  type :: directory_reader

  end type directory_reader


contains


  !* Open a file, read it into a string, close the file, return the string.
  subroutine file_reader_read_file(this, file_path)
    implicit none

    class(file_reader), intent(inout) :: this
    character(len = *), intent(in) :: file_path
    integer :: file_io_identifier
    integer :: file_size

    ! First we check if the file exists.
    inquire(file = file_path, exist = this%exists, size = file_size)

    ! If the file does not exist, we're not going to attempt to allocate anything.
    if (this%exists) then

      ! We want readonly access and streaming of the data into a string.
      open(newunit = file_io_identifier, file = file_path, status = "old", action = "read", access = "stream")

      ! Now allocate the size of the string.
      allocate(character(len=file_size) :: this%file_string)

      ! And finally stream it into the string.
      read(file_io_identifier) this%file_string

      ! Now we must close it so there is not an IO leak.
      close(file_io_identifier)
    else

      print"(A)","[Files] Error: File path ["//file_path//"] does not exist."
    end if
  end subroutine file_reader_read_file


  !* Read a file into an array of heap_strings.
  subroutine file_reader_read_file_into_lines(this, file_path)
    implicit none

    class(file_reader), intent(inout) :: this
    character(len = *), intent(in) :: file_path
    !! This is testing debugging
    character(len = :), allocatable :: temporary_container
    integer :: found_newline_index
    integer :: length_of_buffer

    ! I can't figure out how to make the io operation read line by line so we're going to
    ! use the internal file_string component as a temp buffer.

    ! Push the entire string buffer into this.
    call this%read_file(file_path)

    ! If the file does not exist, we're not going to attempt to do anything.
    if (.not. this%exists) then
      return
    end if

    ! Start off with nothing.
    allocate(this%lines(0))

    ! This should literally be unable to get stuck in an infinite loop.
    do while(.true.)

      ! Sniff out that \n.
      found_newline_index = index(this%file_string, achar(10))

      if (found_newline_index == 0) then
        ! When we reached the end with no \n, we need specific handling of this.
        ! Basically, just dump the final line in.

        ! Tick up the number of lines.
        this%line_count = this%line_count + 1
        ! Dump it in.
        this%lines = [this%lines, heap_string(this%file_string)]
        ! And remove residual memory.
        deallocate(this%file_string)
        exit
      else
        ! Tick up the number of lines.
        this%line_count = this%line_count + 1
        ! We're just going to continuously roll a bigger array with new elements.
        temporary_container = this%file_string(1:found_newline_index - 1)
        ! Append it.
        this%lines = [this%lines, heap_string(temporary_container)]
        ! Find the new total length of the string buffer.
        length_of_buffer = len(this%file_string)
        ! Step it over the \n and cut out the beginning.
        this%file_string = this%file_string(found_newline_index + 1:length_of_buffer)
      end if
    end do
  end subroutine file_reader_read_file_into_lines

end module files
