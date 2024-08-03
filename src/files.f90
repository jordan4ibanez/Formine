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
    integer :: total_lines = 0
  contains
    procedure :: read_file => file_reader_read_file
    procedure :: read_lines => file_reader_read_file_into_lines
  end type file_reader


contains


  !* Open a file, read it into a string, close the file, return the string.
  subroutine file_reader_read_file(this, file_location)
    implicit none

    class(file_reader), intent(inout) :: this
    character(len = *), intent(in) :: file_location
    integer :: file_io_identifier
    integer :: file_size

    ! First we check if the file exists.
    inquire(file = file_location, exist = this%exists, size = file_size)

    ! If the file does not exist, we're not going to attempt to allocate anything.
    if (this%exists) then

      ! We want readonly access and streaming of the data into a string.
      open(newunit = file_io_identifier, file = file_location, status = "old", action = "read", access = "stream")

      ! Now allocate the size of the string.
      allocate(character(len=file_size) :: this%file_string)

      ! And finally stream it into the string.
      read(file_io_identifier) this%file_string

      ! Now we must close it so there is not an IO leak.
      close(file_io_identifier)
    else

      print"(A)","[files] Error: file_location "//file_location//" does not exist."
    end if
  end subroutine file_reader_read_file


  subroutine file_reader_read_file_into_lines(this, file_location)

    class(file_reader), intent(inout) :: this
    character(len = *), intent(in) :: file_location
    !! This is testing debugging
    character(len = :), allocatable :: temporary_container
    integer :: i
    integer :: found_newline_index

    ! I can't figure out how to make the io operation read line by line so we're going to
    ! use the internal file_string component as a temp buffer.

    ! Push the entire string buffer into this.
    call this%read_file(file_location)

    ! If the file does not exist, we're not going to attempt to do anything.
    if (this%exists) then

      ! Start off with nothing.
      allocate(this%lines(0))

      !! Testing safety net.
      do i = 1,10

        found_newline_index = index(this%file_string, achar(10))

        ! If we reached the end, we're done.
        if (found_newline_index == 0) then
          exit
        end if

        ! We're just going to continuously roll a bigger array with new elements.
        temporary_container = this%file_string(1:found_newline_index - 1)

        this%lines = [this%lines, heap_string(temporary_container)]

        print*,this%lines

      end do
    end if
  end subroutine file_reader_read_file_into_lines

end module files
