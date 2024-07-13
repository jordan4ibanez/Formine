!** To keep file io synchronous, only use this library to work with files.
module files
  implicit none


  private


  public :: file_reader


  !** This is your basic (file -> allocated string) reader. I think it's pretty neat. :)
  type :: file_reader

    logical :: exists
    character(len = :), allocatable :: file_string

  contains

    procedure :: read_file => file_reader_read_file

  end type file_reader


contains


  !** Open a file, read it into a string, close the file, return the string.
  !! result%file_string is allocated, remember to call deallocate!
  subroutine file_reader_read_file(this, file_location)
    implicit none

    class(file_reader) :: this
    character(len = *) :: file_location
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

    else

      print"(A)","[files] Error: file_location "//file_location//" does not exist."

    end if

    ! Now we must close it so there is not an IO leak.
    close(file_io_identifier)
  end subroutine file_reader_read_file


end module files
