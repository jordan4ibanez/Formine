module h_string
  implicit none


  private


  public :: heap_string


  !* A heap string is a container to allow strings to be put into arrays dynamically.
  !* The internal data must be gotten with %get() to prevent UB.
  type :: heap_string
    private
    character(len = :), allocatable :: data
  contains
    !? Assignment.
    generic :: assignment(=) => assign
    procedure :: assign
    !? Equality check.
    generic :: operator(==) => equal_heap_string, equal_raw_string
    procedure :: equal_heap_string
    procedure :: equal_raw_string
    !? Print formatting.
    generic :: write(formatted) => write_formatted
    procedure :: write_formatted
    !? Allocated check.
    procedure :: is_allocated
    !? Get internal data.
    procedure :: get



    !? Cut ALL instances of a substring out of a string.
    procedure :: cut_all
    !? Check if a string contains a substring.
    procedure :: contains
    !? Get the file name out of a system path.
    !? If the string isn't a system path, you'll get a nice warning and a blank string.
    procedure :: get_file_name
  end type heap_string


  !* heap_string constructor.
  interface heap_string
    module procedure :: constructor
  end interface heap_string


contains


  !* heap_string constructor.
  function constructor(optional_string) result(new_str)
    implicit none

    character(len = *), intent(in), optional :: optional_string
    type(heap_string) :: new_str

    if (present(optional_string)) then
      new_str%data = optional_string
    end if
  end function constructor


  !* Very simple assignment operator.
  subroutine assign(this, new_data)
    implicit none

    class(heap_string), intent(inout) :: this
    character(len = *), intent(in) :: new_data

    this%data = new_data
  end subroutine assign


  !* Equality check with another heap string.
  logical function equal_heap_string(this, other) result(res)
    implicit none

    class(heap_string), intent(in) :: this
    type(heap_string), intent(in) :: other

    res = this%data == other%data .and. len(this%data) == len(other%data)
  end function equal_heap_string


  !* Equality check with a raw string.
  logical function equal_raw_string(this, other) result(res)
    implicit none

    class(heap_string), intent(in) :: this
    character(len = *), intent(in) :: other

    res = this%data == other .and. len(this%data) == len(other)
  end function equal_raw_string


  !* Simply allows you to use a heap string like a regular string.
  subroutine write_formatted(this, unit, iotype, v_list, iostat, iomsg)
    implicit none

    class(heap_string), intent(in) :: this
    integer, intent(in) :: unit         ! Internal unit to write to.
    character(*), intent(in) :: iotype  ! LISTDIRECTED or DTxxx
    integer, intent(in) :: v_list(:)    ! parameters from fmt spec.
    integer, intent(out) :: iostat      ! non zero on error, etc.
    character(*), intent(inout) :: iomsg  ! define if iostat non zero.

    if (.false.) then
      print*,iotype, v_list
    end if

    write (unit,"(A)", iostat = iostat, iomsg = iomsg) this%data
  end subroutine write_formatted


  !* Very simple check to see if the internal data is allocated.
  logical function is_allocated(this) result(res)
    implicit none

    class(heap_string), intent(inout) :: this

    res = allocated(this%data)
  end function is_allocated


  !* Get the internal data of the heap string.
  !? Aka, what it is pointing to on the heap basically.
  function get(this) result(data)
    implicit none

    class(heap_string), intent(inout) :: this
    character(len = :), allocatable :: data

    data = this%data
  end function get


  !* Cut all instances of a substring out of a string.
  subroutine cut_all(this, substring)
    implicit none

    class(heap_string), intent(inout) :: this
    character(len = *), intent(in) :: substring
    character(len = :), allocatable :: old

    ! Assign old to current as a base.
    old = this%data

    ! Then, we simply repeatedly cut until it's the same, that's about it.
    do while(.true.)

      !? If you want to see this happen in real time, turn this on. It's neat. :)
      ! print*,old

      ! call this%cut(substring)

      ! No more changes happened. Exit loop.
      if (this == old) then
        exit
      end if

      ! A change happened, save state and loop again.
      old = this%data
    end do
  end subroutine cut_all


  !* Check if a string contains a substring.
  logical function contains(this, substring) result(does_contain)
    implicit none

    class(heap_string), intent(inout) :: this
    character(len = *), intent(in) :: substring

    !? If the index is 0, this means it does not contain the substring.
    !? So we invert the logic.
    does_contain = index(this%data, substring) /= 0
  end function contains


  !* Get a file name string from a path.
  function get_file_name(this) result(resulting_name_of_file)
    use, intrinsic :: iso_c_binding, only: c_char
    implicit none

    class(heap_string), intent(in) :: this
    character(len = :, kind = c_char), allocatable :: resulting_name_of_file
    integer :: i
    integer :: length_of_string

    i = index(this%data, "/", back = .true.)

    ! This probably isn't a path.
    if (i == 0) then
      print"(A)", achar(27)//"[38;2;255;128;0m[Heap String] Warning: Could not extract file name from directory."//achar(27)//"[m"
      resulting_name_of_file = ""
      return
    end if

    length_of_string = len(this%data)

    ! This is a folder.
    if (i == length_of_string) then
      print"(A)", achar(27)//"[38;2;255;128;0m[Heap String] Warning: Tried to get file name of folder."//achar(27)//"[m"
      resulting_name_of_file = ""
      return
    end if

    ! So this is a file. Let's now get it
    resulting_name_of_file = this%data(i + 1:length_of_string)
  end function get_file_name


end module h_string
