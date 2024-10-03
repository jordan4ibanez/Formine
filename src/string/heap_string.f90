module t_heap_string_mod
  use, intrinsic :: iso_c_binding
  implicit none


  private


  public :: heap_string


  !* A heap string is a container to allow strings to be put into arrays dynamically.
  type :: heap_string
    character(len = :, kind = c_char), allocatable :: string
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
    !? Get pointer to internal data.
    procedure :: get_pointer
  end type heap_string



contains

  !* Very simple assignment operator.
  subroutine assign(this, new_data)
    implicit none

    class(heap_string), intent(inout) :: this
    character(len = *, kind = c_char), intent(in) :: new_data

    this%string = new_data
  end subroutine assign


  !* Equality check with another heap string.
  logical function equal_heap_string(this, other) result(res)
    implicit none

    class(heap_string), intent(in) :: this
    type(heap_string), intent(in) :: other

    res = this%string == other%string .and. len(this%string) == len(other%string)
  end function equal_heap_string


  !* Equality check with a raw string.
  logical function equal_raw_string(this, other) result(res)
    implicit none

    class(heap_string), intent(in) :: this
    character(len = *, kind = c_char), intent(in) :: other

    res = this%string == other .and. len(this%string) == len(other)
  end function equal_raw_string


  !* Simply allows you to use a heap string like a regular string.
  subroutine write_formatted(this, unit, iotype, v_list, iostat, iomsg)
    implicit none

    class(heap_string), intent(in) :: this
    integer, intent(in) :: unit         ! Internal unit to write to.
    character(len = *, kind = c_char), intent(in) :: iotype  ! LISTDIRECTED or DTxxx
    integer, intent(in) :: v_list(:)    ! parameters from fmt spec.
    integer, intent(out) :: iostat      ! non zero on error, etc.
    character(len = *, kind = c_char), intent(inout) :: iomsg  ! define if iostat non zero.

    if (.false.) then
      print*,iotype, v_list
    end if

    write (unit,"(A)", iostat = iostat, iomsg = iomsg) this%string
  end subroutine write_formatted


  !* Very simple check to see if the internal data is allocated.
  logical function is_allocated(this) result(res)
    implicit none

    class(heap_string), intent(inout) :: this

    res = allocated(this%string)
  end function is_allocated


  !* Get the pointer to the internal data of the heap string.
  function get_pointer(this) result(data_pointer)
    implicit none

    class(heap_string), intent(in), target :: this
    character(len = :), pointer :: data_pointer

    data_pointer => this%string
  end function get_pointer


end module t_heap_string_mod
