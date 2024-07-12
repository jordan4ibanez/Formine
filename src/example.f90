! This was translated from a C++ thing I wrote for kotek.
! I converted it into one module.
module orient_class
  implicit none


  private


  public :: ORIENT_UP
  public :: ORIENT_RIGHT
  public :: ORIENT_DOWN
  public :: ORIENT_LEFT
  public :: ORIENT_NONE

  integer, parameter :: ORIENT_UP = 0
  integer, parameter :: ORIENT_RIGHT = 1
  integer, parameter :: ORIENT_DOWN = 2
  integer, parameter :: ORIENT_LEFT = 3
  integer, parameter :: ORIENT_NONE = 13


  public :: orientation


  type orientation
    private
    ! Encapsulate this.
    integer :: value

  contains

    !** All these are public.
    ! Subroutine overload. Can assign from type(orientation) or integer.
    generic :: assignment(=) => assign_orientation,assign_integer
    procedure :: assign_orientation
    procedure :: assign_integer
    ! Function overload. Can equal against type(orientation) or integer.
    generic :: operator(==) => equal_orientation
    procedure :: equal_orientation
    ! Getter
    procedure :: get
    procedure :: get_string
  end type orientation


  public :: new_orientation


contains


  ! Convert an orientation to a string.
  function orientation_to_string(input) result(output)
    use string
    implicit none

    integer, intent(in) :: input
    character(len=:), allocatable :: output

    select case (input)
     case (ORIENT_UP)
      output = "UP"
     case (ORIENT_RIGHT)
      output = "UP"
     case (ORIENT_DOWN)
      output = "UP"
     case (ORIENT_LEFT)
      output = "UP"
     case (ORIENT_NONE)
      output = "UP"
     case default
      error stop "illegal argument: ["//int_to_string(input)//"]"
    end select
  end function orientation_to_string


! Subroutine to ensure correctness.
  subroutine check_orient(input)
    use string
    implicit none

    integer, intent(in) :: input

    select case(input)
     case(ORIENT_UP)
     case(ORIENT_DOWN)
     case(ORIENT_LEFT)
     case(ORIENT_RIGHT)
     case(ORIENT_NONE)
     case default
      error stop "illegal argument: ["//int_to_string(input)//"]"
    end select
  end subroutine check_orient


  ! Assign from another orientation.
  subroutine assign_orientation(this, other)
    implicit none

    class(orientation), intent(inout) :: this
    type(orientation), intent(in) :: other

    this%value = other%value
  end subroutine assign_orientation


  ! Assign from an integer.
  subroutine assign_integer(this, other)
    implicit none

    class(orientation), intent(inout) :: this
    integer, intent(in) :: other

    call check_orient(other)
    this%value = other
  end subroutine assign_integer


  ! Test equality to another orientation.
  logical function equal_orientation(this, other) result(res)
    implicit none

    class(orientation), intent(in) :: this
    type(orientation), intent(in) :: other

    res = this%value == other%value
  end function equal_orientation


  ! Test equality to an integer.
  logical function equal_integer(this, other) result(res)
    implicit none

    class(orientation), intent(in) :: this
    integer, intent(in) :: other

    res = this%value == other
  end function equal_integer


  ! Get raw value.
  integer function get(this) result(res)
    implicit none

    class(orientation), intent(inout) :: this

    res = this%value
  end function get


  ! Get value as string.
  function get_string(this) result(res)
    implicit none

    class(orientation), intent(inout) :: this
    character(len = :), allocatable :: res

    res = orientation_to_string(this%value)
  end function get_string


  ! Constructor.
  type(orientation) function new_orientation(optional_initial_value) result(new_instance)
    implicit none

    integer, intent(in), optional :: optional_initial_value
    integer :: value

    ! Make sure we're not getting garbage data.
    if (present(optional_initial_value)) then
      call check_orient(optional_initial_value)
      value = optional_initial_value
    else
      value = ORIENT_NONE
    end if

    ! Now construct it.
    new_instance = orientation(value)
  end function new_orientation


end module orient_class

module test_my_garbage
  implicit none


  private


  public :: test_this_thing


contains


  ! We can finally test this thing.
  subroutine test_this_thing
    use orient_class
    implicit none

    type(orientation) :: blah

    blah = new_orientation(ORIENT_UP)

    ! print*,blah

    ! This stops the program.
    ! test = 5

  end subroutine test_this_thing


end module test_my_garbage
