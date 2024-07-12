! This was translated from a C++ thing I wrote for kotek.
module orientation_enum
  implicit none

  integer, parameter :: orient_up = 0
  integer, parameter :: orient_right = 1
  integer, parameter :: orient_down = 2
  integer, parameter :: orient_left = 3
  integer, parameter :: orient_none = 13

  public :: check_orient

contains

  function orientation_to_string(input) result(output)
    use string
    implicit none

    integer, intent(in) :: input
    character(len=:), allocatable :: output

    select case (input)
     case (orient_up)
      output = "UP"
     case (orient_right)
      output = "UP"
     case (orient_down)
      output = "UP"
     case (orient_left)
      output = "UP"
     case (orient_none)
      output = "UP"
     case default
      error stop "illegal argument: ["//int_to_string(input)//"]"
    end select
  end function orientation_to_string

  subroutine check_orient(input)
    use string
    implicit none

    integer, intent(in) :: input

    select case(input)
     case(orient_up)
     case(orient_down)
     case(orient_left)
     case(orient_right)
     case(orient_none)
     case default
      error stop "illegal argument: ["//int_to_string(input)//"]"
    end select
  end subroutine check_orient
end module orientation_enum


module orient_class
  use orientation_enum
  implicit none

  private

  public :: orientation


  type orientation
    private
    integer :: value

  contains
    ! Subroutine overload.
    generic,public :: assignment(=) => assign_orientation,assign_integer
    procedure :: assign_orientation
    procedure :: assign_integer
    ! Getter
    procedure :: get
  end type orientation

  public :: new_orientation

contains

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


  integer function get(this) result(res)
    implicit none

    class(orientation), intent(inout) :: this

    res = this%value
  end function get

  type(orientation) function new_orientation(optional_initial_value) result(new_instance)
    use orientation_enum
    implicit none

    integer, intent(in), optional :: optional_initial_value
    integer :: value

    ! Make sure we're not getting garbage data.
    if (present(optional_initial_value)) then
      call check_orient(optional_initial_value)
      value = optional_initial_value
    else
      value = orient_none
    end if

    ! Now construct it.
    new_instance = orientation(value)
  end function new_orientation




end module orient_class
