module rez
  implicit none

  private

  public :: res
  public result_wrap

  type res
    class(*), allocatable :: data
    logical :: exists
  end type res

contains

  type(res) function result_wrap(input) result(result)

    class(*), allocatable :: input

    if (allocated(input)) then
      result%exists = .true.
      result%data = input
    end if
  end function result_wrap

end module rez
