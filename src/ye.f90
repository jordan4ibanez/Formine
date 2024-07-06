module ye
  implicit none

  private

  logical :: up = .true.

  public :: blah

contains
  subroutine blah(input)
    implicit none
    real :: input

    if (up) then
      input = input + 0.01
      if (input >= 1.0) then
        input = 1.0
        up = .false.
      end if
    else
      input = input - 0.01
      if (input <= 0.0) then
        input = 0.0
        up = .true.
      end if
    end if
  end subroutine
end module ye
