! Deal is short for deallocators. :D
module deal
  implicit none

  private

  logical :: debug_mode = .true.

  public :: deallocate_string

contains

  ! I have no idea how to use generics yet so good luck to me!

  subroutine deallocate_string(string)
    implicit none
    character(:), allocatable :: string

    if (allocated(string)) then
      if (debug_mode) then
        print*,"[Deallocating]: "//string
      end if
      deallocate(string)
    else if (debug_mode) then
      print*,"[Deallocating]: <String already deallocated>"
    end if

  end subroutine deallocate_string

end module deal
