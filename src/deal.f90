module deal
  implicit none

  private

  public :: deallocate_string

contains

  ! I have no idea how to use generics yet so good luck to me!

  subroutine deallocate_string(string)
    implicit none
    character(:), allocatable :: string

    if (allocated(string)) then
      print*,"[Deallocating]: "//string
      deallocate(string)
    else
      print*,"[Deallocating]: <String already deallocated>"
    end if

  end subroutine deallocate_string

end module deal
