module crashify
  implicit none


  private


  public :: crashy


contains


  logical function oops() result(res)
    implicit none

    res = .true.
    ! Try to print while returning to print.
    print*,res
  end function oops


  subroutine crashy
    ! print in a print frame. Hangs.
    print*,oops()
  end subroutine crashy


end module crashify
