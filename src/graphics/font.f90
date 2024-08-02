module font
  use, intrinsic :: iso_c_binding
  implicit none


  private


  public :: font_prototyping

  integer, parameter :: character_width = 5
  integer, parameter :: character_height = 7
  integer, parameter :: spacing = 1


contains


  subroutine font_prototyping()
    use :: stb_image
    implicit none


  end subroutine font_prototyping


end module font
