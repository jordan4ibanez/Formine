module font
  use, intrinsic :: iso_c_binding
  implicit none

  !* I am a solo developer. I only use 1 font.
  !* In the future, if there is a desire for more than one font,
  !* we will build upon this.

  private


  public :: font_prototyping

  integer, parameter :: character_width = 5
  integer, parameter :: character_height = 7
  integer, parameter :: spacing = 1

  ! Slots are the total size of a character, including the border.
  integer, parameter :: slot_width = character_width + spacing ! 6
  integer, parameter :: slot_height = character_height + spacing ! 8

  integer, parameter :: characters_horizontal = 9
  integer, parameter :: characters_vertical = 9


contains


  subroutine font_prototyping(font_texture_location)
    use :: stb_image
    use :: string
    implicit none

    character(len = *, kind = c_char), intent(in) :: font_texture_location
    integer :: x, y, channels, desired_channels
    character(len = :, kind = c_char), allocatable :: c_file_location

    c_file_location = into_c_string(font_texture_location)

    ! We always want 4 channels.
    desired_channels = 4


  end subroutine font_prototyping


end module font
