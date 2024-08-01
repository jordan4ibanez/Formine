module texture
  implicit none


  private


  public :: texture_create


contains

  subroutine texture_create()
    use :: stb_image
    use :: string
    use :: opengl
    use :: iso_c_binding
    implicit none

    integer :: x, y, channels, desired_channels
    character(len = :, kind = c_char), allocatable :: file_name
    integer(1), dimension(:), allocatable :: image_data

    file_name = into_c_string("./textures/rgba_test.png")

    ! We always want 4 channels.
    desired_channels = 4

    image_data = stbi_load(file_name, x, y, channels, desired_channels)

    if (x + y + channels == 0) then
      error stop "[Texture] Error: Could not load texture. It does not exist."
    end if

    ! print*,x,y,channels,desired_channels

  end subroutine texture_create




end module texture
