module texture
  implicit none


  private


  public :: test_stbi


contains

  subroutine test_stbi()
    use :: stb_image
    use :: string
    use :: iso_c_binding
    implicit none

    integer :: x, y, channels, desired_channels
    character(len = :, kind = c_char), allocatable :: file_name

    file_name = into_c_string("./textures/rgba_test.png")

    call stbi_load(file_name, x, y, channels, desired_channels)

    print*,x,y,channels,desired_channels

  end subroutine test_stbi




end module texture
