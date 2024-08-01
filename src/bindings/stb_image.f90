module stb_image
  use, intrinsic :: iso_c_binding
  implicit none


  private

  public :: stbi_load

  ! Here I'm binding to the C stb_image shared library.
  interface


    function internal_stbi_load(file_name, x, y, channels_in_file, desired_channels) result(raw_data) bind(c, name = "stbi_load")
      use, intrinsic :: iso_c_binding
      implicit none

      character(kind = c_char), intent(in), value :: file_name
      integer(c_int), intent(inout) :: x, y, channels_in_file
      integer(c_int), intent(in), value :: desired_channels
      type(c_ptr) :: raw_data
    end function internal_stbi_load


  end interface

contains

  subroutine stbi_load(file_name, x, y, channels_in_file, desired_channels)
    implicit none

    character(kind = c_char), intent(in), value :: file_name
    integer(c_int), intent(inout) :: x, y, channels_in_file
    integer(c_int), intent(in), value :: desired_channels
    type(c_ptr) :: raw_data

    raw_data = internal_stbi_load(file_name, x, y, channels_in_file, desired_channels)
    
  end subroutine stbi_load


end module stb_image
