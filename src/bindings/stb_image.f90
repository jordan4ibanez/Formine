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

      character(kind = c_char), intent(in) :: file_name
      integer(c_int), intent(inout) :: x, y, channels_in_file
      integer(c_int), intent(in), value :: desired_channels
      type(c_ptr) :: raw_data
    end function internal_stbi_load


  end interface

contains

  subroutine stbi_load(file_name, x, y, channels_in_file, desired_channels)
    use :: math_helpers, only: c_uchar_to_int_array
    implicit none

    character(kind = c_char), intent(in) :: file_name
    integer(c_int), intent(inout) :: x, y, channels_in_file
    integer(c_int), intent(in), value :: desired_channels
    type(c_ptr) :: raw_data
    integer :: array_length
    integer(1), dimension(:), pointer :: passed_data_pointer
    integer(1), dimension(:), allocatable :: intermidiate_data_byte
    integer(c_int), dimension(:), allocatable :: output_data_int

    raw_data = internal_stbi_load(file_name, x, y, channels_in_file, desired_channels)

    array_length = x * y * channels_in_file

    call c_f_pointer(raw_data, passed_data_pointer, shape = [array_length])

    print*,passed_data_pointer

    allocate(intermidiate_data_byte(array_length))

    intermidiate_data_byte = passed_data_pointer

    output_data_int = c_uchar_to_int_array(intermidiate_data_byte)

    print*,output_data_int




  end subroutine stbi_load


end module stb_image
