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

  function stbi_load(file_name, x, y, channels_in_file, desired_channels) result(raw_image_data)
    use :: math_helpers, only: c_uchar_to_int_array
    implicit none

    character(kind = c_char), intent(in) :: file_name
    integer(c_int), intent(inout) :: x, y, channels_in_file
    integer(c_int), intent(in), value :: desired_channels
    type(c_ptr) :: c_pointer
    integer :: array_length
    integer(1), dimension(:), pointer :: passed_data_pointer
    integer(1), dimension(:), allocatable :: raw_image_data
    ! integer(c_int), dimension(:), allocatable :: output_data_int

    !! WARNING: All data in the output is assumed to be overflowed, do not modify it.
    !! It is designed to be passed straight into C.

    ! Get the raw C data.
    c_pointer = internal_stbi_load(file_name, x, y, channels_in_file, desired_channels)

    ! Calculate the length of the array.
    array_length = x * y * channels_in_file

    ! Pass it into fortran.
    call c_f_pointer(c_pointer, passed_data_pointer, shape = [array_length])

    ! Initialize the raw image data with the raw pointer.
    raw_image_data = passed_data_pointer

    !? Enable this if you want to read the raw data
    ! output_data_int = c_uchar_to_int_array(intermidiate_data_byte)
    ! print*,output_data_int

    ! Free the Fortran pointer. (Just in case.)
    deallocate(passed_data_pointer)
  end function stbi_load


end module stb_image
