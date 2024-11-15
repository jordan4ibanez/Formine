module memory_texture_module
  use, intrinsic :: iso_c_binding
  implicit none


  private

  public :: pixel
  public :: memory_texture


  !* This is a single pixel.
  !* This is commonly called: RGBA_8
  !* In the range of 0-255.
  type :: pixel
    integer(c_int) :: r = 0
    integer(c_int) :: g = 0
    integer(c_int) :: b = 0
    integer(c_int) :: a = 0
  end type pixel


  interface pixel
    module procedure :: pixel_constructor
  end interface pixel


  !* This is an actual texture
  !* It contains pixels in the pixels component.
  !* In the standard of: RGBA_8
  type :: memory_texture
    ! Layout: [ x, y ]
    type(pixel), dimension(:, :), allocatable :: pixels(:, :)
    integer(c_int) :: width
    integer(c_int) :: height
  contains
    procedure :: get_pixel => memory_texture_get_pixel
    procedure :: set_pixel => memory_texture_set_pixel
    procedure :: get_raw_data => memory_texture_get_raw_data
  end type memory_texture


  interface memory_texture
    module procedure :: rgba8_texture_constructor, blank_memory_texture_constructor
  end interface memory_texture


contains


  !* Constructor for a pixel.
  function pixel_constructor(r, g, b, a) result(new_pixel)
    use :: string_f90
    implicit none

    integer(c_int), intent(in), value :: r, g, b, a
    type(pixel) :: new_pixel

    ! Range checks for RGBA.
    if (r < 0 .or. r > 255) then
      error stop "[RGBA Texture] Error: Red is out of range. Range: [0-255]. Received: ["//int_to_string(r)//"]"
    end if
    if (g < 0 .or. g > 255) then
      error stop "[RGBA Texture] Error: Green is out of range. Range: [0-255]. Received: ["//int_to_string(g)//"]"
    end if
    if (b < 0 .or. b > 255) then
      error stop "[RGBA Texture] Error: Blue is out of range. Range: [0-255]. Received: ["//int_to_string(b)//"]"
    end if
    if (a < 0 .or. a > 255) then
      error stop "[RGBA Texture] Error: Alpha is out of range. Range: [0-255]. Received: ["//int_to_string(a)//"]"
    end if

    new_pixel%r = r
    new_pixel%g = g
    new_pixel%b = b
    new_pixel%a = a
  end function pixel_constructor


  !* Constructor for raw rgba8 data.
  function rgba8_texture_constructor(raw_texture_memory_u8, width, height) result(new_rgba_texture)
    use :: string_f90
    use :: math_helpers
    implicit none

    integer(1), dimension(:) :: raw_texture_memory_u8
    integer(c_int), intent(in), value :: width, height
    type(memory_texture) :: new_rgba_texture
    integer(c_int) :: array_length, pixel_array_length, current_index, y, x
    integer(c_int), dimension(:), allocatable :: raw_texture_memory_i32

    array_length = size(raw_texture_memory_u8)
    ! 4 channels per pixel.
    pixel_array_length = array_length / 4

    if (width * height /= pixel_array_length) then
      error stop "[RGBA Texture] Error: Received raw texture memory with assumed width ["//int_to_string(width)//"] | height ["//int_to_string(height)//"]. Assumed size is wrong."
    end if

    new_rgba_texture%width = width
    new_rgba_texture%height = height

    ! Shift this into a format we can use.
    raw_texture_memory_i32 = c_uchar_to_int_array(raw_texture_memory_u8)

    ! Allocate the array.
    allocate(new_rgba_texture%pixels(width, height))

    current_index = 1

    do y = 1,height
      do x = 1,width
        ! Now we create the pixel.
        new_rgba_texture%pixels(x, y) = pixel( &
          raw_texture_memory_i32(current_index), &
          raw_texture_memory_i32(current_index + 1), &
          raw_texture_memory_i32(current_index + 2), &
          raw_texture_memory_i32(current_index + 3) &
          )

        ! 4 channels. RGBA.
        current_index = current_index + 4
      end do
    end do
  end function rgba8_texture_constructor


  !* Constructor for a blank memory texture with a size.
  function blank_memory_texture_constructor(width, height) result(new_rgba_texture)
    implicit none

    integer(c_int), intent(in), value :: width, height
    type(memory_texture) :: new_rgba_texture
    integer(c_int) :: pixel_array_length

    pixel_array_length = width * height

    new_rgba_texture%width = width
    new_rgba_texture%height = height

    ! Pretty simple.
    allocate(new_rgba_texture%pixels(width, height))
  end function blank_memory_texture_constructor


  !* This wraps a chain of functions to just get the data we need, which is RGBA of a pixel.
  function memory_texture_get_pixel(this, x, y) result(color)
    use :: string_f90
    implicit none

    class(memory_texture), intent(in) :: this
    integer(c_int), intent(in), value :: x, y
    type(pixel) :: color

    ! This is calculated via indices.
    if (x < 1 .or. x > this%width) then
      error stop "[RGBA Texture] Error: X is out of bounds ["//int_to_string(x)//"]"
    end if
    if (y < 1 .or. y > this%height) then
      error stop "[RGBA Texture] Error: Y is out of bounds ["//int_to_string(y)//"]"
    end if

    color = this%pixels(x, y)
  end function memory_texture_get_pixel


  !* Set the pixel of a texture.
  subroutine memory_texture_set_pixel(this, x, y, new_pixel)
    implicit none

    class(memory_texture), intent(inout) :: this
    integer(c_int), intent(in), value :: x, y
    type(pixel), intent(in) :: new_pixel

    this%pixels(x, y) = new_pixel
  end subroutine memory_texture_set_pixel


  !* Will extract the workable pixel data into a uint8 array which should never be modified.
  function memory_texture_get_raw_data(this) result(new_raw_texture_data)
    use :: math_helpers, only: int_to_c_uchar
    implicit none

    class(memory_texture), intent(in) :: this
    integer(1), dimension(:), allocatable :: new_raw_texture_data
    integer(c_int) :: raw_size, current_raw_index, y, x
    type(pixel) :: current_pixel

    ! 4 channels. RGBA.
    raw_size = this%width * this%height * 4

    allocate(new_raw_texture_data(raw_size))

    current_raw_index = 1

    ! Collect all the pixels into the temporary array.
    do y = 1,this%height
      do x = 1,this%width
        current_pixel = this%pixels(x, y)

        ! Transmute it into uint8_t.
        new_raw_texture_data(current_raw_index) = int_to_c_uchar(current_pixel%r)
        new_raw_texture_data(current_raw_index + 1) = int_to_c_uchar(current_pixel%g)
        new_raw_texture_data(current_raw_index + 2) = int_to_c_uchar(current_pixel%b)
        new_raw_texture_data(current_raw_index + 3) = int_to_c_uchar(current_pixel%a)

        ! 4 channels. RGBA.
        current_raw_index = current_raw_index + 4
      end do
    end do
  end function memory_texture_get_raw_data


end module memory_texture_module
