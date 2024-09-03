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
  function pixel_constructor(r, g, b, a) result(pixel_new)
    use :: string
    implicit none

    integer(c_int), intent(in), value :: r, g, b, a
    type(pixel) :: pixel_new

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

    pixel_new%r = r
    pixel_new%g = g
    pixel_new%b = b
    pixel_new%a = a
  end function pixel_constructor


  !* Constructor for raw rgba8 data.
  function rgba8_texture_constructor(raw_texture_memory_u8, width, height) result(rgba_texture_new)
    use :: string
    use :: math_helpers
    implicit none

    integer(1), dimension(:) :: raw_texture_memory_u8
    integer(c_int), intent(in), value :: width, height
    type(memory_texture) :: rgba_texture_new
    integer(c_int) :: array_length, pixel_array_length, current_index, y, x
    integer(c_int), dimension(:), allocatable :: raw_texture_memory_i32

    array_length = size(raw_texture_memory_u8)
    ! 4 channels per pixel.
    pixel_array_length = array_length / 4

    if (width * height /= pixel_array_length) then
      error stop "[RGBA Texture] Error: Received raw texture memory with assumed width ["//int_to_string(width)//"] | height ["//int_to_string(height)//"]. Assumed size is wrong."
    end if

    rgba_texture_new%width = width
    rgba_texture_new%height = height

    ! Shift this into a format we can use.
    raw_texture_memory_i32 = c_uchar_to_int_array(raw_texture_memory_u8)

    ! Allocate the array.
    allocate(rgba_texture_new%pixels(width, height))

    current_index = 1

    do y = 1,height
      do x = 1,width
        ! Now we create the pixel.
        rgba_texture_new%pixels(x, y) = pixel( &
          raw_texture_memory_i32(current_index), &
          raw_texture_memory_i32(current_index + 1), &
          raw_texture_memory_i32(current_index + 2), &
          raw_texture_memory_i32(current_index + 3) &
          )

        current_index = current_index + 4
      end do
    end do
  end function rgba8_texture_constructor


  !* Constructor for a blank memory texture with a size.
  function blank_memory_texture_constructor(width, height) result(rgba_texture_new)
    implicit none

    integer(c_int), intent(in), value :: width, height
    type(memory_texture) :: rgba_texture_new
    integer(c_int) :: pixel_array_length

    pixel_array_length = width * height

    rgba_texture_new%width = width
    rgba_texture_new%height = height

    ! Pretty simple.
    allocate(rgba_texture_new%pixels(width, height))
  end function blank_memory_texture_constructor


  !* This wraps a chain of functions to just get the data we need, which is RGBA of a pixel.
  function memory_texture_get_pixel(this, x, y) result(color)
    use :: string
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
  subroutine memory_texture_set_pixel(this, x, y, pixel_new)
    implicit none

    class(memory_texture), intent(inout) :: this
    integer(c_int), intent(in), value :: x, y
    type(pixel), intent(in) :: pixel_new

    this%pixels(x, y) = pixel_new
  end subroutine memory_texture_set_pixel


  !* Will extract the workable pixel data into a uint8 array which should never be modified.
  function memory_texture_get_raw_data(this) result(raw_texture_data_new)
    use :: math_helpers, only: int_to_c_uchar_array
    implicit none

    class(memory_texture), intent(in) :: this
    integer(c_int), dimension(:), allocatable :: temporary_integer_data
    integer(1), dimension(:), allocatable :: raw_texture_data_new
    integer(c_int) :: raw_size, current_raw_index, y, x
    type(pixel) :: current_pixel

    raw_size = this%width * this%height * 4

    allocate(temporary_integer_data(raw_size))

    current_raw_index = 1

    ! Collect all the pixels into the temporary array.
    do y = 1,this%height
      do x = 1,this%width
        current_pixel = this%pixels(x, y)

        temporary_integer_data(current_raw_index) = current_pixel%r
        temporary_integer_data(current_raw_index + 1) = current_pixel%g
        temporary_integer_data(current_raw_index + 2) = current_pixel%b
        temporary_integer_data(current_raw_index + 3) = current_pixel%a

        current_raw_index = current_raw_index + 4
      end do
    end do

    ! Finally, transmute it into uint8_t.
    raw_texture_data_new = int_to_c_uchar_array(temporary_integer_data)
  end function memory_texture_get_raw_data


  !* Position (in pixels) to the index in the texture array.
  function memory_texture_internal_position_to_index(this, x, y) result(i)
    implicit none

    class(memory_texture), intent(in) :: this
    integer(c_int), intent(in), value :: x, y
    integer(c_int) :: a, b
    integer(c_int) :: i

    ! -1 because: We're shifting from indices into offsets.
    a = x - 1
    b = y - 1

    ! +1 because: We're shifting from offsets into indices.
    i = ((b * this%width) + a) + 1
  end function memory_texture_internal_position_to_index


end module memory_texture_module
