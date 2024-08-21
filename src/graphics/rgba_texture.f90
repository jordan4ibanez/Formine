module rgba8_texture_mod
  use, intrinsic :: iso_c_binding
  implicit none


  private


  public :: rgba8_pixel
  public :: rgba8_texture


  !* This is a single pixel.
  !* This is commonly called: RGBA_8
  !* In the range of 0-255.
  type :: rgba8_pixel
    integer(c_int) :: r = 0
    integer(c_int) :: g = 0
    integer(c_int) :: b = 0
    integer(c_int) :: a = 0
  end type rgba8_pixel


  interface rgba8_pixel
    module procedure :: rgba8_pixel_constructor
  end interface rgba8_pixel


  !* This is an actual texture
  !* It contains pixels in the pixels component.
  !* In the standard of: RGBA_8
  type :: rgba8_texture
    type(rgba8_pixel), dimension(:), allocatable :: pixels
    integer(c_int) :: width
    integer(c_int) :: height
  end type rgba8_texture


  interface rgba8_texture
    module procedure :: rgba8_texture_constructor
  end interface rgba8_texture


contains


  function rgba8_pixel_constructor(r, g, b, a) result(new_pixel)
    implicit none

    integer(c_int), intent(in), value :: r, g, b, a
    type(rgba8_pixel) :: new_pixel


  end function rgba8_pixel_constructor



  function rgba8_texture_constructor(r, g, b, a) result(new_pixel)
    implicit none

    integer(c_int), intent(in), value :: r, g, b, a
    type(rgba8_pixel) :: new_pixel


  end function rgba8_texture_constructor


end module rgba8_texture_mod
