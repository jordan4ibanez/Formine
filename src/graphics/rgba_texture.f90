module rgba8_texture_mod
  use, intrinsic :: iso_c_binding
  implicit none


  private


  public :: rgba8
  public :: rgba8_texture


  !* This is a single pixel.
  !* This is commonly called: RGBA_8
  !* In the range of 0-255.
  type :: rgba8
    integer(c_int) :: r = 0
    integer(c_int) :: g = 0
    integer(c_int) :: b = 0
    integer(c_int) :: a = 0
  end type rgba8


  !* This is an actual texture
  !* It contains pixels in the pixels component.
  !* In the standard of: RGBA_8
  type :: rgba8_texture
    type(rgba8), dimension(:), allocatable :: pixels
    integer(c_int) :: width
    integer(c_int) :: height
  end type rgba8_texture


contains



end module rgba8_texture_mod
