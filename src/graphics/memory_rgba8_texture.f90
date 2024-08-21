module memory_rgba8_texture
  use, intrinsic :: iso_c_binding
  implicit none

  ! This is an abstraction over the raw array components to allow me to actually
  ! read what I'm doing during the subroutine that checks how wide characters are.
  type :: rgba8
    integer(c_int) :: r = 0
    integer(c_int) :: g = 0
    integer(c_int) :: b = 0
    integer(c_int) :: a = 0
  end type rgba8

end module memory_rgba8_texture
