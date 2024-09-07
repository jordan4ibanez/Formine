module fast_pack_types
  use, intrinsic :: iso_c_binding
  implicit none


  !* A texture's position in the atlas.
  type :: texture_rectangle
    real(c_float) :: min_x = 0.0
    real(c_float) :: min_y = 0.0
    real(c_float) :: max_x = 0.0
    real(c_float) :: max_y = 0.0
  end type texture_rectangle


end module fast_pack_types
