module fast_pack
  use :: memory_texture_module
  use, intrinsic :: iso_c_binding
  implicit none


  private


  type :: texture_rectangle
    real(c_float) :: min_x = 0.0
    real(c_float) :: min_y = 0.0
    real(c_float) :: max_x = 0.0
    real(c_float) :: max_y = 0.0
  end type texture_rectangle


  type :: texture_packer_config
    logical(c_bool) :: fast_canvas_export
    integer(c_int) :: padding
    type(pixel) :: edge_color
    type(pixel) :: blank_color
    integer(c_int) :: canvas_expansion_amount = 100
    logical(c_bool) :: debug_edge = .false.
    integer(c_int) :: width = 400
    integer(c_int) :: height = 400
  end type texture_packer_config


contains


end module fast_pack
