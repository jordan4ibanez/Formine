module fast_pack
  use :: memory_texture_module
  use :: fhash, only: fhash_tbl_t, key => fhash_key
  use, intrinsic :: iso_c_binding
  implicit none


  private


  !* Represents a texture size.
  type :: texture_rectangle
    real(c_float) :: min_x = 0.0
    real(c_float) :: min_y = 0.0
    real(c_float) :: max_x = 0.0
    real(c_float) :: max_y = 0.0
  end type texture_rectangle


  !* Configure the fast packer.
  type :: fast_packer_config
    logical(c_bool) :: fast_canvas_export
    integer(c_int) :: padding
    type(pixel) :: edge_color
    type(pixel) :: blank_color
    integer(c_int) :: canvas_expansion_amount = 100
    logical(c_bool) :: debug_edge = .false.
    integer(c_int) :: width = 400
    integer(c_int) :: height = 400
  end type fast_packer_config


  !* The fast packer.
  type :: fast_packer
    integer(c_int) :: current_id = 1
    type(fast_packer_config) :: config
    type(fhash_tbl_t) :: keys
    integer(c_int) :: canvas_width = 0
    integer(c_int) :: canvas_height = 0

    !! Everything below this should be allocated in the constructor.
    integer(c_int), dimension(:), allocatable :: position_x
    integer(c_int), dimension(:), allocatable :: position_y
    integer(c_int), dimension(:), allocatable :: box_width
    integer(c_int), dimension(:), allocatable :: box_height
    type(memory_texture), dimension(:), allocatable :: textures
    integer(c_int), dimension(:), allocatable :: available_x ! [0]
    integer(c_int), dimension(:), allocatable :: available_y ! [0]
  end type fast_packer

contains


end module fast_pack
