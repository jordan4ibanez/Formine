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
    logical(c_bool) :: fast_canvas_export = .true.
    integer(c_int) :: padding = 1
    type(pixel) :: edge_color
    type(pixel) :: blank_color
    integer(c_int) :: canvas_expansion_amount = 100
    logical(c_bool) :: debug_edge = .false.
    integer(c_int) :: width = 400
    integer(c_int) :: height = 400
  end type fast_packer_config


  !* The fast packer.
  type :: fast_packer
    private

    integer(c_int) :: current_id = 1
    logical(c_bool) :: fast_canvas_export = .true.
    integer(c_int) :: padding = 1
    type(pixel) :: edge_color
    type(pixel) :: blank_color
    integer(c_int) :: canvas_expansion_amount = 100
    logical(c_bool) :: debug_edge = .false.
    integer(c_int) :: new_canvas_width = 0
    integer(c_int) :: new_canvas_height = 0
    integer(c_int) :: canvas_width = 0
    integer(c_int) :: canvas_height = 0
    logical(c_bool) :: allocated = .false.
    ! Everything below this is allocated in the constructor.
    type(fhash_tbl_t) :: keys
    integer(c_int), dimension(:), allocatable :: position_x
    integer(c_int), dimension(:), allocatable :: position_y
    integer(c_int), dimension(:), allocatable :: box_width
    integer(c_int), dimension(:), allocatable :: box_height
    type(memory_texture), dimension(:), allocatable :: textures
    integer(c_int), dimension(:), allocatable :: available_x ! [0]
    integer(c_int), dimension(:), allocatable :: available_y ! [0]

  contains
    procedure :: pack => fast_packer_pack_from_file_path, fast_packer_pack_from_memory
    procedure, private :: internal_pack => fast_packer_internal_pack
    procedure, private :: tetris_pack => fast_packer_tetris_pack
    procedure, private :: update_canvas_size => fast_packer_update_canvas_size
  end type fast_packer


  interface fast_packer
    module procedure :: constructor_fast_packer
  end interface fast_packer


contains



  function constructor_fast_packer(config) result(new_fast_packer)
    implicit none

    type(fast_packer_config), intent(in) :: config
    type(fast_packer) :: new_fast_packer

    ! Assign from config.
    new_fast_packer%fast_canvas_export = config%fast_canvas_export
    new_fast_packer%padding = config%padding
    new_fast_packer%edge_color = config%edge_color
    new_fast_packer%blank_color = config%blank_color
    new_fast_packer%canvas_expansion_amount = config%canvas_expansion_amount
    new_fast_packer%debug_edge = config%debug_edge
    new_fast_packer%canvas_width = config%width
    new_fast_packer%canvas_height = config%height
    new_fast_packer%new_canvas_width = config%width
    new_fast_packer%new_canvas_height = config%height

    ! Allocate
    call new_fast_packer%keys%allocate()
    allocate(new_fast_packer%position_x(0))
    allocate(new_fast_packer%position_y(0))
    allocate(new_fast_packer%box_width(0))
    allocate(new_fast_packer%box_height(0))
    allocate(new_fast_packer%textures(0))
    allocate(new_fast_packer%available_x(1))
    allocate(new_fast_packer%available_y(1))

    new_fast_packer%available_x(1) = 0
    new_fast_packer%available_y(1) = 0

    new_fast_packer%allocated = .true.
  end function constructor_fast_packer


  !* Pack a texture located on disk.
  subroutine fast_packer_pack_from_file_path(this, file_path)
    implicit none

    class(fast_packer), intent(inout) :: this
    character(len = *, kind = c_char), intent(in) :: file_path

    ! todo: implementation.
  end subroutine fast_packer_pack_from_file_path


  !* Pack a texture located in memory.
  subroutine fast_packer_pack_from_memory(this, mem_texture)
    implicit none

    class(fast_packer), intent(inout) :: this
    type(memory_texture), intent(in) :: mem_texture

    ! todo: implementation.
  end subroutine fast_packer_pack_from_memory


  !* Pack the image data.
  subroutine fast_packer_internal_pack(this, current_index)
    implicit none

    class(fast_packer), intent(inout) :: this
    integer(c_int), intent(in) :: current_index

    do while(.not. this%tetris_pack(current_index))
      this%new_canvas_width = this%new_canvas_width + this%canvas_expansion_amount
      this%new_canvas_height = this%new_canvas_height + this%canvas_expansion_amount
    end do

    ! Finally, update the canvas's size in memory.
    call this%update_canvas_size(current_index)
  end subroutine fast_packer_internal_pack


  !* Tetris packing algorithm.
  function fast_packer_tetris_pack(this, current_index) result(pack_success)
    implicit none

    class(fast_packer), intent(inout) :: this
    integer(c_int), intent(in) :: current_index
    logical :: pack_success

    ! todo: implementation.
  end function fast_packer_tetris_pack


  !* Update the size of the texture packer's canvas.
  subroutine fast_packer_update_canvas_size(this, current_index)
    implicit none

    class(fast_packer), intent(inout) :: this
    integer(c_int), intent(in) :: current_index

    ! todo: implementation.
  end subroutine



end module fast_pack
