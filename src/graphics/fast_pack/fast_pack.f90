module fast_pack
  use :: memory_texture_module
  ! use :: texture
  use :: stb_image
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
    procedure, private :: upload_texture_path => fast_packer_upload_texture_from_file_path
    procedure, private :: upload_texture_memory => fast_packer_upload_texture_from_memory
    procedure, private :: trim_and_sort_available_slots => fast_packer_trim_and_sort_available_slots
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
    integer(c_int) :: new_right, new_top, padding

    new_right = this%position_x(current_index)

    ! todo: implementation.
  end subroutine


  !* Upload a texture from a file path into the fast_packer.
  function fast_packer_upload_texture_from_file_path(this, texture_key, file_path) result(new_index)
    implicit none

    class(fast_packer), intent(inout) :: this
    character(len = *, kind = c_char), intent(in) :: texture_key, file_path
    integer(c_int) :: new_index
    integer(c_int) :: width, height, channels
    integer(1), dimension(:), allocatable :: temporary_raw_texture
    type(memory_texture) :: new_texture

    temporary_raw_texture = stbi_load(file_path, width, height, channels, 4)

    new_texture = memory_texture(temporary_raw_texture, width, height)

    ! This is chained.
    new_index = this%upload_texture_memory(texture_key, new_texture)
  end function fast_packer_upload_texture_from_file_path


  !* Upload a memory_texture into the fast_packer.
  function fast_packer_upload_texture_from_memory(this, texture_key, mem_texture) result(new_index)
    implicit none

    class(fast_packer), intent(inout) :: this
    character(len = *, kind = c_char), intent(in) :: texture_key
    type(memory_texture), intent(in) :: mem_texture
    integer(c_int) :: new_index

    ! todo: implement trimming maybe

    new_index = this%current_id

    this%current_id = this%current_id + 1

    ! Add data.
    call this%keys%set(key(texture_key), mem_texture)
    this%position_x = [this%position_x, 0]
    this%position_y = [this%position_y, 0]
    this%box_width = [this%box_width, mem_texture%width]
    this%box_height = [this%box_height, mem_texture%height]
    this%textures = [this%textures, mem_texture]

    call this%trim_and_sort_available_slots()
  end function fast_packer_upload_texture_from_memory


  !* Removes duplicates, automatically sorts small to large.
  subroutine fast_packer_trim_and_sort_available_slots(this)
    use :: array, only: array_i32_small_to_large_unique
    implicit none

    class(fast_packer), intent(inout) :: this

    this%available_x = array_i32_small_to_large_unique(this%available_x)
    this%available_y = array_i32_small_to_large_unique(this%available_y)
  end subroutine fast_packer_trim_and_sort_available_slots


end module fast_pack
