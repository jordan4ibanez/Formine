module chunk
  use :: string
  use :: chunk_mesh
  use, intrinsic :: iso_c_binding
  implicit none


  private


  !* Width stands for X and Z. There is no sense in defining depth as they're equal sized.

  integer(c_int), parameter :: CHUNK_WIDTH = 16
  integer(c_int), parameter :: CHUNK_HEIGHT = 128

  !* Then we can define it as a flat array for massive caching boost.

  integer(c_int), parameter :: CHUNK_ARRAY_SIZE = CHUNK_WIDTH * CHUNK_HEIGHT * CHUNK_WIDTH

  !* The chunk is divided up into 8 meshes with width, height, and depth of 16.
  !* 1 is the bottom, 8 is the top.

  integer(c_int), parameter :: MESH_STACK_ARRAY_SIZE = 8
  integer(c_int), parameter :: MESH_STACK_HEIGHT = 16

  !* The stride before we reach into actual data.
  integer(c_int), parameter :: XY_STRIDE = CHUNK_WIDTH * CHUNK_HEIGHT


  !* Block data is one element in a chunk.

  type :: block_data
    ! Starts off as air.
    integer(c_int) :: id = 0
    ! Starts off as pitch black. Range: 0-15
    integer(1) :: light = 0
    ! There is no use for state yet. So we're going to leave this disabled.
    ! integer(c_int) :: state = 0
  end type block_data


  !* Chunk data is the data for the entire chunk.

  type :: chunk_data
    type(block_data), dimension(CHUNK_ARRAY_SIZE), allocatable :: data(:)
    type(heap_string), dimension(MESH_STACK_ARRAY_SIZE), allocatable :: mesh(:)
  end type chunk_data

  interface chunk_data
    module procedure :: chunk_data_constructor
  end interface chunk_data


  public :: debug_generate_chunk


contains


  function chunk_data_constructor() result(chunk_data_new)
    implicit none

    type(chunk_data) :: chunk_data_new

    allocate(chunk_data_new%data(CHUNK_ARRAY_SIZE))
    allocate(chunk_data_new%mesh(MESH_STACK_ARRAY_SIZE))
  end function chunk_data_constructor


  integer(c_int) function pos_to_index(x, y, z) result(index)
    implicit none

    integer(c_int), intent(in), value :: x, y, z
    integer(c_int) :: i, j, k

    ! Convert from index to offset.
    i = x - 1
    j = y - 1
    k = z - 1

    ! Convert back to index.
    index = ((i * XY_STRIDE) + (k * CHUNK_HEIGHT) + j) + 1
  end function pos_to_index


  function index_to_pos(index) result(temp_array)
    implicit none

    integer(c_int), intent(in), value :: index
    integer(c_int), dimension(3) :: temp_array
    integer(c_int) :: i

    ! Convert from index to offset.
    i = index - 1

    ! Convert from offset to index with +1.
    temp_array(1) = (i / XY_STRIDE) + 1
    i = mod(i, XY_STRIDE)
    temp_array(3) = (i / CHUNK_HEIGHT) + 1
    i = mod(i, CHUNK_HEIGHT)
    temp_array(2) = (i) + 1
  end function index_to_pos


  subroutine debug_generate_chunk(chunk_x, chunk_z)
    use :: fast_noise_lite
    implicit none

    integer(c_int), intent(in), value :: chunk_x, chunk_z
    type(fnl_state) :: noise_state

    integer(c_int) :: x, y, z, base_x, base_y, base_z, base_height, noise_multiplier, current_height
    type(chunk_data) :: current_chunk
    type(block_data) :: current_block
    integer(c_int) :: current_index
    integer(c_int), dimension(3) :: back_to

    current_index = 1

    current_chunk = chunk_data()

    base_x = chunk_x * CHUNK_WIDTH
    base_y = 0
    base_z = chunk_z * CHUNK_WIDTH

    noise_state = fnl_state()

    base_height = 70
    noise_multiplier = 20

    do x = 1, CHUNK_WIDTH
      do z = 1, CHUNK_WIDTH
        current_height = base_height + floor(fnl_get_noise_2d(noise_state, real(x), real(z)) * noise_multiplier)
        ! print*,current_height
        do y = 1, CHUNK_HEIGHT
          ! todo: make this more complex with lua registered biomes.

          if (pos_to_index(x,y,z) /= current_index) then
            error stop "wrong"
          end if

          back_to = index_to_pos(current_index)

          if (x /= back_to(1) .or. y /= back_to(2) .or. z /= back_to(3)) then
            error stop
          end if

          current_index = current_index + 1
          if (y <= current_height) then
            current_block = block_data()
            current_block%id = 1
            current_chunk%data(pos_to_index(x, y, z)) = current_block
          end if
        end do
      end do
    end do
  end subroutine


end module chunk
