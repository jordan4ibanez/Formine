module chunk
  use :: string
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
    type(block_data), dimension(CHUNK_ARRAY_SIZE) :: data
    type(heap_string), dimension(MESH_STACK_ARRAY_SIZE) :: mesh
  end type chunk_data


  public :: debug_generate_chunk

contains


  subroutine debug_generate_chunk(chunk_x, chunk_z)
    use :: fast_noise_lite
    implicit none

    integer(c_int), intent(in), value :: chunk_x, chunk_z
    type(fnl_state) :: noise_state

    integer(c_int) :: x, y, z, base_x, base_y, base_z, base_height, noise_multiplier, current_height

    base_x = chunk_x * CHUNK_WIDTH
    base_y = 0
    base_z = chunk_z * CHUNK_WIDTH

    noise_state = fnl_state()

    base_height = 70
    noise_multiplier = 20

    do x = 1, CHUNK_WIDTH
      do z = 1, CHUNK_WIDTH
        current_height = base_height + floor(fnl_get_noise_2d(noise_state, real(x), real(z)) * noise_multiplier)
        do y = 1, CHUNK_HEIGHT
          
        end do
      end do
    end do
  end subroutine


end module chunk
