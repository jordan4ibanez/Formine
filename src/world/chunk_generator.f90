module chunk_generator
  use :: string
  use :: chunk_mesh
  use :: chunk_data
  use, intrinsic :: iso_c_binding
  implicit none


  private


  public :: debug_generate_chunk


contains

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
    type(memory_chunk) :: current_chunk
    type(block_data) :: current_block
    integer(c_int) :: current_index
    integer(c_int), dimension(3) :: back_to

    current_index = 1

    current_chunk = memory_chunk()

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

    current_chunk%mesh(1) = chunk_mesh_generate(current_chunk)
  end subroutine


end module chunk_generator
