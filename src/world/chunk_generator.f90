module chunk_generator
  use :: string
  use :: chunk_mesh
  use :: chunk_data
  use, intrinsic :: iso_c_binding
  implicit none


  private


  public :: chunk_generator_new_chunk


contains


  subroutine chunk_generator_new_chunk(chunk_x, chunk_z)
    use :: fast_noise_lite
    use :: chunk_handler
    implicit none

    integer(c_int), intent(in), value :: chunk_x, chunk_z
    type(fnl_state) :: noise_state

    integer(c_int) :: x, y, z, base_x, base_y, base_z, base_height, noise_multiplier, current_height, i
    type(memory_chunk), pointer :: chunk_pointer
    type(block_data) :: current_block

    chunk_pointer => memory_chunk(chunk_x, chunk_z)

    base_x = chunk_x * CHUNK_WIDTH
    base_y = 0
    base_z = chunk_z * CHUNK_WIDTH

    noise_state = fnl_state()

    base_height = 70
    noise_multiplier = 20

    do x = 1, CHUNK_WIDTH
      do z = 1, CHUNK_WIDTH
        current_height = base_height + floor(fnl_get_noise_2d(noise_state, real(x + base_x), real(z + base_z)) * noise_multiplier)
        do y = 1, CHUNK_HEIGHT
          ! todo: make this more complex with lua registered biomes.

          if (y <= current_height) then
            current_block = block_data()
            current_block%id = 1
            chunk_pointer%data(y, z, x) = current_block
          end if
        end do
      end do
    end do

    call chunk_handler_store_chunk_pointer(chunk_pointer)


    do i = 1,MESH_STACK_ARRAY_SIZE
      chunk_pointer%mesh(i) = ""
      !! BEGIN MEMORY LEAK
      call chunk_mesh_generate(chunk_x, chunk_z, i)
      !! END MEMORY LEAK
    end do
  end subroutine chunk_generator_new_chunk


end module chunk_generator
