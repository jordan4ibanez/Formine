module chunk_generator
  use :: string
  use :: chunk_mesh
  use :: chunk_data
  use :: chunk_handler
  use, intrinsic :: iso_c_binding
  implicit none


  private


  public :: chunk_generator_new_chunk


  type :: message_to_thread
    integer(c_int) :: x = 0
    integer(c_int) :: z = 0
  end type message_to_thread

  type :: message_from_thread
    type(memory_chunk), pointer :: data => null()
  end type message_from_thread


contains


  recursive function chunk_generator_thread(chunk_x, chunk_z) result(void_pointer) bind(c)
    use :: fast_noise_lite
    use :: chunk_handler
    implicit none

    integer(c_int), intent(in), value :: chunk_x, chunk_z
    type(c_ptr) :: void_pointer
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

    !todo: make this work in the queue processor.

    ! call chunk_handler_store_chunk_pointer(chunk_pointer)


    ! do i = 1,MESH_STACK_ARRAY_SIZE
    !   chunk_pointer%mesh(i) = ""
    !   call chunk_mesh_generate(chunk_x, chunk_z, i)
    ! end do
  end function chunk_generator_thread


  !* Queue up a chunk to be generated.
  subroutine chunk_generator_new_chunk(x, z)
    implicit none

    integer(c_int), intent(in), value :: x, z



  end subroutine chunk_generator_new_chunk

end module chunk_generator
