module chunk_generator
  use :: string
  use :: chunk_mesh
  use :: chunk_data
  use :: chunk_handler
  use :: thread
  use, intrinsic :: iso_c_binding
  implicit none


  private

  public :: chunk_generator_initialize
  public :: chunk_generator_process_output_queue
  public :: chunk_generator_new_chunk


  type :: message_to_thread
    integer(c_int) :: x = 0
    integer(c_int) :: z = 0
  end type message_to_thread

  type :: message_from_thread
    type(memory_chunk), pointer :: data => null()
  end type message_from_thread


  type(concurrent_linked_filo_queue) :: thread_output_queue

  ! todo: make this a settign in the game's menu.
  integer(c_int) :: queue_pop_limit = 16


contains


  subroutine chunk_generator_initialize()
    implicit none

    thread_output_queue = concurrent_linked_filo_queue()
  end subroutine chunk_generator_initialize


  subroutine chunk_generator_process_output_queue()
    implicit none

    integer(c_int) :: i, chunk_x, chunk_z, w
    class(*), pointer :: generic_pointer
    type(memory_chunk), pointer :: chunk_pointer

    do i = 1,queue_pop_limit

      if (.not. thread_output_queue%pop(generic_pointer)) then
        exit
      end if

      select type (generic_pointer)
       type is (message_from_thread)
        chunk_pointer => generic_pointer%data
        deallocate(generic_pointer)
       class default
        error stop "[Chunk Generator] Error: Wrong type in queue."
      end select

      chunk_x = chunk_pointer%world_position%x
      chunk_z = chunk_pointer%world_position%y

      call chunk_handler_store_chunk_pointer(chunk_pointer)

      do w = 1,MESH_STACK_ARRAY_SIZE
        chunk_pointer%mesh(w) = 0
        call chunk_mesh_generate(chunk_x, chunk_z, w)
      end do

    end do

  end subroutine chunk_generator_process_output_queue


  recursive function chunk_generator_thread(c_arg_pointer) result(void_pointer) bind(c)
    use :: fast_noise_lite
    use :: chunk_handler
    implicit none

    type(c_ptr), intent(in), value :: c_arg_pointer
    type(c_ptr) :: void_pointer
    type(thread_argument), pointer :: arguments
    type(message_to_thread), pointer :: generator_message
    type(fnl_state) :: noise_state
    integer(c_int) :: chunk_x, chunk_z, x, y, z, base_x, base_y, base_z, base_height, noise_multiplier, current_height, status
    type(memory_chunk), pointer :: chunk_pointer
    type(block_data) :: current_block
    type(message_from_thread), pointer :: output_message

    !? Transfer main argument pointer to Fortran.

    if (.not. c_associated(c_arg_pointer)) then
      error stop "[Chunk Mesh] Fatal error: Was passed a null thread_argument pointer."
    end if

    call c_f_pointer(c_arg_pointer, arguments)

    !? Transfer sent data pointer to Fortran.

    if (.not. c_associated(arguments%data)) then
      error stop "[Chunk Mesh] Fatal error: Was passed a null sent_data pointer."
    end if

    call c_f_pointer(arguments%data, generator_message)

    chunk_x = generator_message%x
    chunk_z = generator_message%z

    !? Since this is quite a simple message, we will deallocate it right away.
    deallocate(generator_message)

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

    !? Finally, push the message to the queue.
    allocate(output_message)
    output_message%data => chunk_pointer

    call thread_output_queue%push(output_message)

    !? Flag thread as complete.
    status = thread_write_lock(arguments%mutex_pointer)

    void_pointer = c_null_ptr
    arguments%active_flag = .false.

    status = thread_unlock_lock(arguments%mutex_pointer)
  end function chunk_generator_thread


  !* Queue up a chunk to be generated.
  subroutine chunk_generator_new_chunk(x, z)
    implicit none

    integer(c_int), intent(in), value :: x, z
    type(message_to_thread), pointer :: message

    allocate(message)

    message%x = x
    message%z = z

    call thread_create(chunk_generator_thread, c_loc(message))
  end subroutine chunk_generator_new_chunk

end module chunk_generator
