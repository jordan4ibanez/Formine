module chunk_generator
  use :: string
  use :: chunk_mesh
  use :: chunk_data
  use :: chunk_handler
  use :: thread
  use :: vector
  use :: biome_repo
  use, intrinsic :: iso_c_binding
  implicit none


  private

  public :: chunk_generator_initialize
  public :: chunk_generator_process_output_queue
  public :: chunk_generator_new_chunk


  type :: message_to_thread
    integer(c_int) :: x = 0
    integer(c_int) :: z = 0
    integer(c_int) :: seed = 0
    type(vec) :: biomes
    integer(c_int) :: bedrock_id = 0
  end type message_to_thread

  type :: message_from_thread
    type(memory_chunk), pointer :: data => null()
  end type message_from_thread


  !* Type is: message_from_thread
  type(concurrent_fifo_queue) :: thread_output_queue

  ! todo: make this a setting in the game's menu.
  integer(c_int) :: queue_pop_limit = 16


contains


  subroutine chunk_generator_initialize()
    implicit none

    type(message_from_thread) :: blank

    thread_output_queue = new_concurrent_fifo_queue(sizeof(blank))
  end subroutine chunk_generator_initialize


  subroutine chunk_generator_process_output_queue()
    implicit none

    integer(c_int) :: i, chunk_x, chunk_z, w
    type(c_ptr) :: raw_c_ptr
    type(message_from_thread), pointer :: message_pointer
    type(memory_chunk), pointer :: chunk_pointer

    do i = 1,queue_pop_limit

      if (.not. thread_output_queue%pop(raw_c_ptr)) then
        exit
      end if

      call c_f_pointer(raw_c_ptr, message_pointer)

      chunk_pointer => message_pointer%data

      chunk_x = chunk_pointer%world_position%x
      chunk_z = chunk_pointer%world_position%y

      !? This will free() the chunk pointer when it stores it.
      ! print*,"====="
      ! print*,chunk_x,chunk_z
      call chunk_handler_store_chunk_pointer(chunk_pointer)

      deallocate(message_pointer)

      do w = 1,MESH_STACK_ARRAY_SIZE
        call chunk_mesh_generate(chunk_x, chunk_z, w, .true.)
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
    type(fnl_state) :: height_noise, biome_noise
    integer(c_int) :: chunk_x, chunk_z, seed, x, y, z, base_x, base_y, base_z, base_height, noise_multiplier, current_height, status, biome_amount, i
    type(memory_chunk), pointer :: chunk_pointer
    type(message_from_thread) :: output_message
    real(c_float) :: biome_noise_output
    type(vec) :: biomes
    type(c_ptr) :: raw_c_ptr
    type(biome_definition), pointer :: selected_biome

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
    seed = generator_message%seed
    biomes = generator_message%biomes

    !? We have our stack data, destroy it.
    deallocate(generator_message)

    height_noise = fnl_state()
    height_noise%seed = seed
    height_noise%frequency = 0.009

    biome_noise = fnl_state()
    biome_noise%seed = seed
    biome_noise%frequency = 0.002

    chunk_pointer => new_memory_chunk_pointer(chunk_x, chunk_z)

    base_x = chunk_x * CHUNK_WIDTH
    base_y = 0
    base_z = chunk_z * CHUNK_WIDTH

    base_height = 70
    noise_multiplier = 20

    biome_amount = int(biomes%size())

    !? If there are no other biomes besides grasslands, grasslands will always be selected.
    if (biome_amount == 1) then
      raw_c_ptr = biomes%get(1_8)
      call c_f_pointer(raw_c_ptr, selected_biome)
    end if

    do x = 1, CHUNK_WIDTH
      do z = 1, CHUNK_WIDTH

        !? Note: This floating point error creates the far lands.

        !? Note: Biome output ranges from: -1.0 - 1.0

        biome_noise_output = fnl_get_noise_2d(biome_noise, real(x + base_x), real(z + base_z))

        current_height = base_height + floor(fnl_get_noise_2d(height_noise, real(x + base_x), real(z + base_z)) * noise_multiplier)


        ! Select a biome if there's more than grasslands.
        if (biome_amount > 1) then

          !? Note: Biome noise goes from -1.0 - 1.0
          biome_noise_output = fnl_get_noise_2d(biome_noise, real(x + base_x), real(z + base_z))

          ! Run back and count on 1 (grasslands) as the default.
          selection: do i = biome_amount,1,-1

            raw_c_ptr = biomes%get(int(i, c_int64_t))

            call c_f_pointer(raw_c_ptr, selected_biome)

            if (selected_biome%heat_min <= biome_noise_output .and. selected_biome%heat_max >= biome_noise_output) then
              exit selection
            end if

          end do selection
        end if

        do y = 1, CHUNK_HEIGHT
          ! todo: make this more complex with lua registered biomes.

          if (y == current_height) then
            chunk_pointer%data(y, z, x)%id = selected_biome%grass_layer

          else if (y < current_height .and. y >= current_height - 4) then

            chunk_pointer%data(y, z, x)%id = selected_biome%dirt_layer
          else if (y < current_height) then
            chunk_pointer%data(y, z, x)%id = selected_biome%stone_layer

            ! todo: bedrock == y 1
          end if
        end do
      end do
    end do

    ! Destroy the biomes vector.
    call biomes%destroy()

    !? Finally, push the message to the queue.
    output_message%data => chunk_pointer

    call thread_output_queue%push(output_message)

    !? Flag thread as complete.
    status = thread_lock_mutex(arguments%mutex_ptr)

    void_pointer = c_null_ptr
    arguments%active_flag = .false.

    status = thread_unlock_mutex(arguments%mutex_ptr)
  end function chunk_generator_thread


  !* Queue up a chunk to be generated.
  subroutine chunk_generator_new_chunk(x, z)
    use :: world_data
    implicit none

    integer(c_int), intent(in), value :: x, z
    type(message_to_thread), pointer :: message

    allocate(message)
    message%x = x
    message%z = z
    message%seed = world_data_get_world_seed()
    message%biomes = biome_repo_copy_definition_array()
    message%bedrock_id = biome_repo_get_bedrock_id()

    call thread_create(chunk_generator_thread, c_loc(message))
  end subroutine chunk_generator_new_chunk


end module chunk_generator
