module chunk_handler
  use :: chunk_data
  use :: mesh
  use :: concurrent_hashmap_str
  use :: thread
  implicit none


  private


  public :: chunk_handler_module_initalize
  public :: chunk_handler_set_chunk_mesh
  public :: chunk_handler_chunk_exists
  public :: chunk_handler_store_chunk_pointer
  public :: chunk_handler_delete_chunk
  public :: chunk_handler_get_chunk_pointer
  public :: chunk_handler_get_clone_chunk_pointer
  public :: chunk_handler_draw_chunks


  !* Type: memory_chunk
  type(concurrent_hashmap_string_key) :: chunk_database


contains


  !* Initialize the module.
  subroutine chunk_handler_module_initalize()
    implicit none

    type(memory_chunk), allocatable :: blank

    chunk_database = new_concurrent_hashmap_string_key(sizeof(blank), gc_chunk_database)
  end subroutine chunk_handler_module_initalize


  !* Sets the chunk mesh in the chunk's stack array.
  subroutine chunk_handler_set_chunk_mesh(x, z, stack, vao_id)
    implicit none

    integer(c_int), intent(in), value :: x, z, stack
    integer(c_int), intent(in), value :: vao_id
    type(memory_chunk), pointer :: current_chunk
    character(len = :, kind = c_char), allocatable :: key
    type(c_ptr) :: raw_c_ptr

    call chunk_database%lock()

    key = grab_chunk_key(x,z)

    if (.not. chunk_database%get(key, raw_c_ptr)) then
      ! print"(A)", "[Chunk Handler] Warning: Cannot set mesh for null chunk. Abort."
      !? Auto GC the VAO.
      call mesh_delete(vao_id)
    end if

    call c_f_pointer(raw_c_ptr, current_chunk)

    ! Clean up the old chunk mesh.
    if (current_chunk%mesh(stack) /= 0) then
      call mesh_delete(current_chunk%mesh(stack))
    end if

    ! print"(A)", "[Chunk Handler] Debug: Set mesh ["//int_to_string(current_chunk%world_position%x)//","//int_to_string(current_chunk%world_position%y)//"] stack: ["//int_to_string(stack)//"]"

    current_chunk%mesh(stack) = vao_id

    call chunk_database%unlock()
  end subroutine chunk_handler_set_chunk_mesh


  !* Check if a chunk exists.
  function chunk_handler_chunk_exists(x,z) result(exist)
    implicit none

    integer(c_int), intent(in), value :: x, z
    logical(c_bool) :: exist
    character(len = :, kind = c_char), allocatable :: key

    call chunk_database%lock()

    key = grab_chunk_key(x, z)

    exist = chunk_database%has_key(key)

    call chunk_database%unlock()
  end function chunk_handler_chunk_exists


  !* Store a chunk pointer into the database.
  subroutine chunk_handler_store_chunk_pointer(chunk_pointer)
    use :: string_f90
    implicit none

    type(memory_chunk), intent(inout), pointer :: chunk_pointer
    character(len = :, kind = c_char), allocatable :: chunk_key

    call chunk_database%lock()

    chunk_key = grab_chunk_key(chunk_pointer%world_position%x, chunk_pointer%world_position%y)

    if (chunk_database%has_key(chunk_key)) then
      print"(A)", "[Chunk Handler] Warning: Attempted to overwrite a memory chunk pointer."

      deallocate(chunk_pointer)
      call chunk_database%unlock()
      return
    end if

    ! print*,"storing chunk: ["//int_to_string(chunk_pointer%world_position%x)//","//int_to_string(chunk_pointer%world_position%y)//"]"

    call chunk_database%set(chunk_key, chunk_pointer)

    ! This is memcpy'd into the hashmap.
    ! Free the memory.
    deallocate(chunk_pointer)

    call chunk_database%unlock()
  end subroutine chunk_handler_store_chunk_pointer


  !* Delete a chunk from the database.
  subroutine chunk_handler_delete_chunk(x, y)
    implicit none

    integer(c_int), intent(in), value :: x, y
    character(len = :, kind = c_char), allocatable :: key

    call chunk_database%lock()

    key = grab_chunk_key(x, y)

    call chunk_database%remove(key)

    call chunk_database%unlock()
  end subroutine chunk_handler_delete_chunk


  !* Get a raw chunk pointer from the database.
  function chunk_handler_get_chunk_pointer(x, y, chunk_pointer) result(exists)
    implicit none

    integer(c_int), intent(in), value :: x, y
    type(memory_chunk), intent(inout), pointer :: chunk_pointer
    logical(c_bool) :: exists
    type(c_ptr) :: raw_c_ptr
    character(len = :, kind = c_char), allocatable :: key

    call chunk_database%lock()

    exists = .false.

    key = grab_chunk_key(x,y)

    if (.not. chunk_database%get(key, raw_c_ptr)) then
      ! print"(A)","[Chunk Handler] Warning: Attempted to retrieve null chunk."
      call chunk_database%unlock()
      return
    end if

    call c_f_pointer(raw_c_ptr, chunk_pointer)

    exists = .true.
    call chunk_database%unlock()
  end function chunk_handler_get_chunk_pointer


  !* This will clone a chunk's raw data (not the meshes) and return the pointer to it.
  function chunk_handler_get_clone_chunk_pointer(x, y) result(clone_chunk_pointer)
    implicit none

    integer(c_int), intent(in), value :: x, y
    type(memory_chunk), pointer :: clone_chunk_pointer
    type(memory_chunk), pointer :: original_chunk_pointer
    type(c_ptr) :: raw_c_ptr
    character(len = :, kind = c_char), allocatable :: key


    call chunk_database%lock()

    clone_chunk_pointer => null()

    ! If not existent, return a null pointer.
    key = grab_chunk_key(x,y)
    if (.not. chunk_database%get(key, raw_c_ptr)) then
      call chunk_database%unlock()
      return
    end if

    call c_f_pointer(raw_c_ptr, original_chunk_pointer)

    ! Allocate pointer.
    clone_chunk_pointer => new_memory_chunk_pointer(x,y)

    clone_chunk_pointer%data = original_chunk_pointer%data
    clone_chunk_pointer%world_position = original_chunk_pointer%world_position

    call chunk_database%unlock()
  end function chunk_handler_get_clone_chunk_pointer


  !* Draw all chunk meshes.
  subroutine chunk_handler_draw_chunks()
    use :: hashmap_str
    use :: mesh
    use :: texture
    use :: camera
    implicit none

    character(len = :, kind = c_char), pointer :: string_key
    type(c_ptr) :: raw_c_ptr
    type(memory_chunk), pointer :: chunk_pointer
    integer(c_int) :: i, current_mesh_id

    call chunk_database%lock()

    ! If there's nothing to do, don't do anything.
    if (chunk_database%is_empty()) then
      call chunk_database%unlock()
      return
    end if

    call texture_use("TEXTURE_ATLAS")

    call chunk_database%initialize_iterator()
    do while(chunk_database%iterate_kv(string_key, raw_c_ptr))

      call c_f_pointer(raw_c_ptr, chunk_pointer)

      do i = 1,MESH_STACK_ARRAY_SIZE
        current_mesh_id = chunk_pointer%mesh(i)

        if (current_mesh_id == 0) then
          cycle
        end if

        call camera_set_object_matrix_f32(&
          real(chunk_pointer%world_position%x * CHUNK_WIDTH, c_float), &
          real((i - 1) * MESH_STACK_HEIGHT, c_float), &
          real(chunk_pointer%world_position%y * CHUNK_WIDTH, c_float), &
          0.0, 0.0, 0.0, &
          1.0, 1.0, 1.0)

        call mesh_draw(current_mesh_id)
      end do
    end do

    call chunk_database%unlock()
  end subroutine chunk_handler_draw_chunks


  !! Internal only. Generates a key from an x,y position.
  function grab_chunk_key(x, y) result(new_key)
    implicit none

    integer(c_int), intent(in), value :: x, y
    character(len = :, kind = c_char), allocatable :: new_key

    new_key = "chunk_"//int_to_string(x)//"_"//int_to_string(y)
  end function grab_chunk_key


  subroutine gc_chunk_database(raw_c_ptr)
    implicit none

    type(c_ptr), intent(in), value :: raw_c_ptr
    type(memory_chunk), pointer :: chunk_pointer
    integer(c_int) :: i, vao_id

    call c_f_pointer(raw_c_ptr, chunk_pointer)

    ! print*,"deleting ["//int_to_string(chunk_pointer%world_position%x)//","//int_to_string(chunk_pointer%world_position%y)//"]"

    do i = 1,MESH_STACK_ARRAY_SIZE
      vao_id = chunk_pointer%mesh(i)

      if (vao_id /= 0) then
        call mesh_delete(vao_id)
        ! print*,"DELETED MESH: "//int_to_string(vao_id)
      end if
    end do
  end subroutine gc_chunk_database


end module chunk_handler
