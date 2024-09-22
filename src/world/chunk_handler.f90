module chunk_handler
  use :: chunk_data
  use :: mesh
  use :: hashmap_str
  implicit none


  private


  public :: chunk_handler_module_initalize
  public :: chunk_handler_set_chunk_mesh
  public :: chunk_handler_store_chunk_pointer
  public :: chunk_handler_delete_chunk
  public :: chunk_handler_get_chunk_pointer
  public :: chunk_handler_get_clone_chunk_pointer
  public :: chunk_handler_draw_chunks


  !! fixme: use a GC function!
  type(hashmap_string_key) :: chunk_database


contains


  !* Initialize the module.
  subroutine chunk_handler_module_initalize()
    implicit none

    chunk_database = new_hashmap_string_key()
  end subroutine chunk_handler_module_initalize


  !* Sets the chunk mesh in the chunk's stack array.
  subroutine chunk_handler_set_chunk_mesh(x, z, stack, mesh_id)
    implicit none

    integer(c_int), intent(in), value :: x, z, stack
    character(len = *, kind = c_char), intent(in) :: mesh_id
    type(memory_chunk), pointer :: current_chunk

    current_chunk => chunk_handler_get_chunk_pointer(x,z)

    ! Clean up the old chunk mesh.
    if (current_chunk%mesh(stack)%get_pointer() /= "") then
      call mesh_delete(current_chunk%mesh(stack)%get_pointer())
    end if

    current_chunk%mesh(stack) = mesh_id
  end subroutine chunk_handler_set_chunk_mesh


  !* Store a chunk pointer into the database.
  subroutine chunk_handler_store_chunk_pointer(chunk_to_store)
    use :: string
    implicit none

    type(memory_chunk), intent(in), pointer :: chunk_to_store
    character(len = :, kind = c_char), allocatable :: chunk_key

    chunk_key = grab_chunk_key(chunk_to_store%world_position%x, chunk_to_store%world_position%y)

    if (chunk_database%has_key(chunk_key)) then
      error stop "[Chunk Handler] Error: Attempted to overwrite a memory chunk pointer."
    end if

    call chunk_database%set(chunk_key, chunk_to_store)
  end subroutine chunk_handler_store_chunk_pointer


  !* Delete a chunk from the database.
  subroutine chunk_handler_delete_chunk(x, y)
    implicit none

    integer(c_int), intent(in), value :: x, y
    class(*), pointer :: generic_pointer
    character(len = :, kind = c_char), allocatable :: chunk_key
    type(memory_chunk), pointer :: chunk_pointer

    chunk_key = grab_chunk_key(x, y)

    if (.not. chunk_database%get(chunk_key, generic_pointer)) then
      print"(A)", "[Chunk Handler] Warning: Attempted to delete chunk that doesn't exist."
      return
    end if

    select type (generic_pointer)
     type is (memory_chunk)
      chunk_pointer => generic_pointer
     class default
      error stop "[Chunk Handler] Error: Wrong type in the chunk database."
    end select

    deallocate(chunk_pointer%world_position)
    deallocate(chunk_pointer%data)
    deallocate(chunk_pointer%mesh)
    deallocate(chunk_pointer)

    call chunk_database%delete(chunk_key)
  end subroutine chunk_handler_delete_chunk


  !* Get a raw chunk pointer from the database.
  function chunk_handler_get_chunk_pointer(x, y) result(chunk_pointer)
    implicit none

    integer(c_int), intent(in), value :: x, y
    type(memory_chunk), pointer :: chunk_pointer
    class(*), pointer :: generic_pointer
    integer(c_int) :: status

    if (.not. chunk_database%get(grab_chunk_key(x,y), generic_pointer)) then
      error stop "[Chunk Handler] Error: Attempted to retrieve null chunk."
    end if

    select type(generic_pointer)
     type is (memory_chunk)
      chunk_pointer => generic_pointer
     class default
      error stop "[Chunk Handler] Error: The wrong type was inserted into the database."
    end select
  end function chunk_handler_get_chunk_pointer


  !* This will clone a chunk's raw data (not the meshes) and return the pointer to it.
  function chunk_handler_get_clone_chunk_pointer(x, y) result(clone_chunk_pointer)
    implicit none

    integer(c_int), intent(in), value :: x, y
    type(memory_chunk), pointer :: clone_chunk_pointer
    type(memory_chunk), pointer :: original_chunk_pointer
    class(*), pointer :: generic_pointer
    integer(c_int) :: status

    clone_chunk_pointer => null()

    ! If not existent, return a null pointer.
    if (.not. chunk_database%get(grab_chunk_key(x,y), generic_pointer)) then
      return
    end if

    select type(generic_pointer)
     type is (memory_chunk)
      original_chunk_pointer => generic_pointer
     class default
      error stop "[Chunk Handler] Error: The wrong type was inserted into the database."
    end select

    ! Allocate pointer.
    clone_chunk_pointer => memory_chunk(x,y)

    ! Allocation upon assignment.
    clone_chunk_pointer%data = original_chunk_pointer%data
    clone_chunk_pointer%world_position = original_chunk_pointer%world_position
  end function chunk_handler_get_clone_chunk_pointer


  !* Draw all chunk meshes.
  subroutine chunk_handler_draw_chunks()
    use :: hashmap_str
    use :: mesh
    use :: texture
    use :: camera
    implicit none

    character(len = :, kind = c_char), pointer :: string_key
    class(*), pointer :: generic_pointer
    type(memory_chunk), pointer :: chunk
    integer(c_int64_t) :: i
    character(len = :, kind = c_char), pointer :: current_mesh_id

    ! If there's nothing to do, don't do anything.
    !!FIXME: REPLACE WITH IS_EMPTY()
    if (chunk_database%count() == 0) then
      return
    end if

    call texture_use("TEXTURE_ATLAS")

    i = 0
    !! fixme: use the new iterator style!
    do while(chunk_database%iterate_kv(i, string_key, generic_pointer))
      !   select type(generic_data)
      !    type is (memory_chunk)
      !     chunk => generic_data
      !    class default
      !     error stop "[Chunk Handler] Error: The wrong type was inserted into the database."
      !   end select

      !   do i = 1,MESH_STACK_ARRAY_SIZE
      !     current_mesh_id => chunk%mesh(i)%get_pointer()

      !     if (current_mesh_id == "") then
      !       cycle
      !     end if

      !     ! call camera_set_object_matrix_f32(&
      !     !   real(chunk%world_position%x * CHUNK_WIDTH, c_float), &
      !     !   real((i - 1) * MESH_STACK_HEIGHT, c_float), &
      !     !   real(chunk%world_position%y * CHUNK_WIDTH, c_float), &
      !     !   0.0, 0.0, 0.0, &
      !     !   1.0, 1.0, 1.0)

      !     ! call mesh_draw(current_mesh_id)
      !   end do
    end do
  end subroutine chunk_handler_draw_chunks


  !! Internal only. Generates a key from an x,y position.
  function grab_chunk_key(x, y) result(new_key)
    implicit none

    integer(c_int), intent(in), value :: x, y
    character(len = :, kind = c_char), allocatable :: new_key

    new_key = "chunk_"//int_to_string(x)//"_"//int_to_string(y)
  end function grab_chunk_key


end module chunk_handler
