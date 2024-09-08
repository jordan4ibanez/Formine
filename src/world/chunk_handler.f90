module chunk_handler
  use :: chunk_data
  use :: fhash, only: fhash_tbl_t, key => fhash_key, fhash_key_char_t
  implicit none


  private


  public :: chunk_handler_store_chunk_pointer
  public :: chunk_handler_delete_chunk
  public :: chunk_handler_get_chunk_pointer
  public :: chunk_handler_get_clone_chunk_pointer
  public :: chunk_handler_draw_chunks


  type(fhash_tbl_t) :: chunk_database



contains


  !* Store a chunk pointer into the database.
  subroutine chunk_handler_store_chunk_pointer(chunk_to_store)
    use :: string
    implicit none

    type(memory_chunk), intent(in), pointer :: chunk_to_store
    type(fhash_key_char_t) :: chunk_key
    integer(c_int) :: status

    chunk_key = grab_chunk_key(chunk_to_store%world_position%x, chunk_to_store%world_position%y)

    call chunk_database%check_key(chunk_key, stat = status)

    if (status == 0) then
      error stop "[Chunk Handler] Error: Attempted to overwrite a memory chunk pointer."
    end if

    call chunk_database%set_ptr(chunk_key, chunk_to_store)
  end subroutine chunk_handler_store_chunk_pointer


  !* Delete a chunk from the database.
  subroutine chunk_handler_delete_chunk(x, y)
    implicit none

    integer(c_int), intent(in), value :: x, y
    type(fhash_key_char_t) :: chunk_key
    integer(c_int) :: status

    chunk_key = grab_chunk_key(x, y)

    call chunk_database%check_key(chunk_key, stat = status)

    if (status /= 0) then
      print"(A)", "[Chunk Handler] Warning: Attempted to delete chunk that doesn't exist."
    end if

    call chunk_database%unset(chunk_key)
  end subroutine chunk_handler_delete_chunk


  !* Get a raw chunk pointer from the database.
  function chunk_handler_get_chunk_pointer(x, y) result(chunk_pointer)
    implicit none

    integer(c_int), intent(in), value :: x, y
    type(memory_chunk), pointer :: chunk_pointer
    class(*), pointer :: generic_pointer
    integer(c_int) :: status

    call chunk_database%get_raw_ptr(grab_chunk_key(x,y), generic_pointer, stat = status)

    if (status /= 0) then
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

    call chunk_database%get_raw_ptr(grab_chunk_key(x,y), generic_pointer, stat = status)

    ! If not existent, return a null pointer.
    if (status /= 0) then
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
    use :: fhash, only: fhash_iter_t, fhash_key_t
    use :: mesh
    use :: texture
    use :: camera
    implicit none

    type(fhash_iter_t) :: iterator
    class(fhash_key_t), allocatable :: generic_key
    class(*), allocatable, target :: generic_data
    type(memory_chunk), pointer :: chunk
    integer(c_int) :: i
    character(len = :, kind = c_char), pointer :: current_mesh_id

    call chunk_database%stats(num_items = i)

    ! If there's nothing to do, don't do anything.
    if (i == 0) then
      return
    end if

    call texture_use("TEXTURE_ATLAS")

    iterator = fhash_iter_t(chunk_database)

    do while(iterator%next(generic_key, generic_data))
      select type(generic_data)
       type is (memory_chunk)
        chunk => generic_data
       class default
        error stop
        error stop "[Chunk Handler] Error: The wrong type was inserted into the database."
      end select

      do i = 1,MESH_STACK_ARRAY_SIZE
        current_mesh_id => chunk%mesh(i)%get_pointer()

        if (current_mesh_id == "") then
          cycle
        end if

        call camera_set_object_matrix_f32(&
          real(chunk%world_position%x * CHUNK_WIDTH, c_float), &
          real((i - 1) * MESH_STACK_HEIGHT, c_float), &
          real(chunk%world_position%y * CHUNK_WIDTH, c_float), &
          0.0, 0.0, 0.0, &
          1.0, 1.0, 1.0)

        call mesh_draw(current_mesh_id)
      end do
    end do
  end subroutine chunk_handler_draw_chunks


  !! Internal only. Generates a key from an x,y position.
  function grab_chunk_key(x, y) result(new_key)
    implicit none

    integer(c_int), intent(in), value :: x, y
    type(fhash_key_char_t) :: new_key

    new_key = key("chunk_"//int_to_string(x)//"_"//int_to_string(y))
  end function grab_chunk_key


end module chunk_handler
