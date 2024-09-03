module chunk_handler
  use :: chunk_data
  use :: fhash, only: fhash_tbl_t, key => fhash_key
  implicit none


  private


  public :: store_chunk


  type(fhash_tbl_t) :: chunk_database



contains


  subroutine store_chunk(chunk_to_store)
    use :: string
    use :: fhash, only: fhash_key_char_t
    implicit none

    type(memory_chunk), intent(in), pointer :: chunk_to_store
    type(fhash_key_char_t) :: chunk_key
    integer(c_int) :: status

    chunk_key = key("chunk_"//int_to_string(chunk_to_store%world_position%x)//"_"//int_to_string(chunk_to_store%world_position%y))

    call chunk_database%check_key(chunk_key, stat = status)

    if (status == 0) then
      error stop "[Chunk Handler] Error: Attempted to overwrite a memory chunk pointer."
    end if

    call chunk_database%set_ptr(chunk_key, chunk_to_store)
  end subroutine store_chunk


end module chunk_handler
