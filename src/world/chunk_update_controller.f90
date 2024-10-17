module chunk_update_controller
  use, intrinsic :: iso_c_binding
  use :: chunk_generator
  implicit none


  private


  public :: chunk_update_controller_new_chunk


contains


  subroutine chunk_update_controller_new_chunk(x,z)
    implicit none

    integer(c_int), intent(in), value :: x,z

    call chunk_generator_new_chunk(x,z)

  end subroutine chunk_update_controller_new_chunk


  ! todo: hashset queue

  ! todo: single block update which calculates into the specific portion of the chunk

end module chunk_update_controller
