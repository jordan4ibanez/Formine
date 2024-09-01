module chunk_mesh
  use :: mesh
  use :: chunk_data
  use, intrinsic :: iso_c_binding
  implicit none


  private


  public :: chunk_mesh_generate


contains


  function chunk_mesh_generate(input_chunk) result(mesh_id)
    implicit none

    type(memory_chunk), intent(in) :: input_chunk
    character(len = :, kind = c_char), allocatable :: mesh_id

  end function chunk_mesh_generate


end module chunk_mesh
