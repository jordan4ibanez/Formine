!* This module kind of works like a state machine.
module mesh
  use :: string_f90
  use :: vector_3f
  use :: hashmap_int
  use :: hashmap_str
  use :: opengl
  use :: shader
  use :: mod_mesh_intrinsics
  use, intrinsic :: iso_c_binding
  implicit none

  private

  public :: mesh_module_initialize

  public :: mesh_create_2d
  public :: mesh_create_2d_named
  public :: mesh_create_3d
  public :: mesh_create_3d_named

  public :: mesh_draw
  public :: mesh_draw_by_name
  public :: mesh_delete
  public :: mesh_delete_by_name
  public :: mesh_exists
  public :: mesh_exists_by_name

  public :: mesh_destroy_database


  !* Type: mesh_data
  type(hashmap_integer_key) :: mesh_database

  !* Type: integer(c_int)
  type(hashmap_string_key) :: mesh_name_to_id_database


contains

  !* Initialize the module.
  subroutine mesh_module_initialize()
    implicit none

    mesh_database = new_hashmap_integer_key(sizeof(mesh_data()), gc_mesh_database)

    mesh_name_to_id_database = new_hashmap_string_key(sizeof(10))
  end subroutine mesh_module_initialize


  !* Create a mesh for 2 dimensional space.
  function mesh_create_2d(positions, texture_coordinates, colors, indices) result(vao_id)
    implicit none

    real(c_float), dimension(:), intent(in), target :: positions, texture_coordinates, colors
    integer(c_int), dimension(:), intent(in), target :: indices
    integer(c_int) :: vao_id

    vao_id = mesh_create_internal(mesh_database, mesh_name_to_id_database, 2, positions, texture_coordinates, colors, indices, .false.)
  end function mesh_create_2d


  !* Create a mesh with a name for 2 dimensional space.
  subroutine mesh_create_2d_named(mesh_name, positions, texture_coordinates, colors, indices)
    implicit none

    character(len = *, kind = c_char), intent(in) :: mesh_name
    real(c_float), dimension(:), intent(in), target :: positions, texture_coordinates, colors
    integer(c_int), dimension(:), intent(in), target :: indices
    integer(c_int) :: discard

    discard = mesh_create_internal(mesh_database, mesh_name_to_id_database, 2, positions, texture_coordinates, colors, indices, .true., mesh_name)
  end subroutine mesh_create_2d_named


  !* Create a mesh for 3 dimensional space.
  function mesh_create_3d(positions, texture_coordinates, colors, indices) result(vao_id)
    implicit none

    real(c_float), dimension(:), intent(in), target :: positions, texture_coordinates, colors
    integer(c_int), dimension(:), intent(in), target :: indices
    integer(c_int) :: vao_id

    vao_id = mesh_create_internal(mesh_database, mesh_name_to_id_database, 3, positions, texture_coordinates, colors, indices, .false.)
  end function mesh_create_3d


  !* Create a mesh with a name for 3 dimensional space.
  subroutine mesh_create_3d_named(mesh_name, positions, texture_coordinates, colors, indices)
    implicit none

    character(len = *, kind = c_char), intent(in) :: mesh_name
    real(c_float), dimension(:), intent(in), target :: positions, texture_coordinates, colors
    integer(c_int), dimension(:), intent(in), target :: indices
    integer(c_int) :: discard

    discard = mesh_create_internal(mesh_database, mesh_name_to_id_database, 3, positions, texture_coordinates, colors, indices, .true., mesh_name)
  end subroutine mesh_create_3d_named


  !* Get a mesh from the hash table.
  !* The mesh is a clone. To update, set_mesh().
  function get_mesh(vao_id, gotten_mesh) result(exists)
    use :: terminal
    implicit none

    integer(c_int), intent(in), value :: vao_id
    type(mesh_data), intent(inout) :: gotten_mesh
    logical(c_bool) :: exists
    type(c_ptr) :: raw_c_ptr
    type(mesh_data), pointer :: mesh_pointer

    exists = .false.

    if (.not. mesh_database%get(int(vao_id, c_int64_t), raw_c_ptr)) then
      call print_color(WARNING, "[Mesh] Warning: ID ["//int_to_string(vao_id)//"] does not exist.")
      return
    end if

    call c_f_pointer(raw_c_ptr, mesh_pointer)
    gotten_mesh = mesh_pointer

    exists = .true.
  end function get_mesh


  !* Get a mesh from the hash table by name.
  !* The mesh is a clone. To update, set_mesh().
  !! This is slower than get_mesh.
  function get_mesh_by_name(mesh_name, gotten_mesh) result(exists)
    use :: terminal
    implicit none

    character(len = *, kind = c_char), intent(in) :: mesh_name
    type(mesh_data), intent(inout) :: gotten_mesh
    logical(c_bool) :: exists
    type(c_ptr) :: raw_c_ptr
    integer(c_int), pointer :: id_pointer
    type(mesh_data), pointer :: mesh_pointer

    exists = .false.

    if (.not. mesh_name_to_id_database%get(mesh_name, raw_c_ptr)) then
      call print_color(WARNING, "[Mesh] Warning: ["//mesh_name//"] does not exist.")
      return
    end if

    call c_f_pointer(raw_c_ptr, id_pointer)

    if (.not. mesh_database%get(int(id_pointer, c_int64_t), raw_c_ptr)) then
      error stop "[Mesh] Error: Mesh ["//mesh_name//"] is pointing at an invalid id ["//int_to_string(id_pointer)//"]"
    end if

    call c_f_pointer(raw_c_ptr, mesh_pointer)
    gotten_mesh = mesh_pointer

    exists = .true.
  end function get_mesh_by_name


  !* Draw a mesh.
  subroutine mesh_draw(vao_id)
    use :: terminal
    implicit none

    integer(c_int), intent(in), value :: vao_id
    type(mesh_data) :: gotten_mesh

    if (.not. get_mesh(vao_id, gotten_mesh)) then
      call print_color(WARNING, "[Mesh] Warning: Mesh ID ["//int_to_string(vao_id)//"] does not exist. Cannot draw.")
      return
    end if

    call gl_bind_vertex_array(gotten_mesh%vao)

    call gl_draw_elements(GL_TRIANGLES, gotten_mesh%indices_length, GL_UNSIGNED_INT)

    call gl_bind_vertex_array(0)
  end subroutine mesh_draw


  !* Draw a mesh by name.
  !! This is slower than mesh_draw.
  subroutine mesh_draw_by_name(mesh_name)
    use :: terminal
    implicit none

    character(len = *, kind = c_char), intent(in) :: mesh_name
    type(mesh_data) :: gotten_mesh

    if (.not. get_mesh_by_name(mesh_name, gotten_mesh)) then
      call print_color(WARNING, "[Mesh] Warning: Mesh ["//mesh_name//"] does not exist. Cannot draw.")
      return
    end if

    call gl_bind_vertex_array(gotten_mesh%vao)

    call gl_draw_elements(GL_TRIANGLES, gotten_mesh%indices_length, GL_UNSIGNED_INT)

    call gl_bind_vertex_array(0)
  end subroutine mesh_draw_by_name


  !* Delete a mesh.
  subroutine mesh_delete(vao_id)
    use :: terminal
    implicit none

    integer(c_int), intent(in), value :: vao_id

    ! This wipes out the OpenGL memory as well or else there's going to be a massive memory leak.
    ! This is written so it can be used for set_mesh to auto delete the old mesh.

    !? This must error stop because there is an implementation error.
    if (.not. mesh_exists(vao_id)) then
      call print_color(WARNING, "[Mesh] Warning: Mesh ["//int_to_string(vao_id)//"] does not exist. Cannot delete.")
      return
    end if

    call mesh_database%remove(int(vao_id, c_int64_t))
  end subroutine mesh_delete


  !* Delete a mesh by name.
  !! This is slower than mesh_delete.
  subroutine mesh_delete_by_name(mesh_name)
    use :: terminal
    implicit none

    character(len = *, kind = c_char), intent(in) :: mesh_name
    type(c_ptr) :: raw_c_ptr
    integer(c_int), pointer :: vao_id

    ! This wipes out the OpenGL memory as well or else there's going to be a massive memory leak.
    ! This is written so it can be used for set_mesh to auto delete the old mesh.

    if (.not. mesh_name_to_id_database%get(mesh_name, raw_c_ptr)) then
      call print_color(WARNING, "[Mesh] Warning: Mesh ["//mesh_name//"] does not exist. Cannot delete.")
      return
    end if

    call c_f_pointer(raw_c_ptr, vao_id)

    !? This must error stop because there is an implementation error.
    if (.not. mesh_exists(vao_id)) then
      call print_color(ERROR, "[Mesh] Error: Mesh ["//mesh_name//"] is pointing to an invalid ID ["//int_to_string(vao_id)//"].")
      return
    end if

    call mesh_database%remove(int(vao_id, c_int64_t))
    call mesh_name_to_id_database%remove(mesh_name)
  end subroutine mesh_delete_by_name



  !* Check if a mesh exists by name.
  logical function mesh_exists(vao_id) result(existence)
    implicit none

    integer(c_int), intent(in), value :: vao_id

    existence = mesh_database%has_key(int(vao_id, c_int64_t))
  end function mesh_exists


  !* Check if a mesh exists by name.
  logical function mesh_exists_by_name(mesh_name) result(existence)
    implicit none

    character(len = *, kind = c_char), intent(in) :: mesh_name

    existence = mesh_name_to_id_database%has_key(mesh_name)
  end function mesh_exists_by_name


  !* Completely wipe out all existing meshes.
  subroutine mesh_destroy_database()
    use :: terminal
    implicit none

    call mesh_database%destroy()
    call mesh_name_to_id_database%destroy()
  end subroutine mesh_destroy_database


  subroutine gc_mesh_database(raw_c_ptr)
    implicit none

    type(c_ptr), intent(in), value :: raw_c_ptr
    type(mesh_data), pointer :: mesh_pointer

    call c_f_pointer(raw_c_ptr, mesh_pointer)

    ! Now start.

    call gl_bind_vertex_array(mesh_pointer%vao)

    ! Positions.
    call gl_disable_vertex_attrib_array(ATTRIBUTE_POSITION)
    call gl_delete_buffers(mesh_pointer%vbo_position)

    if (gl_is_buffer(mesh_pointer%vbo_position)) then
      error stop "[Mesh]: Failed to delete VBO [position] for mesh ID ["//int_to_string(mesh_pointer%vao)//"]"
    end if
    ! if (debug_mode) then
    !   print"(A)", "[Mesh]: Deleted VBO [position] at location["//int_to_string(gotten_mesh%vbo_position)//"]"
    ! end if

    ! Texture coordinates.
    call gl_disable_vertex_attrib_array(ATTRIBUTE_TEXTURE_COORDINATE)
    call gl_delete_buffers(mesh_pointer%vbo_texture_coordinate)

    if (gl_is_buffer(mesh_pointer%vbo_texture_coordinate)) then
      error stop "[Mesh]: Failed to delete VBO [texture_coordinate] for mesh ID ["//int_to_string(mesh_pointer%vao)//"]"
    end if
    ! if (debug_mode) then
    !   print"(A)", "[Mesh]: Deleted VBO [texture_coordinate] at location["//int_to_string(gotten_mesh%vbo_texture_coordinate)//"]"
    ! end if

    ! Colors
    call gl_disable_vertex_attrib_array(ATTRIBUTE_COLOR)
    call gl_delete_buffers(mesh_pointer%vbo_color)

    if (gl_is_buffer(mesh_pointer%vbo_color)) then
      error stop "[Mesh]: Failed to delete VBO [color] for mesh ID ["//int_to_string(mesh_pointer%vao)//"]"
    end if
    ! if (debug_mode) then
    !   print"(A)", "[Mesh]: Deleted VBO [color] at location["//int_to_string(gotten_mesh%vbo_color)//"]"
    ! end if

    ! Indices.
    call gl_delete_buffers(mesh_pointer%vbo_indices)

    if (gl_is_buffer(mesh_pointer%vbo_indices)) then
      error stop "[Mesh]: Failed to delete VBO [indices] for mesh ID ["//int_to_string(mesh_pointer%vao)//"]"
    end if
    ! if (debug_mode) then
    !   print"(A)", "[Mesh]: Deleted VBO [indices] at location["//int_to_string(gotten_mesh%vbo_indices)//"]"
    ! end if

    ! Unbind.
    call gl_bind_vertex_array(0)

    ! Then delete the VAO.
    call gl_delete_vertex_arrays(mesh_pointer%vao)
    if (gl_is_vertex_array(mesh_pointer%vao)) then
      error stop "[Mesh]: Failed to delete VAO for mesh ID ["//int_to_string(mesh_pointer%vao)//"]"
    end if

    ! Finally remove it from the database.
    if (debug_mode) then
      print"(A)", "[Mesh]: Deleted mesh ID ["//int_to_string(mesh_pointer%vao)//"]"
    end if
  end subroutine gc_mesh_database


end module mesh
