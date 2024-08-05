!* This module kind of works like a state machine.
module mesh
  use :: string
  use :: vector_3f
  use :: fhash, only: fhash_tbl_t, key => fhash_key
  use, intrinsic :: iso_c_binding, only: c_float, c_int
  implicit none


  private


  public :: mesh_create_3d
  public :: mesh_draw
  public :: mesh_delete
  public :: mesh_exists
  public :: mesh_clear_database


  logical, parameter :: debug_mode = .false.
  type(fhash_tbl_t) :: mesh_database


  type mesh_data
    integer :: vao = 0
    integer :: vbo_position = 0
    integer :: vbo_texture_coordinate = 0
    integer :: vbo_color = 0
    integer :: vbo_indices = 0
    integer :: indices_length = 0
  end type mesh_data


contains


  subroutine mesh_create_3d(shader_name, mesh_name, positions, texture_coordinates, colors, indices)
    use :: shader
    use :: opengl
    use :: string
    use :: terminal
    use :: terminal
    implicit none
    ! Notes:

    ! 2. make a type for a mesh.
    ! 3. make this handle mesh things.
    ! 4. improve, somehow.

    character(len = *), intent(in) :: shader_name, mesh_name
    real(c_float), dimension(:), intent(in), target :: positions, texture_coordinates, colors
    integer(c_int), dimension(:), intent(in), target :: indices
    real(c_float), dimension(:), pointer :: positions_pointer, texture_coordinates_pointer, colors_pointer
    integer(c_int), dimension(:), pointer :: indices_pointer
    type(mesh_data), pointer :: new_mesh

    ! Set up our memory here.
    allocate(new_mesh)
    positions_pointer => positions
    texture_coordinates_pointer => texture_coordinates
    colors_pointer => colors
    indices_pointer => indices

    ! Into vertex array object.

    new_mesh%vao = gl_gen_vertex_arrays()

    if (debug_mode) then
      print"(A)","vao: ["//int_to_string(new_mesh%vao)//"]"
    end if

    call gl_bind_vertex_array(new_mesh%vao)

    ! Into position vertex buffer object.

    new_mesh%vbo_position = upload_positions(shader_name, positions_pointer, 3)

    new_mesh%vbo_texture_coordinate = upload_texture_coordinate(shader_name, texture_coordinates_pointer)

    new_mesh%vbo_color = upload_colors(colors_pointer)

    new_mesh%vbo_indices = upload_indices(indices_pointer)

    new_mesh%indices_length = size(indices)

    ! Now unbind vertex array object.
    call gl_bind_vertex_array(0)

    ! Finally, upload into the database.
    call set_mesh(mesh_name, new_mesh)
  end subroutine mesh_create_3d


  integer function upload_positions(shader_name, position_array_pointer, vec_components) result(vbo_position)
    use, intrinsic :: iso_c_binding
    use :: opengl
    use :: shader
    implicit none

    character(len = *), intent(in) :: shader_name
    real(c_float), dimension(:), intent(in), pointer :: position_array_pointer
    integer(c_int), intent(in), value :: vec_components
    integer :: position_vbo_position

    position_vbo_position = shader_get_attribute(shader_name, "position")

    ! Create the VBO context.
    vbo_position = gl_gen_buffers()

    if (debug_mode) then
      print"(A)","vbo position: ["//int_to_string(vbo_position)//"]"
    end if

    ! Walk into the VBO context.
    call gl_bind_buffer(GL_ARRAY_BUFFER, vbo_position)

    ! Pass this data into the OpenGL state machine.
    call gl_buffer_float_array(position_array_pointer)

    ! Width = vec_components because this is vecX
    ! false because this is not normalized
    ! 0 stride
    call gl_vertex_attrib_pointer(position_vbo_position, vec_components, GL_FLOAT, .false., 0)

    ! Enable this new data.
    call gl_enable_vertex_attrib_array(position_vbo_position)

    ! Now unbind.
    call gl_bind_buffer(GL_ARRAY_BUFFER, 0)
  end function upload_positions


  integer function upload_texture_coordinate(shader_name, texture_coordinates_pointer) result(vbo_position)
    use, intrinsic :: iso_c_binding
    use :: opengl
    use :: shader
    implicit none

    character(len = *), intent(in) :: shader_name
    real(c_float), dimension(:), intent(in), pointer :: texture_coordinates_pointer
    integer :: texture_coordinate_vbo_position

    texture_coordinate_vbo_position = shader_get_attribute(shader_name, "texture_coordinate")

    ! Create the VBO context.
    vbo_position = gl_gen_buffers()

    if (debug_mode) then
      print"(A)","vbo texture coordinates: ["//int_to_string(vbo_position)//"]"
    end if

    ! Walk into the VBO context.
    call gl_bind_buffer(GL_ARRAY_BUFFER, vbo_position)

    ! Pass this data into the OpenGL state machine.
    call gl_buffer_float_array(texture_coordinates_pointer)

    ! Width = 2 because this is a vec2
    ! false because this is not normalized
    ! 0 stride
    call gl_vertex_attrib_pointer(texture_coordinate_vbo_position, 2, GL_FLOAT, .false., 0)

    ! Enable this new data.
    call gl_enable_vertex_attrib_array(texture_coordinate_vbo_position)

    ! Now unbind.
    call gl_bind_buffer(GL_ARRAY_BUFFER, 0)
  end function upload_texture_coordinate


  integer function upload_colors(colors_pointer) result(vbo_position)
    use, intrinsic :: iso_c_binding
    use :: opengl
    use :: shader
    implicit none

    real(c_float), dimension(:), intent(in), pointer :: colors_pointer
    integer :: color_vbo_position

    color_vbo_position = shader_get_attribute("main", "color")

    ! Create the VBO context.
    vbo_position = gl_gen_buffers()

    if (debug_mode) then
      print"(A)","vbo color: ["//int_to_string(vbo_position)//"]"
    end if

    ! Walk into the VBO context.
    call gl_bind_buffer(GL_ARRAY_BUFFER, vbo_position)

    ! Pass this data into the OpenGL state machine.
    call gl_buffer_float_array(colors_pointer)

    ! Width = 3 because this is a vec3
    ! false because this is not normalized
    ! 0 stride
    call gl_vertex_attrib_pointer(color_vbo_position, 3, GL_FLOAT, .false., 0)

    ! Enable this new data.
    call gl_enable_vertex_attrib_array(color_vbo_position)

    ! Now unbind.
    call gl_bind_buffer(GL_ARRAY_BUFFER, 0)

  end function upload_colors


  integer function upload_indices(indices_pointer) result(vbo_position)
    use, intrinsic :: iso_c_binding
    use :: opengl
    use :: shader
    implicit none

    integer(c_int), dimension(:), intent(in), pointer :: indices_pointer

    ! Create the VBO context.
    vbo_position = gl_gen_buffers()

    if (debug_mode) then
      print"(A)","vbo indices: ["//int_to_string(vbo_position)//"]"
    end if

    ! Walk into the VBO context.
    call gl_bind_buffer(GL_ELEMENT_ARRAY_BUFFER, vbo_position)

    ! Upload into state machine.
    call gl_buffer_indices_array(indices_pointer)

    !! Never call this, instant segfault.
    ! call gl_bind_buffer(GL_ELEMENT_ARRAY_BUFFER, 0)

  end function upload_indices


  !* Set or update a shader in the database.
  subroutine set_mesh(mesh_name, new_mesh)
    implicit none

    character(len = *), intent(in) :: mesh_name
    type(mesh_data), intent(in), pointer :: new_mesh

    ! This creates an enforcement where the mesh must be deleted before it can be re-assigned.
    ! This prevents a severe memory leak.
    if (mesh_exists(mesh_name)) then
      error stop "[Mesh] Error: Tried to overwrite mesh ["//mesh_name//"]. Please delete it before setting it."
    end if

    if (debug_mode) then
      print"(A)", "[Mesh]: set mesh ["//mesh_name//"]"
    end if

    call mesh_database%set_ptr(key(mesh_name), new_mesh)
  end subroutine set_mesh


  !* Get a mesh from the hash table.
  !* The mesh is a clone. To update, set_mesh().
  function get_mesh(mesh_name, exists) result(gotten_mesh)
    use :: terminal
    implicit none

    character(len = *), intent(in) :: mesh_name
    logical, intent(inout) :: exists
    class(*), pointer :: generic_data
    integer :: status
    type(mesh_data), pointer :: gotten_mesh

    exists = .false.

    call mesh_database%get_raw_ptr(key(mesh_name), generic_data, stat = status)

    if (status /= 0) then
      print"(A)",colorize_rgb("[Mesh] Error: ["//mesh_name//"] does not exist.", 255, 0, 0)
      return
    end if

    select type(generic_data)
     type is (mesh_data)
      exists = .true.
      gotten_mesh => generic_data
     class default
      error stop colorize_rgb("[Mesh] Error: ["//mesh_name//"] has the wrong type.", 255, 0, 0)
    end select
  end function get_mesh


  !* Draw a mesh.
  subroutine mesh_draw(mesh_name)
    use :: terminal
    use :: opengl
    implicit none

    character(len = *), intent(in) :: mesh_name
    type(mesh_data) :: gotten_mesh
    logical :: exists

    gotten_mesh = get_mesh(mesh_name, exists)

    if (.not. exists) then
      print"(A)", colorize_rgb("[Mesh] Error: Mesh ["//mesh_name//"] does not exist. Cannot draw.", 255, 0, 0)
      return
    end if

    call gl_bind_vertex_array(gotten_mesh%vao)

    call gl_draw_elements(GL_TRIANGLES, gotten_mesh%indices_length, GL_UNSIGNED_INT)

    call gl_bind_vertex_array(0)
  end subroutine mesh_draw


  !* Delete a mesh.
  subroutine mesh_delete(mesh_name)
    use :: opengl
    use :: shader
    use :: terminal
    implicit none

    character(len = *), intent(in) :: mesh_name
    class(*), pointer :: generic
    integer :: status
    type(mesh_data), pointer :: gotten_mesh

    ! This wipes out the OpenGL memory as well or else there's going to be a massive memory leak.
    ! This is written so it can be used for set_mesh to auto delete the old mesh.

    call mesh_database%get_raw_ptr(key(mesh_name), generic, stat = status)

    if (status /= 0) then
      print"(A)",colorize_rgb("[Mesh] Error: Mesh ["//mesh_name//"] does not exist. Cannot delete.", 255, 0, 0)
      return
    end if

    select type(generic)
     type is (mesh_data)
      gotten_mesh => generic
     class default
      print"(A)",colorize_rgb("[Mesh] Error: ["//mesh_name//"] has the wrong type.", 255, 0, 0)
      return
    end select

    call gl_bind_vertex_array(gotten_mesh%vao)

    ! Positions.
    call gl_disable_vertex_attrib_array(shader_get_attribute("main", "position"))
    call gl_delete_buffers(gotten_mesh%vbo_position)

    if (gl_is_buffer(gotten_mesh%vbo_position)) then
      error stop "[Mesh]: Failed to delete VBO [position] for mesh ["//mesh_name//"]"
    end if
    ! if (debug_mode) then
    !   print"(A)", "[Mesh]: Deleted VBO [position] at location["//int_to_string(gotten_mesh%vbo_position)//"]"
    ! end if

    ! Texture coordinates.
    call gl_disable_vertex_attrib_array(shader_get_attribute("main", "texture_coordinate"))
    call gl_delete_buffers(gotten_mesh%vbo_texture_coordinate)

    if (gl_is_buffer(gotten_mesh%vbo_texture_coordinate)) then
      error stop "[Mesh]: Failed to delete VBO [texture_coordinate] for mesh ["//mesh_name//"]"
    end if
    ! if (debug_mode) then
    !   print"(A)", "[Mesh]: Deleted VBO [texture_coordinate] at location["//int_to_string(gotten_mesh%vbo_texture_coordinate)//"]"
    ! end if

    ! Colors
    call gl_disable_vertex_attrib_array(shader_get_attribute("main", "color"))
    call gl_delete_buffers(gotten_mesh%vbo_color)

    if (gl_is_buffer(gotten_mesh%vbo_color)) then
      error stop "[Mesh]: Failed to delete VBO [color] for mesh ["//mesh_name//"]"
    end if
    ! if (debug_mode) then
    !   print"(A)", "[Mesh]: Deleted VBO [color] at location["//int_to_string(gotten_mesh%vbo_color)//"]"
    ! end if

    ! Indices.
    call gl_delete_buffers(gotten_mesh%vbo_indices)

    if (gl_is_buffer(gotten_mesh%vbo_indices)) then
      error stop "[Mesh]: Failed to delete VBO [indices] for mesh ["//mesh_name//"]"
    end if
    ! if (debug_mode) then
    !   print"(A)", "[Mesh]: Deleted VBO [indices] at location["//int_to_string(gotten_mesh%vbo_indices)//"]"
    ! end if

    ! Unbind.
    call gl_bind_vertex_array(0)

    ! Then delete the VAO.
    call gl_delete_vertex_arrays(gotten_mesh%vao)
    if (gl_is_vertex_array(gotten_mesh%vao)) then
      error stop "[Mesh]: Failed to delete VAO for mesh ["//mesh_name//"]"
    end if

    ! We are working with manual memory management, we must manually deallocate it.
    deallocate(gotten_mesh)

    ! Finally remove it from the database.
    call mesh_database%unset(key(mesh_name))
    if (debug_mode) then
      print"(A)", "[Mesh]: Deleted mesh ["//mesh_name//"]"
    end if
  end subroutine mesh_delete


  !* Check if a mesh exists.
  logical function mesh_exists(mesh_name) result(existence)
    implicit none

    character(len = *), intent(in) :: mesh_name
    integer :: status

    call mesh_database%check_key(key(mesh_name), stat = status)

    existence = status == 0
  end function mesh_exists


  !* Completely wipe out all existing meshes. This might be slow.
  subroutine mesh_clear_database()
    use :: fhash, only: fhash_iter_t, fhash_key_t
    use :: string
    use :: terminal
    implicit none

    type(heap_string), dimension(:), allocatable :: key_array
    type(fhash_iter_t) :: iterator
    class(fhash_key_t), allocatable :: generic_key
    class(*), allocatable :: generic_placeholder
    integer :: i
    integer :: remaining_size

    ! Start with a size of 0.
    allocate(key_array(0))

    ! Create the iterator.
    iterator = fhash_iter_t(mesh_database)

    ! Now we will collect the keys from the iterator.
    do while(iterator%next(generic_key, generic_placeholder))
      ! Appending. Allocatable will clean up the old data.
      key_array = [key_array, heap_string_array(generic_key%to_string())]
    end do

    ! Now clear the database out.
    do i = 1,size(key_array)
      call mesh_delete(key_array(i)%get())
    end do

    !* We will always check that the remaining size is 0. This will protect us from random issues.
    call mesh_database%stats(num_items = remaining_size)

    if (remaining_size /= 0) then
      print"(A)", colorize_rgb("[Mesh] Error: Did not delete all meshes! Expected size: [0] | Actual: ["//int_to_string(remaining_size)//"]", 255, 0, 0)
    else
      print"(A)", "[Mesh]: Successfully cleared the mesh database."
    end if
  end subroutine mesh_clear_database


end module mesh
