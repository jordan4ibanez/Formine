!** This module kind of works like a state machine.
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


  logical, parameter :: debug_mode = .false.
  type(fhash_tbl_t) :: mesh_database


  type mesh_data
    integer :: vao = 0
    integer :: vbo_position = 0
    integer :: vbo_color = 0
    integer :: vbo_indices = 0
    integer :: indices_length = 0
  end type mesh_data


contains


  subroutine mesh_create_3d(mesh_name, positions, colors, indices)
    use :: shader
    use :: opengl
    use :: string
    use :: terminal
    use :: terminal
    implicit none
    ! Notes:
    ! 1. break this up into functions.
    ! 2. make a type for a mesh.
    ! 3. make this handle mesh things.
    ! 4. improve, somehow.

    character(len = *), intent(in) :: mesh_name
    real(c_float), dimension(:), intent(in) :: positions, colors
    integer(c_int), dimension(:), intent(in) :: indices
    type(mesh_data) :: new_mesh

    ! print"(A)",colorize_rgb("[Mesh] WARNING: SHADER MODULE NEEDS A STATE MACHINE!", 255,128,0)

    ! Into vertex array object.

    new_mesh%vao = gl_gen_vertex_arrays()

    if (debug_mode) then
      print"(A)","vao: ["//int_to_string(new_mesh%vao)//"]"
    end if

    call gl_bind_vertex_array(new_mesh%vao)

    ! Into position vertex buffer object.

    new_mesh%vbo_position = upload_positions(positions)

    new_mesh%vbo_color = upload_colors(colors)

    new_mesh%vbo_indices = upload_indices(indices)

    new_mesh%indices_length = size(indices)

    ! Now unbind vertex array object.
    call gl_bind_vertex_array(0)

    ! Finally, upload into the database.
    call set_mesh(mesh_name, new_mesh)
  end subroutine mesh_create_3d


  integer function upload_positions(position_array) result(vbo_position)
    use, intrinsic :: iso_c_binding
    use :: opengl
    use :: shader
    implicit none

    real(c_float), dimension(:), intent(in) :: position_array
    integer :: position_vbo_position

    position_vbo_position = shader_get_attribute("main", "position")

    ! Create the VBO context.
    vbo_position = gl_gen_buffers()

    if (debug_mode) then
      print"(A)","vbo position: ["//int_to_string(vbo_position)//"]"
    end if

    ! Walk into the VBO context.
    call gl_bind_buffer(GL_ARRAY_BUFFER, vbo_position)

    ! Pass this data into the OpenGL state machine.
    call gl_buffer_float_array(position_array)

    ! Width = 3 because this is a vec3
    ! false because this is not normalized
    ! 0 stride
    call gl_vertex_attrib_pointer(position_vbo_position, 3, GL_FLOAT, .false., 0)

    ! Enable this new data.
    call gl_enable_vertex_attrib_array(position_vbo_position)

    ! Now unbind.
    call gl_bind_buffer(GL_ARRAY_BUFFER, 0)
  end function upload_positions


  integer function upload_positions_vec3f(position_array) result(vbo_position)
    use, intrinsic :: iso_c_binding
    use :: opengl
    use :: shader
    implicit none

    type(vec3f), dimension(:), intent(in) :: position_array
    integer :: position_vbo_position

    position_vbo_position = shader_get_attribute("main", "position")

    ! Create the VBO context.
    vbo_position = gl_gen_buffers()

    if (debug_mode) then
      print"(A)","vbo position: ["//int_to_string(vbo_position)//"]"
    end if

    ! Walk into the VBO context.
    call gl_bind_buffer(GL_ARRAY_BUFFER, vbo_position)

    ! Pass this data into the OpenGL state machine.
    call gl_buffer_vec3f_array(position_array)

    ! Width = 3 because this is a vec3
    ! false because this is not normalized
    ! 0 stride
    call gl_vertex_attrib_pointer(position_vbo_position, 3, GL_FLOAT, .false., 0)

    ! Enable this new data.
    call gl_enable_vertex_attrib_array(position_vbo_position)

    ! Now unbind.
    call gl_bind_buffer(GL_ARRAY_BUFFER, 0)
  end function upload_positions_vec3f


  integer function upload_colors(color_array) result(vbo_position)
    use, intrinsic :: iso_c_binding
    use :: opengl
    use :: shader
    implicit none

    real(c_float), dimension(:), intent(in) :: color_array
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
    call gl_buffer_float_array(color_array)

    ! Width = 3 because this is a vec3
    ! false because this is not normalized
    ! 0 stride
    call gl_vertex_attrib_pointer(color_vbo_position, 3, GL_FLOAT, .false., 0)

    ! Enable this new data.
    call gl_enable_vertex_attrib_array(color_vbo_position)

    ! Now unbind.
    call gl_bind_buffer(GL_ARRAY_BUFFER, 0)

  end function upload_colors


  integer function upload_indices(indices_array) result(vbo_position)
    use, intrinsic :: iso_c_binding
    use :: opengl
    use :: shader
    implicit none

    integer(c_int), dimension(:), intent(in) :: indices_array

    ! Create the VBO context.
    vbo_position = gl_gen_buffers()

    if (debug_mode) then
      print"(A)","vbo indices: ["//int_to_string(vbo_position)//"]"
    end if

    ! Walk into the VBO context.
    call gl_bind_buffer(GL_ELEMENT_ARRAY_BUFFER, vbo_position)

    ! Upload into state machine.
    call gl_buffer_indices_array(indices_array)

    !! Never call this, instant segfault.
    ! call gl_bind_buffer(GL_ELEMENT_ARRAY_BUFFER, 0)

  end function upload_indices


  !** Set or update a shader in the database.
  subroutine set_mesh(mesh_name, new_mesh)
    implicit none

    character(len = *), intent(in) :: mesh_name
    type(mesh_data), intent(in) :: new_mesh

    call mesh_delete(mesh_name)

    if (debug_mode) then
      print"(A)", "[Mesh]: set mesh ["//mesh_name//"]"
    end if

    call mesh_database%set(key(mesh_name), new_mesh)
  end subroutine set_mesh


  !** Get a mesh from the hash table.
  !** The mesh is a clone. To update, set_mesh().
  type(mesh_data) function get_mesh(mesh_name, exists) result(gotten_mesh)
    implicit none

    character(len = *), intent(in) :: mesh_name
    logical, intent(inout) :: exists
    class(*), allocatable :: generic
    integer :: status

    exists = .false.

    call mesh_database%get_raw(key(mesh_name), generic, stat = status)

    if (status /= 0) then
      ! print"(A)","[Mesh] Error: ["//mesh_name//"] does not exist."
      return
    end if

    select type(generic)
     type is (mesh_data)
      exists = .true.
      gotten_mesh = generic
     class default
      ! print"(A)","[Mesh] Error: ["//mesh_name//"] has the wrong type."
      return
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
      print"(A)", colorize_rgb("[Mesh] Error: Mesh ["//mesh_name//"] does not exist. Will not draw.", 255, 0, 0)
      return
    end if

    call gl_bind_vertex_array(gotten_mesh%vao)

    call gl_draw_elements(GL_TRIANGLES, gotten_mesh%indices_length, GL_UNSIGNED_INT)

    call gl_bind_vertex_array(0)
  end subroutine mesh_draw


  !* Delete a mesh.
  subroutine internal_mesh_delete(mesh_name)
    use :: opengl
    use :: shader
    implicit none

    character(len = *), intent(in) :: mesh_name
    class(*), allocatable :: generic
    integer :: status
    type(mesh_data) :: gotten_mesh

    ! This wipes out the OpenGL memory as well or else there's going to be a massive memory leak.
    ! This is written so it can be used for set_mesh to auto delete the old mesh.

    call mesh_database%get_raw(key(mesh_name), generic, stat = status)

    if (status /= 0) then
      print"(A)", "[Mesh]: Mesh ["//mesh_name//"] does not exist. Cannot delete."
      return
    end if

    select type(generic)
     type is (mesh_data)
      gotten_mesh = generic
     class default
      ! print"(A)","[Mesh] Error: ["//mesh_name//"] has the wrong type."
      return
    end select

    call gl_bind_vertex_array(gotten_mesh%vao)

    ! Positions.
    call gl_disable_vertex_attrib_array(shader_get_attribute("main", "position"))
    call gl_delete_buffers(gotten_mesh%vbo_position)

    if (gl_is_buffer(gotten_mesh%vbo_position)) then
      error stop "[Mesh]: Failed to delete VBO [position] for mesh ["//mesh_name//"]"
    end if

    ! Colors
    call gl_disable_vertex_attrib_array(shader_get_attribute("main", "color"))
    call gl_delete_buffers(gotten_mesh%vbo_color)

    if (gl_is_buffer(gotten_mesh%vbo_color)) then
      error stop "[Mesh]: Failed to delete VBO [color] for mesh ["//mesh_name//"]"
    end if

    ! Indices.
    call gl_delete_buffers(gotten_mesh%vbo_indices)
    if (gl_is_buffer(gotten_mesh%vbo_indices)) then
      error stop "[Mesh]: Failed to delete VBO [indices] for mesh ["//mesh_name//"]"
    end if

    ! Unbind.
    call gl_bind_vertex_array(0)

    ! Then delete.
    call gl_delete_vertex_arrays(gotten_mesh%vao)
    if (gl_is_vertex_array(gotten_mesh%vao)) then
      error stop "[Mesh]: Failed to delete VAO for mesh ["//mesh_name//"]"
    end if

    call mesh_database%unset(key(mesh_name))
  end subroutine internal_mesh_delete


end module mesh
