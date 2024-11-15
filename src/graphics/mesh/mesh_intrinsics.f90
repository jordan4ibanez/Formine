module mod_mesh_intrinsics
  use, intrinsic :: iso_c_binding
  use :: opengl
  use :: string_f90
  use :: shader
  implicit none


  logical(c_bool), parameter :: debug_mode = .false.


  type :: mesh_data
    integer(c_int) :: vao = 0
    integer(c_int) :: vbo_position = 0
    integer(c_int) :: vbo_texture_coordinate = 0
    integer(c_int) :: vbo_color = 0
    integer(c_int) :: vbo_indices = 0
    integer(c_int) :: indices_length = 0
  end type mesh_data


contains


  integer function upload_positions(position_array, vec_components) result(vbo_position)
    implicit none

    real(c_float), dimension(:), intent(in), target :: position_array
    integer(c_int), intent(in), value :: vec_components
    integer(c_int) :: position_vbo_position

    position_vbo_position = ATTRIBUTE_POSITION

    ! Create the VBO context.
    vbo_position = gl_gen_buffers()

    if (debug_mode) then
      print"(A)","vbo position: ["//int_to_string(vbo_position)//"]"
    end if

    ! Walk into the VBO context.
    call gl_bind_buffer(GL_ARRAY_BUFFER, vbo_position)

    ! Pass this data into the OpenGL state machine.
    call gl_buffer_float_array(position_array)

    ! Width = vec_components because this is vecX
    ! false because this is not normalized
    ! 0 stride
    call gl_vertex_attrib_pointer(position_vbo_position, vec_components, GL_FLOAT, .false., 0)

    ! Enable this new data.
    call gl_enable_vertex_attrib_array(position_vbo_position)

    ! Now unbind.
    call gl_bind_buffer(GL_ARRAY_BUFFER, 0)
  end function upload_positions


  integer function upload_texture_coordinate(texture_coordinates) result(vbo_position)
    implicit none

    real(c_float), dimension(:), intent(in), target :: texture_coordinates
    integer(c_int) :: texture_coordinate_vbo_position

    texture_coordinate_vbo_position = ATTRIBUTE_TEXTURE_COORDINATE

    ! Create the VBO context.
    vbo_position = gl_gen_buffers()

    if (debug_mode) then
      print"(A)","vbo texture coordinates: ["//int_to_string(vbo_position)//"]"
    end if

    ! Walk into the VBO context.
    call gl_bind_buffer(GL_ARRAY_BUFFER, vbo_position)

    ! Pass this data into the OpenGL state machine.
    call gl_buffer_float_array(texture_coordinates)

    ! Width = 2 because this is a vec2
    ! false because this is not normalized
    ! 0 stride
    call gl_vertex_attrib_pointer(texture_coordinate_vbo_position, 2, GL_FLOAT, .false., 0)

    ! Enable this new data.
    call gl_enable_vertex_attrib_array(texture_coordinate_vbo_position)

    ! Now unbind.
    call gl_bind_buffer(GL_ARRAY_BUFFER, 0)
  end function upload_texture_coordinate


  integer function upload_colors(colors) result(vbo_position)
    implicit none

    real(c_float), dimension(:), intent(in), target :: colors
    integer(c_int) :: color_vbo_position

    color_vbo_position = ATTRIBUTE_COLOR

    ! Create the VBO context.
    vbo_position = gl_gen_buffers()

    if (debug_mode) then
      print"(A)","vbo color: ["//int_to_string(vbo_position)//"]"
    end if

    ! Walk into the VBO context.
    call gl_bind_buffer(GL_ARRAY_BUFFER, vbo_position)

    ! Pass this data into the OpenGL state machine.
    call gl_buffer_float_array(colors)

    ! Width = 3 because this is a vec3
    ! false because this is not normalized
    ! 0 stride
    call gl_vertex_attrib_pointer(color_vbo_position, 3, GL_FLOAT, .false., 0)

    ! Enable this new data.
    call gl_enable_vertex_attrib_array(color_vbo_position)

    ! Now unbind.
    call gl_bind_buffer(GL_ARRAY_BUFFER, 0)
  end function upload_colors


  integer function upload_indices(indices) result(vbo_position)
    implicit none

    integer(c_int), dimension(:), intent(in), target :: indices

    ! Create the VBO context.
    vbo_position = gl_gen_buffers()

    if (debug_mode) then
      print"(A)","vbo indices: ["//int_to_string(vbo_position)//"]"
    end if

    ! Walk into the VBO context.
    call gl_bind_buffer(GL_ELEMENT_ARRAY_BUFFER, vbo_position)

    ! Upload into state machine.
    call gl_buffer_indices_array(indices)

    !! Never call this, instant segfault.
    ! call gl_bind_buffer(GL_ELEMENT_ARRAY_BUFFER, 0)
  end function upload_indices


  !* Internal component for creating a mesh.
  function mesh_create_internal(mesh_database, mesh_name_to_id_database, dimensions, positions, texture_coordinates, colors, indices, named, mesh_name) result(vao_id)
    use :: hashmap_int
    use :: hashmap_str
    use :: terminal
    implicit none

    type(hashmap_integer_key), intent(inout) :: mesh_database
    type(hashmap_string_key), intent(inout) :: mesh_name_to_id_database
    integer(c_int), intent(in), value :: dimensions
    real(c_float), dimension(:), intent(in), target :: positions, texture_coordinates, colors
    integer(c_int), dimension(:), intent(in), target :: indices
    logical, intent(in), value :: named
    character(len = *, kind = c_char), intent(in), optional :: mesh_name
    integer(c_int) :: vao_id
    type(mesh_data) :: new_mesh

    ! Into vertex array object.

    new_mesh%vao = gl_gen_vertex_arrays()

    if (debug_mode) then
      print"(A)","vao: ["//int_to_string(new_mesh%vao)//"]"
    end if

    call gl_bind_vertex_array(new_mesh%vao)

    ! Into position vertex buffer object.

    new_mesh%vbo_position = upload_positions(positions, dimensions)

    new_mesh%vbo_texture_coordinate = upload_texture_coordinate(texture_coordinates)

    new_mesh%vbo_color = upload_colors(colors)

    new_mesh%vbo_indices = upload_indices(indices)

    new_mesh%indices_length = size(indices)

    ! Now unbind vertex array object.
    call gl_bind_vertex_array(0)

    ! Now set it.

    ! Do not allow overwriting. This will cause a severe memory leak.
    if (mesh_database%has_key(int(new_mesh%vao, c_int64_t))) then
      error stop "[Mesh] Error: Tried to overwrite mesh ID ["//int_to_string(new_mesh%vao)//"]. Please delete it before setting it."
    end if

    call mesh_database%set(int(new_mesh%vao, c_int64_t), new_mesh)

    !? Only named meshes get put into the translation database.
    if (named) then

      ! Do not allow overwriting. This will cause a severe memory leak.
      if (mesh_name_to_id_database%has_key(mesh_name)) then
        error stop "[Mesh] Error: Tried to overwrite mesh ["//mesh_name//"]. Please delete it before setting it."
      end if

      call mesh_name_to_id_database%set(mesh_name, new_mesh%vao)
    end if

    vao_id = new_mesh%vao
  end function mesh_create_internal


end module mod_mesh_intrinsics
