module mod_mesh_intrinsics
  use, intrinsic :: iso_c_binding
  use :: opengl
  use :: string
  use :: shader
  implicit none


  logical(c_bool), parameter :: debug_mode = .false.


contains


  integer function upload_positions(position_array_pointer, vec_components) result(vbo_position)
    implicit none

    real(c_float), dimension(:), intent(in), target :: position_array_pointer
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


  integer function upload_texture_coordinate(texture_coordinates_pointer) result(vbo_position)
    implicit none

    real(c_float), dimension(:), intent(in), target :: texture_coordinates_pointer
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
    implicit none

    real(c_float), dimension(:), intent(in), target :: colors_pointer
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
    implicit none

    integer(c_int), dimension(:), intent(in), target :: indices_pointer

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

end module mod_mesh_intrinsics
