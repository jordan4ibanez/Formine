!** This module kind of works like a state machine.
module mesh
  use :: string
  use :: vector_3f
  use :: fhash, only: fhash_tbl_t, key => fhash_key
  implicit none


  private


  public :: mesh_create_3d


  type(fhash_tbl_t) :: mesh_database


  type mesh_data
    integer :: vao = 0
    integer :: vbo_position = 0
    integer :: vbo_color = 0
    integer :: vbo_indices = 0
  end type mesh_data


contains


  subroutine mesh_create_3d(mesh_name)
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
    type(mesh_data) :: new_mesh

    if (.true.) then
      print"(A)",colorize_rgb("[WARNING]: remember to implement the hash table of meshes.", 255, 0, 0)
    end if

    ! print"(A)",colorize_rgb("[Mesh] WARNING: SHADER MODULE NEEDS A STATE MACHINE!", 255,128,0)

    ! Into vertex array object.

    new_mesh%vao = gl_gen_vertex_arrays()

    print"(A)","vao: ["//int_to_string(new_mesh%vao)//"]"

    call gl_bind_vertex_array(new_mesh%vao)

    ! Into position vertex buffer object.

    new_mesh%vbo_position = upload_positions([ &
      -0.5, -0.5, 0.0, &
      0.5, -0.5, 0.0, &
      0.0, 0.5, 0.0 &
      ])

    new_mesh%vbo_color = upload_colors([ &
      1.0, 0.0, 0.0, &
      0.0, 1.0, 0.0, &
      0.0, 0.0, 1.0 &
      ])

    new_mesh%vbo_indices = upload_indices([0,1,2])

    ! Now unbind vertex array object.
    call gl_bind_vertex_array(0)

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

    print"(A)","vbo position: ["//int_to_string(vbo_position)//"]"

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

    print"(A)","vbo position: ["//int_to_string(vbo_position)//"]"

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

    print"(A)","vbo color: ["//int_to_string(vbo_position)//"]"

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

    print"(A)","vbo indices: ["//int_to_string(vbo_position)//"]"

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

    print"(A)", "[Mesh]: set mesh ["//mesh_name//"]"

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
    implicit none

    character(len = *), intent(in) :: mesh_name
    
    
  end subroutine mesh_draw


end module mesh
