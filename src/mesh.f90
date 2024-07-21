!** This module kind of works like a state machine.
module mesh
  implicit none

  private

  public :: create_mesh

contains

  subroutine create_mesh
    use shader
    use opengl
    implicit none
    ! Notes:
    ! 1. break this up into functions.
    ! 2. make a type for a mesh.
    ! 3. make this handle mesh things.
    ! 4. improve, somehow.

    integer :: vao
    integer :: vbo_positions

    ! Into vertex array object.

    vao = gl_gen_vertex_arrays()

    print*,"vao: ",vao

    call gl_bind_vertex_array(vao)

    ! Into position vertex buffer object.

    vbo_positions = gl_gen_buffers()

    print*,"positions:", vbo_positions

    call gl_bind_buffer(GL_ARRAY_BUFFER, vbo_positions)

    call gl_buffer_float_array([ &
      0.0, 0.0, 0.0, &
      10.0, 0.0, 0.0, &
      10.0, 0.0, 10.0 &
      ])

    ! call gl_enable_vertex_attrib_array(vbo_positions)

    ! Width = 3 because this is a vec3
    ! false because this is not normalized
    ! 0 stride
    call gl_vertex_attrib_pointer(shader_get_attribute("main", "position"), 3, GL_FLOAT, .false., 0)

    call gl_enable_vertex_attrib_array(shader_get_attribute("main", "position"))

    ! Now unbind.
    call gl_bind_buffer(GL_ARRAY_BUFFER, 0)

  end subroutine create_mesh



end module mesh
