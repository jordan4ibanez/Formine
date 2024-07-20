module mesh
  implicit none

  private

  public :: debug_create_mesh

contains

  subroutine debug_create_mesh
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





  end subroutine debug_create_mesh



end module mesh
