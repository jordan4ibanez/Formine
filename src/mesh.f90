module mesh
  implicit none

  private

  public :: debug_create_mesh

contains

  subroutine debug_create_mesh
    use opengl
    implicit none

    integer :: vao
    integer :: positions

    vao = gl_gen_vertex_arrays()

    print*,"vao: ",vao

    call gl_bind_vertex_array(vao)

    positions = gl_gen_buffers()

    print*,"positions:", positions



  end subroutine debug_create_mesh



end module mesh
