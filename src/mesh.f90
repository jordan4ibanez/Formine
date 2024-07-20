module mesh
  implicit none

  private

  public :: debug_create_mesh

contains

  subroutine debug_create_mesh
    use opengl
    implicit none

    integer :: vao

    vao =  gl_gen_vertex_arrays()

    print*,"vao: ",vao

    call gl_bind_vertex_array(vao)


  end subroutine debug_create_mesh



end module mesh
