module mesh
  implicit none

  private

  public :: debug_create_mesh

contains

  subroutine debug_create_mesh
    use opengl
    implicit none

    integer :: testing

    testing =  gl_gen_vertex_arrays()

    print*,testing

  end subroutine debug_create_mesh



end module mesh
