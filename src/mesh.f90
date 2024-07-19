module mesh
  implicit none

  private

  public :: debug_gl_array

contains

  subroutine debug_gl_array
    use opengl
    implicit none

    integer :: testing

    testing =  gl_gen_vertex_arrays()

    print*,testing

  end subroutine debug_gl_array



end module mesh
