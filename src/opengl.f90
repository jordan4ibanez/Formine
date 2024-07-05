module opengl
  use, intrinsic :: iso_c_binding
  implicit none

  private

  ! C variables.
  integer(c_int), bind(c, name = "GL_COLOR_BUFFER_BIT") :: GL_COLOR_BUFFER_BIT

  ! What we want exposed.

  public :: clear_color_buffer


  interface

    subroutine internal_gl_clear(thing) bind(c, name = "glClear")
      use, intrinsic :: iso_c_binding
      implicit none
      integer(c_int) :: thing
    end

  end interface

contains

  subroutine clear_color_buffer
    call internal_gl_clear(GL_COLOR_BUFFER_BIT)
  end



end module opengl
