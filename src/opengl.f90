module opengl
  use, intrinsic :: iso_c_binding
  implicit none

  private

  ! C variables.

  public :: GL_DEBUG_OUTPUT_SYNCHRONOUS
  public :: GL_COLOR_BUFFER_BIT

  ! integer :: GL_VERSION = int(Z"1f02")
  ! integer :: GL_NONE = 0
  integer :: GL_COLOR_BUFFER_BIT = int(Z"00004000")
  ! integer :: GL_UNSIGNED_BYTE = int(Z"1401")
  integer :: GL_DEBUG_OUTPUT_SYNCHRONOUS = int(Z"8242")


  ! What we want exposed.

  public :: gl_clear_color_buffer
  public :: gl_enable
  public :: gl_clear_color


  ! Here I'm binding to the C shared library.

  interface

    subroutine internal_gl_clear(thing_to_clear) bind(c, name = "glClear")
      use, intrinsic :: iso_c_binding
      implicit none
      integer(c_int), intent(in), value :: thing_to_clear
    end subroutine internal_gl_clear

    subroutine internal_gl_clear_color(r,g,b,a) bind(c, name = "glClearColor")
      use, intrinsic :: iso_c_binding
      implicit none
      real(c_float), intent(in), value :: r
      real(c_float), intent(in), value :: g
      real(c_float), intent(in), value :: b
      real(c_float), intent(in), value :: a
    end subroutine internal_gl_clear_color

    subroutine gl_enable(cap) bind(c, name = "glEnable")
      use, intrinsic :: iso_c_binding
      implicit none
      integer(c_int) :: cap
    end subroutine gl_enable

    subroutine internal_gl_debug_message_callback(callback) bind(c, name = "glDebugMessageCallback")
      use, intrinsic :: iso_c_binding
      implicit none
      type(c_ptr), intent(in), value :: callback
    end subroutine internal_gl_debug_message_callback

  end interface

contains

  ! Here I'm just kind of using OpenGL the way I want to use it.

  subroutine gl_clear_color_buffer
    implicit none
    call internal_gl_clear(GL_COLOR_BUFFER_BIT)
  end

  subroutine gl_clear_color(r,g,b)
    implicit none
    real(c_float) :: r
    real(c_float) :: g
    real(c_float) :: b
    call internal_gl_clear_color(r,g,b,1.0)

  end subroutine gl_clear_color



end module opengl
