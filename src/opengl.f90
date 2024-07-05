module opengl
  use, intrinsic :: iso_c_binding
  implicit none

  private

  ! C variables.

  ! integer :: GL_VERSION = int(Z"1f02")
  ! integer :: GL_NONE = 0
  integer :: GL_COLOR_BUFFER_BIT = int(Z"00004000")
  ! integer :: GL_UNSIGNED_BYTE = int(Z"1401")
  ! integer :: GL_EXTENSIONS = int(Z"1f03")
  ! integer :: GL_NUM_EXTENSIONS = int(Z"821d")
  ! integer :: GL_CONTEXT_FLAGS = int(Z"821e")
  ! integer :: GL_CONTEXT_FLAG_FORWARD_COMPATIBLE_BIT = int(Z"00000001")
  ! integer :: GL_CONTEXT_FLAG_DEBUG_BIT = int(Z"00000002")
  ! integer :: GL_CONTEXT_PROFILE_MASK = int(Z"9126")
  ! integer :: GL_CONTEXT_COMPATIBILITY_PROFILE_BIT = int(Z"00000002")
  ! integer :: GL_CONTEXT_CORE_PROFILE_BIT = int(Z"00000001")
  ! integer :: GL_RESET_NOTIFICATION_STRATEGY_ARB = int(Z"8256")
  ! integer :: GL_LOSE_CONTEXT_ON_RESET_ARB = int(Z"8252")
  ! integer :: GL_NO_RESET_NOTIFICATION_ARB = int(Z"8261")
  ! integer :: GL_CONTEXT_RELEASE_BEHAVIOR = int(Z"82fb")
  ! integer :: GL_CONTEXT_RELEASE_BEHAVIOR_FLUSH = int(Z"82fc")
  ! integer :: GL_CONTEXT_FLAG_NO_ERROR_BIT_KHR = int(Z"00000008")


  ! What we want exposed.

  public :: gl_clear_color_buffer
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
