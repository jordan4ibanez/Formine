module glfw
  use, intrinsic :: iso_c_binding
  implicit none

  private

  ! Module fields.
  type(c_ptr) :: window_pointer

  ! What we want exposed.
  public :: test_things

  public :: glfw_init
  public :: glfw_terminate
  public :: glfw_create_window
  public :: glfw_make_context_current
  public :: glfw_window_should_close
  public :: glfw_swap_buffers
  public :: glfw_poll_events
  public :: glfw_destroy_window
  public :: glfw_get_error

  public :: clear_color_buffer

  ! C variables.
  integer(c_int), bind(c, name = "GL_COLOR_BUFFER_BIT") :: GL_COLOR_BUFFER_BIT

  ! Here I'm binding to the C glfw shared library.
  interface


    logical(c_bool) function glfw_init() result(success) bind(c, name="glfwInit")
      use, intrinsic :: iso_c_binding
      implicit none
    end function glfw_init

    subroutine glfw_terminate() bind(c, name="glfwTerminate")
      use, intrinsic :: iso_c_binding
      implicit none
    end subroutine glfw_terminate


    type(c_ptr) function internal_glfw_create_window(width, height, title, monitor, share) result(new_window_pointer) bind(c, name = "glfwCreateWindow")
      use, intrinsic :: iso_c_binding
      implicit none
      integer(c_int) :: width
      integer(c_int) :: height
      character(kind = c_char) :: title
      type(c_ptr), optional :: monitor
      type(c_ptr), optional :: share
    end function internal_glfw_create_window

    subroutine internal_glfw_make_context_current(new_window_pointer) bind(c, name = "glfwMakeContextCurrent")
      use, intrinsic :: iso_c_binding
      implicit none
      type(c_ptr), optional :: new_window_pointer
    end subroutine internal_glfw_make_context_current

    logical(c_bool) function internal_glfw_window_should_close(current_window_pointer) result(should_close) bind(c, name = "glfwWindowShouldClose")
      use, intrinsic :: iso_c_binding
      implicit none
      type(c_ptr), optional :: current_window_pointer
    end function internal_glfw_window_should_close

    subroutine internal_glfw_swap_buffers(current_window_pointer) bind(c, name = "glfwSwapBuffers")
      use, intrinsic :: iso_c_binding
      implicit none
      type(c_ptr), optional :: current_window_pointer
    end subroutine internal_glfw_swap_buffers

    subroutine internal_glfw_poll_events(current_window_pointer) bind(c, name = "glfwPollEvents")
      use, intrinsic :: iso_c_binding
      implicit none
      type(c_ptr), optional :: current_window_pointer
    end subroutine internal_glfw_poll_events

    subroutine internal_glfw_destroy_window(current_window_pointer) bind(c, name = "glfwDestroyWindow")
      use, intrinsic :: iso_c_binding
      implicit none
      type(c_ptr), optional :: current_window_pointer
    end subroutine internal_glfw_destroy_window

    integer(c_int) function glfw_get_error(char_pointer) result(error_type) bind(c, name = "glfwDestroyWindow")
      use, intrinsic :: iso_c_binding
      implicit none
      type(c_ptr) :: char_pointer
    end function glfw_get_error

    ! TODO: OpenGL needs to be it's own module !

    subroutine internal_gl_clear(thing) bind(c, name = "glClear")
      use, intrinsic :: iso_c_binding
      implicit none
      integer(c_int) :: thing
    end

  end interface

contains

  ! So here I'm just kind of using glfw the way I want to use it.

  logical function glfw_create_window(width, height, title) result(success)
    use, intrinsic :: iso_c_binding
    implicit none
    integer(c_int) :: width
    integer(c_int) :: height
    character(kind = c_char) :: title
    window_pointer = internal_glfw_create_window(width, height, title, null(), null())
    print "('Window address: ', I0)",transfer(window_pointer, 0_c_long)
    ! Then we check if the window pointer is null.
    success = c_associated(window_pointer)
  end

  subroutine glfw_make_context_current
    implicit none
    call internal_glfw_make_context_current(window_pointer)
  end subroutine glfw_make_context_current

  logical function glfw_window_should_close() result(should_close)
    implicit none
    should_close = internal_glfw_window_should_close(window_pointer) .eqv. .true.
  end function glfw_window_should_close

  subroutine glfw_swap_buffers
    implicit none
    call internal_glfw_swap_buffers(window_pointer)
  end subroutine glfw_swap_buffers

  subroutine glfw_poll_events
    implicit none
    call internal_glfw_poll_events(window_pointer)
  end

  subroutine glfw_destroy_window
    implicit none
    call internal_glfw_destroy_window(window_pointer)
  end subroutine glfw_destroy_window

  ! TODO: GL functions need to go in the GL module !

  subroutine test_things
    print *,GL_COLOR_BUFFER_BIT
  end subroutine test_things

  subroutine clear_color_buffer
    call internal_gl_clear(GL_COLOR_BUFFER_BIT)
  end




end module glfw
