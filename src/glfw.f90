module glfw
  use, intrinsic :: iso_c_binding
  implicit none

  private

  ! Module fields.

  ! C side.
  type(c_ptr) :: c_window_pointer

  ! Fortran side.
  character(len = :, kind = c_char), allocatable :: window_title

  public :: GLFW_CONTEXT_DEBUG
  public :: GLFW_OPENGL_DEBUG_CONTEXT

  integer :: GLFW_CONTEXT_DEBUG = int(Z"22007")
  integer :: GLFW_OPENGL_DEBUG_CONTEXT = int(Z"22007")

  ! What we want exposed.

  public :: glfw_init
  public :: glfw_terminate
  public :: glfw_create_window
  public :: glfw_make_context_current
  public :: glfw_get_error
  public :: glfw_window_should_close
  public :: glfw_swap_buffers
  public :: glfw_poll_events
  public :: glfw_destroy_window
  public :: glfw_set_error_callback
  public :: glfw_window_hint

  ! Here I'm binding to the C glfw shared library.
  interface
    logical(c_bool) function glfw_init() result(success) bind(c, name="glfwInit")
      use, intrinsic :: iso_c_binding
      implicit none
    end function glfw_init

    subroutine internal_glfw_terminate() bind(c, name="glfwTerminate")
      use, intrinsic :: iso_c_binding
      implicit none
    end subroutine internal_glfw_terminate

    function internal_glfw_create_window(width, height, title, monitor, share) result(new_window_pointer) bind(c, name = "glfwCreateWindow")
      use, intrinsic :: iso_c_binding
      implicit none
      integer(c_int), intent(in), value :: width
      integer(c_int), intent(in), value :: height
      character(kind = c_char), intent(in), optional :: title
      type(c_ptr), intent(in), optional :: monitor
      type(c_ptr), intent(in), optional :: share
      type(c_ptr) :: new_window_pointer
    end function internal_glfw_create_window

    subroutine internal_glfw_make_context_current(new_window_pointer) bind(c, name = "glfwMakeContextCurrent")
      use, intrinsic :: iso_c_binding
      implicit none
      type(c_ptr), intent(in), value :: new_window_pointer
    end subroutine internal_glfw_make_context_current

    integer(c_int) function internal_glfw_get_error(char_pointer) result(error_type) bind(c, name = "glfwGetError")
      use, intrinsic :: iso_c_binding
      implicit none
      type(c_ptr), intent(in) :: char_pointer
    end function internal_glfw_get_error

    logical(c_bool) function internal_glfw_window_should_close(current_window_pointer) result(should_close) bind(c, name = "glfwWindowShouldClose")
      use, intrinsic :: iso_c_binding
      implicit none
      type(c_ptr), intent(in), value :: current_window_pointer
    end function internal_glfw_window_should_close

    subroutine internal_glfw_swap_buffers(current_window_pointer) bind(c, name = "glfwSwapBuffers")
      use, intrinsic :: iso_c_binding
      implicit none
      type(c_ptr), intent(in), value :: current_window_pointer
    end subroutine internal_glfw_swap_buffers

    subroutine internal_glfw_poll_events(current_window_pointer) bind(c, name = "glfwPollEvents")
      use, intrinsic :: iso_c_binding
      implicit none
      type(c_ptr), intent(in), value :: current_window_pointer
    end subroutine internal_glfw_poll_events

    subroutine internal_glfw_destroy_window(current_window_pointer) bind(c, name = "glfwDestroyWindow")
      use, intrinsic :: iso_c_binding
      implicit none
      type(c_ptr), intent(in), value :: current_window_pointer
    end subroutine internal_glfw_destroy_window

    subroutine internal_glfw_set_error_callback(func) bind(c, name = "glfwSetErrorCallback")
      use, intrinsic :: iso_c_binding
      implicit none
      type(c_funptr), intent(in), value :: func
    end subroutine internal_glfw_set_error_callback

    subroutine glfw_window_hint(hint, value) bind(c, name = "glfwWindowHint")
      use, intrinsic :: iso_c_binding
      implicit none
      integer(c_int), intent(in), value :: hint
      integer(c_int), intent(in), value :: value
    end subroutine glfw_window_hint

  end interface

contains

  ! Here I'm just kind of using glfw the way I want to use it.

  subroutine glfw_terminate
    use deal
    implicit none
    call internal_glfw_terminate()
    call deallocate_string(window_title)

  end subroutine glfw_terminate

  subroutine glfw_get_error
    use, intrinsic :: iso_c_binding
    use :: deal
    use :: string
    implicit none

    ! C side.
    type(c_ptr) :: c_string
    ! Fortran side.
    integer :: error_result
    character(:), allocatable :: error_result_text

    error_result = internal_glfw_get_error(c_string)

    error_result_text = string_from_c(c_string, 512)

    if (len(error_result_text) > 0) then
      print*,"GLFW Gotten Error: "//error_result_text//"."
      ! else if (error_result == 0) then
      !   print*,"no glfw error :)"
      ! else
      !   print*,error_result
    end if

    call deallocate_string(error_result_text)

    !! Calling c_free() on c_string will just crash here because this is stack memory.

  end subroutine glfw_get_error

  logical function glfw_create_window(width, height, title) result(success)
    use, intrinsic :: iso_c_binding
    use string
    implicit none
    integer(c_int) :: width
    integer(c_int) :: height
    character(len = *,kind = c_char) :: title

    window_title = into_c_string(title)

    c_window_pointer = internal_glfw_create_window(width, height, window_title, null(), null())

    ! Then we check if the window pointer is null.
    success = c_associated(c_window_pointer)
  end function glfw_create_window

  subroutine glfw_make_context_current
    implicit none
    call internal_glfw_make_context_current(c_window_pointer)
  end subroutine glfw_make_context_current

  logical function glfw_window_should_close() result(should_close)
    implicit none
    should_close = internal_glfw_window_should_close(c_window_pointer) .eqv. .true.
  end function glfw_window_should_close


  subroutine glfw_swap_buffers
    implicit none
    call internal_glfw_swap_buffers(c_window_pointer)
  end subroutine glfw_swap_buffers

  subroutine glfw_poll_events
    implicit none
    call internal_glfw_poll_events(c_window_pointer)
  end

  subroutine glfw_destroy_window
    implicit none
    call internal_glfw_destroy_window(c_window_pointer)
    print*,"GLFW: Window destroyed successfully."
  end subroutine glfw_destroy_window


  !** NOTE: C is passing Fortran data here!
  !** NOTE: This function passed into C as a pointer!
  subroutine error_callback(i, char_pointer)
    use, intrinsic :: iso_c_binding
    use string
    use deal
    implicit none

    integer(c_int), intent(in), value :: i
    type(c_ptr), intent(in), value :: char_pointer
    character(:), allocatable :: error_text
    character(:), allocatable :: error_value_string

    error_text = string_from_c(char_pointer, 512)
    error_value_string = int_to_string(i)

    if (len(error_text) > 0) then
      !? We put a period at the end because I think that looks nice.
      print*,"GLFW Error: ("//error_value_string//") "//error_text//"."
    end if

    call deallocate_string(error_text)
    call deallocate_string(error_value_string)

    !! char_pointer is on the stack. Calling c_free() on it will crash the program.

  end subroutine error_callback !! These are grouped together because they're basically one function.
  subroutine glfw_set_error_callback
    implicit none

    call internal_glfw_set_error_callback(c_funloc(error_callback))
  end subroutine glfw_set_error_callback

end module glfw
