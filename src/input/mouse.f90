module mouse
  use :: glfw
  use, intrinsic :: iso_c_binding
  implicit none


  private


  public :: mouse_initialize
  public :: mouse_lock

  logical :: mouse_is_locked = .false.


contains

  !* Mouse movement events.
  subroutine mouse_position_callback(window_pointer, x_pos, y_pos)
    implicit none

    type(c_ptr), intent(in), value :: window_pointer
    real(c_double), intent(in), value :: x_pos, y_pos

    print*, x_pos, y_pos
  end subroutine mouse_position_callback


  !* This initializes the keyboard callback function.
  !* This must be called after GLFW is initialized.
  subroutine mouse_initialize()
    implicit none

    call glfw_set_cursor_pos_callback(c_funloc(mouse_position_callback))
  end subroutine mouse_initialize


  !* Lock the mouse into the window with infinite movement.
  subroutine mouse_lock()
    implicit none

    if (mouse_is_locked) then
      return
    end if

    call glfw_set_input_mode(GLFW_CURSOR, GLFW_CURSOR_DISABLED)
    mouse_is_locked = .true.
  end subroutine mouse_lock


  !* Unlock the mouse.
  subroutine mouse_unlock()
    implicit none

    if (.not. mouse_is_locked) then
      return
    end if

    call glfw_set_input_mode(GLFW_CURSOR, GLFW_CURSOR_NORMAL)
    mouse_is_locked = .false.
  end subroutine mouse_unlock


end module mouse
