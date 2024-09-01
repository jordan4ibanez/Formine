module mouse
  use :: glfw
  use, intrinsic :: iso_c_binding
  implicit none


  private


  public :: mouse_initialize
  public :: mouse_lock
  public :: mouse_debug_lock_toggle

  logical :: mouse_is_locked = .false.


contains


  !* Mouse movement events.
  subroutine mouse_position_callback(window_pointer, x_pos, y_pos)
    implicit none

    type(c_ptr), intent(in), value :: window_pointer
    real(c_double), intent(in), value :: x_pos, y_pos

    print*, x_pos, y_pos

    if (mouse_is_locked) then
      call glfw_set_cursor_pos(0.0d0, 0.0d0)
    end if
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

    ! Fixes the initial jolt.
    call glfw_set_cursor_pos(0.0d0, 0.0d0)

    call glfw_set_input_mode(GLFW_CURSOR, GLFW_CURSOR_DISABLED)
    mouse_is_locked = .true.
  end subroutine mouse_lock


  !* Unlock the mouse.
  subroutine mouse_unlock()
    implicit none

    real(c_double) window_width, window_height

    if (.not. mouse_is_locked) then
      return
    end if

    call glfw_set_input_mode(GLFW_CURSOR, GLFW_CURSOR_NORMAL)
    mouse_is_locked = .false.

    ! Set the mouse to the center of the window when unlocking.
    window_width = glfw_get_window_width_f64()
    window_height = glfw_get_window_height_f64()

    call glfw_set_cursor_pos(window_width / 2.0d0, window_height / 2.0d0)
  end subroutine mouse_unlock


  !! DEBUG PROTOTYPING ONLY !!
  !! THIS CAN CAUSE CONFUSING ISSUES DOWN THE LINE !!
  subroutine mouse_debug_lock_toggle()
    implicit none

    if (mouse_is_locked) then
      call mouse_unlock()
    else
      call mouse_lock()
    end if
  end subroutine mouse_debug_lock_toggle


end module mouse
