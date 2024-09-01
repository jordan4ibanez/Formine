module mouse
  use :: glfw
  use, intrinsic :: iso_c_binding
  implicit none


  private


  public :: mouse_initialize
  public :: mouse_lock


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

  end subroutine mouse_lock


end module mouse
