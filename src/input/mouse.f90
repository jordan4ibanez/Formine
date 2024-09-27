module mouse
  use :: glfw
  use :: vector_2d
  use, intrinsic :: iso_c_binding
  implicit none


  private


  public :: mouse_initialize
  public :: mouse_lock
  public :: mouse_get_delta
  public :: mouse_update
  public :: mouse_debug_lock_toggle


  logical :: mouse_is_locked = .false.
  type(vec2d) :: mouse_delta
  real(c_double) :: mouse_sensitivity = 0.0025d0


contains


  !* Mouse movement events.
  subroutine mouse_position_callback(window_pointer, x_pos, y_pos)
    implicit none

    type(c_ptr), intent(in), value :: window_pointer
    real(c_double), intent(in), value :: x_pos, y_pos

    if (.false.) then
      print*,window_pointer
    end if

    if (mouse_is_locked) then
      call calculate_mouse_delta(x_pos, y_pos)

      call glfw_set_cursor_pos(0.0d0, 0.0d0)
    end if
  end subroutine mouse_position_callback


  !* Calculate the mouse delta for camera movement.
  subroutine calculate_mouse_delta(x_pos, y_pos)
    implicit none

    real(c_double), intent(in), value :: x_pos, y_pos

    mouse_delta%x = x_pos * mouse_sensitivity
    mouse_delta%y = y_pos * mouse_sensitivity
  end subroutine calculate_mouse_delta


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

    ! Rest the mouse delta.
    call delta_reset()
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

    ! Reset the mouse delta.
    call delta_reset()
  end subroutine mouse_unlock


  !* Get the delta of the mouse.
  type(vec2d) function mouse_get_delta() result(delta)
    implicit none

    delta = mouse_delta
  end function mouse_get_delta


  !* Update the mouse state.
  subroutine mouse_update()
    implicit none

    call delta_reset()
  end subroutine mouse_update


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


  subroutine delta_reset()
    implicit none

    mouse_delta%x = 0.0d0
    mouse_delta%y = 0.0d0
  end subroutine delta_reset


end module mouse
