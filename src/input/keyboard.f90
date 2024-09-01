module keyboard
  use :: glfw
  use, intrinsic :: iso_c_binding
  implicit none


  private


  public :: keyboard_initialize


contains


  !* Key press events.
  subroutine keyboard_input_callback(window_pointer, key, scancode, action, mods)
    use :: mouse
    implicit none

    type(c_ptr), intent(in), value :: window_pointer
    integer(c_int), intent(in), value :: key, scancode, action, mods

    if (action == GLFW_PRESS) then

      select case (key)
       case (GLFW_KEY_ESCAPE)
        print*, "peace"
        call glfw_close_window()
       case (GLFW_KEY_F1)
        call mouse_debug_lock_toggle()

      end select
    end if
  end subroutine keyboard_input_callback


  !* This initializes the keyboard callback function.
  !* This must be called after GLFW is initialized.
  subroutine keyboard_initialize()
    implicit none

    call glfw_set_key_callback(c_funloc(keyboard_input_callback))
  end subroutine keyboard_initialize


end module keyboard
