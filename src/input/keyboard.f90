module keyboard
  use :: glfw
  use, intrinsic :: iso_c_binding
  implicit none


  private


  public :: keyboard_initialize


contains


  subroutine keyboard_input_callback()
    implicit none


  end subroutine keyboard_input_callback


  !* This initializes the keyboard callback function.
  !* This must be called after GLFW is initialized.
  subroutine keyboard_initialize()
    implicit none

    call glfw_set_key_callback(c_funloc(keyboard_input_callback))
  end subroutine keyboard_initialize


end module keyboard
