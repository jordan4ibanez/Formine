module keyboard
  use :: glfw
  use :: hashmap_int
  use, intrinsic :: iso_c_binding
  implicit none


  private


  public :: keyboard_module_initialize
  public :: keyboard_key_down
  public :: keyboard_key_up


  !! FIXME: NEEDS A GC (maybe)
  type(hashmap_integer_key) :: key_database


contains

  !* This initializes the keyboard callback function.
  !* This must be called after GLFW is initialized.
  subroutine keyboard_module_initialize()
    implicit none

    !! FIXME: NEEDS A GC (maybe)
    key_database = new_hashmap_integer_key()

    call glfw_set_key_callback(c_funloc(keyboard_input_callback))
  end subroutine keyboard_module_initialize


  !* Key press events.
  subroutine keyboard_input_callback(window_pointer, keyboard_key, scancode, action, mods)
    use :: mouse
    implicit none

    type(c_ptr), intent(in), value :: window_pointer
    integer(c_int), intent(in), value :: keyboard_key, scancode, action, mods

    ! Suppress unused warnings.
    if (.false.) then
      print*,window_pointer, scancode, mods
    end if

    call process_key(keyboard_key, action)

    !! This is for debugging.
    if (action == GLFW_PRESS) then
      select case (keyboard_key)
       case (GLFW_KEY_ESCAPE)
        print*, "Thanks for testing!"
        call glfw_close_window()
       case (GLFW_KEY_F1)
        call mouse_debug_lock_toggle()
      end select
    end if
  end subroutine keyboard_input_callback


  !* Hold onto the memory of a key.
  subroutine process_key(keyboard_key, action)
    implicit none

    integer(c_int), intent(in), value :: keyboard_key, action

    call key_database%set(int(keyboard_key, c_int64_t), action)
  end subroutine process_key


  !* Check if a keyboard key is down.
  logical function keyboard_key_down(keyboard_key) result(is_down)
    implicit none

    integer(c_int), intent(in), value :: keyboard_key
    integer(c_int) :: state, status
    class(*), pointer :: generic_pointer

    is_down = .false.

    if (.not. key_database%get(int(keyboard_key, c_int64_t), generic_pointer)) then
      return
    end if

    select type (generic_pointer)
     type is (integer(c_int))
      state = generic_pointer
     class default
      error stop "[Keyboard] Error: Wrong type inserted into the database."
    end select

    if (state == GLFW_PRESS .or. state == GLFW_REPEAT) then
      is_down = .true.
    end if
  end function keyboard_key_down


  !* Check if a keyboard key is up.
  logical function keyboard_key_up(keyboard_key) result(is_up)
    implicit none

    integer(c_int), intent(in), value :: keyboard_key
    integer(c_int) :: state, status
    class(*), pointer :: generic_pointer

    is_up = .true.

    if (.not. key_database%get(int(keyboard_key, c_int64_t), generic_pointer)) then
      return
    end if

    select type (generic_pointer)
     type is (integer(c_int))
      state = generic_pointer
     class default
      error stop "[Keyboard] Error: Wrong type inserted into the database."
    end select

    if (state /= GLFW_RELEASE) then
      is_up = .false.
    end if
  end function keyboard_key_up


end module keyboard
