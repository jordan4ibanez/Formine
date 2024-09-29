module keyboard
  use :: glfw
  use :: hashmap_int
  use, intrinsic :: iso_c_binding
  implicit none


  private


  public :: keyboard_module_initialize
  public :: keyboard_key_down
  public :: keyboard_key_up


  !* Type: integer(c_int)
  type(hashmap_integer_key) :: key_database


contains

  !* This initializes the keyboard callback function.
  !* This must be called after GLFW is initialized.
  subroutine keyboard_module_initialize()
    implicit none

    key_database = new_hashmap_integer_key(sizeof(10))

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
    integer(c_int) :: state

    is_down = .false.

    if (.not. get_state(keyboard_key, state)) then
      return
    end if

    if (state == GLFW_PRESS .or. state == GLFW_REPEAT) then
      is_down = .true.
    end if
  end function keyboard_key_down


  !* Check if a keyboard key is up.
  logical function keyboard_key_up(keyboard_key) result(is_up)
    implicit none

    integer(c_int), intent(in), value :: keyboard_key
    integer(c_int) :: state

    is_up = .true.

    if (.not. get_state(keyboard_key, state)) then
      return
    end if

    if (state /= GLFW_RELEASE) then
      is_up = .false.
    end if
  end function keyboard_key_up


  function get_state(keyboard_key, state) result(exists)
    implicit none

    integer(c_int), intent(in), value :: keyboard_key
    integer(c_int), intent(inout) :: state
    logical(c_bool) :: exists
    type(c_ptr) :: raw_c_ptr
    integer(c_int), pointer :: state_pointer

    exists = .false.

    if (.not. key_database%get(int(keyboard_key, c_int64_t), raw_c_ptr)) then
      return
    end if

    call c_f_pointer(raw_c_ptr, state_pointer)
    state = state_pointer

    exists = .true.
  end function get_state


end module keyboard
