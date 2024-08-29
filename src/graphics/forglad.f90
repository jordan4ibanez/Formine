module forglad
  use :: glfw
  use, intrinsic :: iso_c_binding
  implicit none

  procedure(gl_clear_c), pointer :: gl_clear

  interface

    subroutine gl_clear_c(mask) bind(c)
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: mask
    end subroutine gl_clear_c

  end interface

contains

  subroutine forglad_init()
    use :: string
    implicit none

    type(c_funptr) :: function_pointer
    character(len = :, kind = c_char), allocatable :: function_name

    function_name = into_c_string("glClear")
    function_pointer = glfw_get_proc_address(function_name)
    call c_f_procpointer(function_pointer, gl_clear)

  end subroutine forglad_init

end module forglad
