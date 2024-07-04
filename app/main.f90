program main
  use glfw
  use, intrinsic ::  iso_c_binding
  implicit none

  ! Try to create a GLFW context.
  if (glfw_init() .eqv. .true.) then
    print *,"worked"
  else
    print *,"failed"
    return
  end if

  ! Try to initialize the Window.
  if (glfw_create_window(400,400, "hi") .eqv. .true.) then
    print *,"Created window successfully."
  else
    print *,"Failed to create window."
    call glfw_terminate()
    return
  end if






end
