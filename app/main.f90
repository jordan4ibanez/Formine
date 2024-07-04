program main
  use glfw
  use, intrinsic ::  iso_c_binding
  implicit none


  if (glfw_init() .eqv. .true.) then
    print *,"worked"
  else
    print *,"failed"
    return
  end if






end
