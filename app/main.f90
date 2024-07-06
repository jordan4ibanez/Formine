module ye
  implicit none

  private

  logical :: up = .true.

  public :: blah

contains
  subroutine blah(input)
    implicit none
    real :: input

    if (up) then
      input = input + 0.01
      if (input >= 1.0) then
        input = 1.0
        up = .false.
      end if
    else
      input = input - 0.01
      if (input <= 0.0) then
        input = 0.0
        up = .true.
      end if
    end if
  end subroutine
end module ye

program main
  use glfw
  use opengl
  use ye
  use, intrinsic ::  iso_c_binding
  implicit none

  real :: color = 0.0


  ! Try to create a GLFW context.
  ! if (glfw_init()) then
  !   print *,"worked"
  ! else
  !   print *,"failed"
  !   return
  ! end if

  ! call glfw_get_error()

  ! Try to initialize the Window.
  ! if (glfw_create_window(640,480, "hi")) then
  !   print *,"Created window successfully."
  ! else
  !   print *,"Failed to create window."
  !   ! call glfw_terminate()
  !   return
  ! end if

  ! call glfw_get_error()

  call glfw_make_context_current()

  call glfw_get_error()

  ! do while(.not. glfw_window_should_close())

  !   call blah(color)

  !   ! print*,color

  !   call gl_clear_color(color, color, color)

  !   ! call glfw_get_error()

  !   call gl_clear_color_buffer()



  !   call glfw_swap_buffers()

  !   ! call glfw_get_error()

  !   call glfw_poll_events()

  !   ! call glfw_get_error()


  ! end do



  ! call glfw_destroy_window()

  ! call glfw_get_error()

  ! call glfw_terminate()



end
