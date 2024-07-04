program main
  use glfw
  use, intrinsic ::  iso_c_binding
  implicit none

  ! trying to learn how to pass strings to C



  type(c_ptr) :: c_string
  character(c_char), pointer :: local_string(:)



  ! allocate(fstring)
  ! character(kind=c_char), allocatable :: temp
  ! type(c_ptr) :: cstring

  ! allocate(character(len=512) :: cstring)

  ! temp(512:512) = c_null_char

  ! cstring = c_loc(temp)

  ! allocate(test(512))


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

  call glfw_make_context_current()




  do while (glfw_window_should_close() .eqv. .false.)


    ! call test_things()

    do
      print*,"loop"
      allocate(local_string(512*512))
      c_string = c_loc(local_string)
      print*,c_string

      print*,"memory:", glfw_get_error(c_string)

      deallocate(local_string)

      print*,"work"
    end do

    call clear_color_buffer()

    call glfw_swap_buffers()

    call glfw_poll_events()

  end do

  call glfw_destroy_window()


  call glfw_terminate()

end
