module factory
  implicit none
  private

  public :: mob
  public :: new_mob

  type mob
    integer :: id = 0
    integer :: hp = 0
  contains
    procedure :: yell
  end type mob

contains

  ! Construct a mobly mob. Count how many we have.
  function new_mob() result(a_mob)
    use, intrinsic :: iso_fortran_env
    use string
    implicit none

    type(mob) :: a_mob
    integer, save :: total_mobs = 0
    real :: randomness = 0.0

    ! Seed the random generator.
    if (total_mobs == 0) then
      call random_seed()
    end if

    call random_number(randomness)

    ! 0 - 100.0
    randomness = randomness * 100.0

    ! Mob has random hp
    a_mob%hp = floor(randomness)
    a_mob%id = total_mobs

    ! Tick up mob count.
    total_mobs = total_mobs + 1
  end

  subroutine yell(this)
    use string
    implicit none

    class(mob) :: this

    print"(A)","HELLO, I AM MOB ["//int_to_string(this%id)//"] WITH HP ["//int_to_string(this%hp)//"]"
  end subroutine yell

end module factory

program main
  use glfw
  use opengl
  use string
  use shader
  use files

  use factory
  use, intrinsic ::  iso_c_binding
  implicit none

  real :: color = 0.0
  ! we'll just reuse the mob on the stack.
  type(mob) :: mobly
  integer :: i

  do i = 1,100
    mobly = new_mob()
    call mobly%yell()
  end do

  !! BEGIN WARNING: This is only to be used for when developing libraries.
  if (.true.) then
    return
  end if
  !! END WARNING.

  call glfw_set_error_callback()

  ! Try to create a GLFW context.
  if (glfw_init()) then
    print"(A)","[GLFW]: Successfully initialized."
  else
    print"(A)","[GLFW] Error: Failed to initialize."
    return
  end if

  !! Need this flag to have OpenGL debugging available!
  call glfw_window_hint(GLFW_OPENGL_DEBUG_CONTEXT, GL_TRUE)
  call glfw_window_hint(GLFW_CONTEXT_VERSION_MAJOR, 4)
  call glfw_window_hint(GLFW_CONTEXT_VERSION_MINOR, 2)
  call glfw_window_hint(GLFW_OPENGL_FORWARD_COMPAT, GL_TRUE)

  ! Try to initialize the Window.
  if (glfw_create_window(640,480, "Fortran Game Engine")) then
    print"(A)","[GLFW]: Window created successfully."
  else
    print"(A)","[GLFW] Error: Failed to create window."
    call glfw_terminate()
    return
  end if

  call glfw_make_context_current()

  call gl_get_version()

  !! This allows OpenGL debugging.
  call gl_enable(GL_DEBUG_OUTPUT_SYNCHRONOUS)
  call gl_set_debug_message_callback()

  !! This resets the gl_get_error integer back to 0.
  call gl_clear_error_data()

  ! This can fail. We will gracefully exit when it does.
  if (.not. create_shader("main", "./shaders/vertex.vert", "./shaders/fragment.frag")) then
    return
  end if

  !! This is debugging for functions!
  if (.false.) then
    do while(.not. glfw_window_should_close())

      ! call blah(color)

      call gl_clear_color(0.0, color, color)

      call gl_clear_color_buffer()

      call glfw_swap_buffers()

      call glfw_poll_events()

    end do
  end if


  call glfw_destroy_window()

  call glfw_terminate()

end program main
