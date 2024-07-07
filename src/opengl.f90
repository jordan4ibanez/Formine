module opengl
  use, intrinsic :: iso_c_binding
  implicit none

  private

  ! OpenGL constants.

  public :: GL_VERSION
  public :: GL_MAJOR_VERSION
  public :: GL_MINOR_VERSION
  public :: GL_TRUE
  public :: GL_FALSE

  public :: GL_DEBUG_OUTPUT_SYNCHRONOUS
  public :: GL_COLOR_BUFFER_BIT
  public :: GL_VERTEX_SHADER
  public :: GL_FRAGMENT_SHADER

  !

  integer :: GL_VERSION = int(z"1f02")
  integer :: GL_MAJOR_VERSION = int(z"821B")
  integer :: GL_MINOR_VERSION = int(z"821C")
  integer :: GL_TRUE = 1
  integer :: GL_FALSE = 0

  integer :: GL_COLOR_BUFFER_BIT = int(z"00004000")
  integer :: GL_DEBUG_OUTPUT_SYNCHRONOUS = int(z"8242")
  integer :: GL_VERTEX_SHADER = int(z"8B31")
  integer :: GL_FRAGMENT_SHADER = int(z"8B30")

  ! Functions we want exposed.

  public :: gl_clear_color_buffer
  public :: gl_enable
  public :: gl_clear_color
  public :: gl_set_debug_message_callback
  public :: gl_create_program
  public :: gl_create_shader
  public :: gl_shader_source
  public :: gl_compile_shader
  public :: gl_get_integer_v
  public :: gl_get_version
  public :: gl_attach_shader
  public :: gl_link_program

  ! Here I'm binding to the C shared library.

  interface

    subroutine internal_gl_clear(thing_to_clear) bind(c, name = "glClear")
      use, intrinsic :: iso_c_binding
      implicit none
      integer(c_int), intent(in), value :: thing_to_clear
    end subroutine internal_gl_clear

    subroutine internal_gl_clear_color(r,g,b,a) bind(c, name = "glClearColor")
      use, intrinsic :: iso_c_binding
      implicit none
      real(c_float), intent(in), value :: r
      real(c_float), intent(in), value :: g
      real(c_float), intent(in), value :: b
      real(c_float), intent(in), value :: a
    end subroutine internal_gl_clear_color

    subroutine gl_enable(cap) bind(c, name = "glEnable")
      use, intrinsic :: iso_c_binding
      implicit none
      integer(c_int) :: cap
    end subroutine gl_enable

    subroutine internal_gl_debug_message_callback(callback, user_param) bind(c, name = "glDebugMessageCallback")
      use, intrinsic :: iso_c_binding
      implicit none
      type(c_funptr), intent(in), value :: callback
      type(c_ptr), intent(in), optional :: user_param
    end subroutine internal_gl_debug_message_callback

    function internal_gl_create_program() result(program_id) bind(c, name = "glCreateProgram")
      use, intrinsic :: iso_c_binding
      implicit none
      !! This might cause problems, it's a uint.
      integer(c_int) :: program_id
    end function internal_gl_create_program

    function internal_gl_create_shader(shader_type) result(shader_id) bind(c, name = "glCreateShader")
      use, intrinsic :: iso_c_binding
      implicit none
      !! This might cause problems, it's a uint.
      integer(c_int), intent(in), value :: shader_type
      integer(c_int) :: shader_id
    end function internal_gl_create_shader

    subroutine internal_gl_shader_source(shader_id, count, source_code, string_length) bind(c, name = "glShaderSource")
      use, intrinsic :: iso_c_binding
      implicit none
      integer(c_int), intent(in), value :: shader_id
      integer(c_int), intent(in), value :: count
      character(kind = c_char), intent(in), value :: source_code
      !? Less than 0 represents that the string is null terminated. So use that only.
      integer(c_int), intent(in), value :: string_length
    end subroutine internal_gl_shader_source

    subroutine gl_compile_shader(shader_id) bind(c, name = "glCompileShader")
      use, intrinsic :: iso_c_binding
      implicit none
      integer(c_int), intent(in), value :: shader_id
    end subroutine gl_compile_shader

    subroutine gl_get_integer_v(pname, data) bind(c, name = "glGetIntegerv")
      use, intrinsic :: iso_c_binding
      implicit none
      integer(c_int), intent(in), value :: pname
      integer(c_int), intent(in), target :: data
    end subroutine gl_get_integer_v

    subroutine gl_attach_shader(program, shader) bind(c, name = "glAttachShader")
      use, intrinsic :: iso_c_binding
      implicit none
      integer(c_int), intent(in), value :: program
      integer(c_int), intent(in), value :: shader
    end subroutine gl_attach_shader

    subroutine gl_link_program(program) bind(c, name = "glLinkProgram")
      use, intrinsic :: iso_c_binding
      implicit none
      integer(c_int), intent(in), value :: program
    end subroutine gl_link_program

  end interface

contains

  ! Here I'm just kind of using OpenGL the way I want to use it.

  subroutine gl_clear_color_buffer
    implicit none
    call internal_gl_clear(GL_COLOR_BUFFER_BIT)
  end

  subroutine gl_clear_color(r,g,b)
    implicit none
    real(c_float) :: r
    real(c_float) :: g
    real(c_float) :: b
    call internal_gl_clear_color(r,g,b,1.0)
  end subroutine gl_clear_color


  !** NOTE: C is passing Fortran data here!
  !** NOTE: This function passed into C as a pointer!
  subroutine debug_message_callback(source, type, id, severity, length, message_pointer, user_param_pointer)
    use, intrinsic :: iso_c_binding
    use string
    use deal
    implicit none

    integer, intent(in), value :: source
    integer, intent(in), value :: type
    integer, intent(in), value :: id
    integer, intent(in), value :: severity
    integer, intent(in), value :: length
    type(c_ptr), intent(in), value :: message_pointer
    type(c_ptr), intent(in), value :: user_param_pointer
    character(:), allocatable :: fortran_message

    ! Shut the compiler up.
    if (.false.) then
      print*,source,type,id,severity,user_param_pointer
    end if

    if (c_associated(message_pointer)) then
      fortran_message = string_from_c(message_pointer, length + 1)
      if (len(fortran_message) > 0) then
        !? Make this print nicely.
        print*,"[OpenGL] Error: ("//int_to_string(source)//") "//fortran_message//"."
      end if
    end if

    call deallocate_string(fortran_message)

  end subroutine debug_message_callback
  subroutine gl_set_debug_message_callback
    use, intrinsic :: iso_c_binding
    implicit none
    call internal_gl_debug_message_callback(c_funloc(debug_message_callback), null())
  end subroutine gl_set_debug_message_callback

  function gl_create_program() result(program_id)
    implicit none
    integer :: program_id

    program_id = internal_gl_create_program()

    !? We literally must crash out if OpenGL fails to make a shader program.
    !? We need a shader program to draw things.
    if (program_id == 0) then
      error stop "[OpenGL] Error: Failed to create a shader program."
    end if
  end function gl_create_program

  function gl_create_shader(shader_type) result(shader_id)
    implicit none
    integer :: shader_type
    integer :: shader_id

    shader_id = internal_gl_create_shader(shader_type)

    !? We literally must crash out if OpenGL fails to make a shader.
    !? We need a shader to draw things.
    if (shader_id == 0) then
      error stop "[OpenGL] Error: Failed to create a shader."
    end if
  end function gl_create_shader

  subroutine gl_shader_source(shader_id, source_code_location)
    use string
    use deal
    use files
    implicit none

    integer :: shader_id
    character(len = *) :: source_code_location
    type(file_reader) :: reader
    character(len = :, kind = c_char), allocatable :: c_source_code

    call reader%read_file(source_code_location)

    if (.not. reader%exists) then
      error stop "[OpenGL] Error: Source code location for ["//source_code_location//"] does not exist."
    end if

    ! Transfer the source code string into a null terminated string.
    c_source_code = into_c_string(reader%file_string)

    ! Now we can deallocate the reader.
    call reader%deallocate()

    ! Send the source code into the OpenGL state machine.
    call internal_gl_shader_source(shader_id, 0, c_source_code, -1)

    !? OpenGL docs:
    !? OpenGL copies the shader source code strings when glShaderSource is called,
    !? so an application may free its copy of the source code strings immediately after the function returns.

    call deallocate_string(c_source_code)
  end subroutine gl_shader_source


  subroutine gl_get_version
    use, intrinsic :: iso_c_binding
    use string
    implicit none
    integer(c_int) :: major
    integer(c_int) :: minor

    ! We're passing a pointer right into C to mutate it.
    call gl_get_integer_v(GL_MAJOR_VERSION, major)
    call gl_get_integer_v(GL_MINOR_VERSION, minor)

    print*,"[OpenGL] Version: "//int_to_string(major)//"."//int_to_string(minor)

  end subroutine gl_get_version


end module opengl
