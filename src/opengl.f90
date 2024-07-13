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
  public :: GL_COMPILE_STATUS
  public :: GL_LINK_STATUS
  public :: GL_VALIDATE_STATUS

  !

  integer, parameter :: GL_VERSION = int(z"1f02")
  integer, parameter :: GL_MAJOR_VERSION = int(z"821B")
  integer, parameter :: GL_MINOR_VERSION = int(z"821C")
  integer, parameter :: GL_TRUE = 1
  integer, parameter :: GL_FALSE = 0

  integer, parameter :: GL_COLOR_BUFFER_BIT = int(z"00004000")
  integer, parameter :: GL_DEBUG_OUTPUT_SYNCHRONOUS = int(z"8242")
  integer, parameter :: GL_VERTEX_SHADER = int(z"8B31")
  integer, parameter :: GL_FRAGMENT_SHADER = int(z"8B30")
  integer, parameter :: GL_COMPILE_STATUS = int(z"8B81")
  integer, parameter :: GL_LINK_STATUS = int(z"8B82")
  integer, parameter :: GL_VALIDATE_STATUS = int(z"8B83")

  integer, parameter :: GL_DEBUG_SEVERITY_NOTIFICATION = int(z"826B")
  integer, parameter :: GL_DEBUG_SEVERITY_LOW = int(z"9148")
  integer, parameter :: GL_DEBUG_SEVERITY_MEDIUM = int(z"9147")
  integer, parameter :: GL_DEBUG_SEVERITY_HIGH = int(z"9146")

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
  public :: gl_get_error
  public :: gl_clear_error_data
  public :: gl_get_shader_iv
  public :: gl_get_shader_info_log
  public :: gl_get_program_iv
  public :: gl_validate_program
  public :: gl_get_uniform_location
  public :: gl_get_attrib_location
  public :: gl_use_program

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
      character(len=*, kind=c_char), intent(in) :: source_code
      !? Less than 0 represents that the string is null terminated. So use that only.
      type(c_ptr), intent(in), optional :: string_length
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


    function gl_get_error() result(error_code) bind(c, name = "glGetError")
      use, intrinsic :: iso_c_binding
      implicit none
      integer(c_int) :: error_code
    end function gl_get_error


    subroutine internal_gl_get_shader_iv(shader, pname, params) bind(c, name = "glGetShaderiv")
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: shader
      integer(c_int), intent(in), value :: pname
      integer(c_int), intent(in) :: params
    end subroutine internal_gl_get_shader_iv


    subroutine internal_gl_get_shader_info_log(shader, max_length, length, info_log) bind(c, name = "glGetShaderInfoLog")
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: shader
      integer(c_int), intent(in), value :: max_length
      integer(c_int), intent(in) :: length
      type(c_ptr), intent(in) :: info_log
    end subroutine internal_gl_get_shader_info_log


    subroutine internal_gl_get_program_iv(program, pname, params) bind(c, name = "glGetProgramiv")
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: program
      integer(c_int), intent(in), value :: pname
      integer(c_int), intent(in) :: params
    end subroutine internal_gl_get_program_iv


    subroutine gl_validate_program(program_id) bind(c, name = "glValidateProgram")
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: program_id
    end subroutine gl_validate_program


    integer(c_int) function internal_gl_get_uniform_location(program_id, uniform_name) bind(c, name = "glGetUniformLocation")
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: program_id
      character(kind = c_char), intent(in), value :: uniform_name
    end function internal_gl_get_uniform_location


    integer(c_int) function gl_get_attrib_location(program_id, uniform_name) bind(c, name = "glGetAttribLocation")
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: program_id
      character(len = *, kind = c_char), intent(in) :: uniform_name
    end function gl_get_attrib_location


    subroutine gl_use_program(program_id) bind(c, name = "glUseProgram")
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: program_id
    end subroutine gl_use_program


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
    use terminal
    implicit none

    integer, intent(in), value :: source
    integer, intent(in), value :: type
    integer, intent(in), value :: id
    integer, intent(in), value :: severity
    integer, intent(in), value :: length
    type(c_ptr), intent(in), value :: message_pointer
    type(c_ptr), intent(in), value :: user_param_pointer
    character(:), allocatable :: fortran_message
    character(len = :),allocatable :: text_color
    character(len = :),allocatable :: severity_text

    ! Shut the compiler up.
    if (.false.) then
      print*,source,type,id,severity,user_param_pointer
    end if

    if (c_associated(message_pointer)) then
      fortran_message = string_from_c(message_pointer, length + 1)
      if (len(fortran_message) > 0) then

        select case (severity)
         case (GL_DEBUG_SEVERITY_NOTIFICATION)
          severity_text = "NOTIFICATION"
          text_color = to_rgb_string(137,207,240)
         case (GL_DEBUG_SEVERITY_LOW)
          severity_text = "LOW SEVERITY ERROR"
          text_color = to_rgb_string(255,255,0)
         case (GL_DEBUG_SEVERITY_MEDIUM)
          severity_text = "MEDIUM SEVERITY ERROR"
          text_color = to_rgb_string(255,165,0)
         case (GL_DEBUG_SEVERITY_HIGH)
          severity_text = "HIGH SEVERITY ERROR"
          text_color = to_rgb_string(255,0,0)
         case default
        end select

        !? Make this print nicely.
        print"(A)",colorize_rgb_string("[OpenGL] ("//severity_text//"): ("//int_to_string(source)//") "//fortran_message//".", text_color)
      end if
    end if
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
  end function gl_create_program


  function gl_create_shader(shader_type) result(shader_id)
    implicit none
    integer :: shader_type
    integer :: shader_id

    shader_id = internal_gl_create_shader(shader_type)
  end function gl_create_shader


  subroutine gl_shader_source(shader_id, source_code_location)
    use string
    use files
    implicit none

    integer :: shader_id
    character(len = *) :: source_code_location
    type(file_reader) :: reader

    call reader%read_file(source_code_location)

    ! Send the source code into the OpenGL state machine.
    call internal_gl_shader_source(shader_id, 1, reader%file_string//achar(0), null())

    !? OpenGL docs:
    !? OpenGL copies the shader source code strings when glShaderSource is called,
    !? so an application may free its copy of the source code strings immediately after the function returns.
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

    print"(A)","[OpenGL] Version: "//int_to_string(major)//"."//int_to_string(minor)
  end subroutine gl_get_version


  !? This is to be used before attempting to call gl_get_error as OpenGL will always start off at error 1280.
  subroutine gl_clear_error_data
    implicit none

    integer :: i = 0
    integer :: error

    do i = 1,3
      error = gl_get_error()
    end do
  end subroutine gl_clear_error_data


  subroutine gl_get_shader_info_log(shader)
    use,intrinsic :: iso_c_binding
    use string
    implicit none

    integer :: shader
    ! integer :: max_length
    integer :: length = -1
    ! character(:), allocatable :: info_log
    type(c_ptr) :: c_string

    call internal_gl_get_shader_info_log(shader, 512, length, c_string)
  end subroutine gl_get_shader_info_log


  function gl_get_shader_iv(shader, pname) result(code)
    implicit none

    integer :: shader
    integer :: pname
    integer :: code

    call internal_gl_get_shader_iv(shader, pname, code)
  end function gl_get_shader_iv


  function gl_get_program_iv(program_id, pname) result(code)
    use string
    implicit none

    integer :: program_id
    integer :: pname
    integer :: code

    call internal_gl_get_program_iv(program_id, pname, code)
  end function gl_get_program_iv

  integer function gl_get_uniform_location(program_id, uniform_name) result(location)
    use string
    implicit none

    integer, intent(in), value :: program_id
    character(len = *), intent(in) :: uniform_name

    location = internal_gl_get_uniform_location(program_id, into_c_string(uniform_name))
    print*,"loc:",location
  end function gl_get_uniform_location

end module opengl
