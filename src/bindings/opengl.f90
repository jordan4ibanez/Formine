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
  public :: GL_FLOAT

  public :: GL_DEBUG_OUTPUT_SYNCHRONOUS
  public :: GL_COLOR_BUFFER_BIT
  public :: GL_VERTEX_SHADER
  public :: GL_FRAGMENT_SHADER
  public :: GL_COMPILE_STATUS
  public :: GL_LINK_STATUS
  public :: GL_VALIDATE_STATUS

  public :: GL_STATIC_DRAW
  public :: GL_ARRAY_BUFFER
  public :: GL_ELEMENT_ARRAY_BUFFER
  public :: GL_TRIANGLES
  public :: GL_UNSIGNED_INT
  public :: GL_TEXTURE_2D
  public :: GL_TEXTURE_WRAP_S
  public :: GL_TEXTURE_WRAP_T
  public :: GL_CLAMP_TO_BORDER
  public :: GL_TEXTURE_BORDER_COLOR
  public :: GL_NEAREST
  public :: GL_NEAREST_MIPMAP_LINEAR
  public :: GL_NEAREST_MIPMAP_NEAREST
  public :: GL_TEXTURE_MIN_FILTER
  public :: GL_TEXTURE_MAG_FILTER
  public :: GL_UNPACK_ALIGNMENT
  public :: GL_RGBA
  public :: GL_UNSIGNED_BYTE

  !

  integer, parameter :: GL_VERSION = int(z"1f02")
  integer, parameter :: GL_MAJOR_VERSION = int(z"821B")
  integer, parameter :: GL_MINOR_VERSION = int(z"821C")
  integer, parameter :: GL_TRUE = 1
  integer, parameter :: GL_FALSE = 0
  integer, parameter :: GL_FLOAT = int(z"1406")

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

  integer, parameter :: GL_STATIC_DRAW = int(z"88E4")
  integer, parameter :: GL_ARRAY_BUFFER = int(z"8892")
  integer, parameter :: GL_ELEMENT_ARRAY_BUFFER = int(z"8893")
  integer, parameter :: GL_TRIANGLES = int(z"0004")
  integer, parameter :: GL_UNSIGNED_INT = int(z"1405")
  integer, parameter :: GL_TEXTURE_2D = int(z"0DE1")
  integer, parameter :: GL_TEXTURE_WRAP_S = int(z"2802")
  integer, parameter :: GL_TEXTURE_WRAP_T = int(z"2803")
  integer, parameter :: GL_CLAMP_TO_BORDER = int(z"812D")
  integer, parameter :: GL_TEXTURE_BORDER_COLOR = int(z"1004")
  integer, parameter :: GL_NEAREST = int(z"2600")
  integer, parameter :: GL_NEAREST_MIPMAP_LINEAR = int(z"2702")
  integer, parameter :: GL_NEAREST_MIPMAP_NEAREST = int(z"2700")
  integer, parameter :: GL_TEXTURE_MIN_FILTER = int(z"2801")
  integer, parameter :: GL_TEXTURE_MAG_FILTER = int(z"2800")
  integer, parameter :: GL_UNPACK_ALIGNMENT = int(z"0CF5")
  integer, parameter :: GL_RGBA = int(z"1908")
  integer, parameter :: GL_UNSIGNED_BYTE = int(z"1401")


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
  public :: gl_gen_vertex_arrays
  public :: gl_delete_vertex_arrays
  public :: gl_bind_vertex_array
  public :: gl_gen_buffers
  public :: gl_delete_buffers
  public :: gl_bind_buffer
  public :: gl_buffer_float_array
  public :: gl_buffer_vec3f_array
  public :: gl_buffer_indices_array
  public :: gl_enable_vertex_attrib_array
  public :: gl_disable_vertex_attrib_array
  public :: gl_vertex_attrib_pointer
  public :: gl_draw_elements
  public :: gl_uniform_mat4f
  public :: gl_view_port
  public :: gl_is_buffer
  public :: gl_is_vertex_array
  public :: gl_gen_textures
  public :: gl_bind_texture
  public :: gl_tex_parameter_i
  public :: gl_tex_parameter_fv
  public :: gl_tex_image_2d
  public :: gl_is_texture
  public :: gl_generate_mipmap


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
      integer(c_int), intent(in), value :: cap
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


    integer(c_int) function internal_gl_get_attrib_location(program_id, attrib_name) bind(c, name = "glGetAttribLocation")
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: program_id
      character(kind = c_char), intent(in), value :: attrib_name
    end function internal_gl_get_attrib_location


    subroutine gl_use_program(program_id) bind(c, name = "glUseProgram")
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: program_id
    end subroutine gl_use_program


    subroutine internal_gl_gen_vertex_arrays(n, arrays) bind(c, name = "glGenVertexArrays")
      use,intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: n
      !! This part is written wrong on purpose. I only want 1 not multiple.
      integer(c_int), intent(inout) :: arrays
    end subroutine internal_gl_gen_vertex_arrays


    subroutine internal_gl_delete_vertex_arrays(n, arrays) bind(c, name = "glDeleteVertexArrays")
      use,intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: n
      !! This part is written wrong on purpose. I only want 1 not multiple.
      integer(c_int), intent(in) :: arrays
    end subroutine internal_gl_delete_vertex_arrays


    subroutine gl_bind_vertex_array(array) bind(c, name = "glBindVertexArray")
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: array
    end subroutine gl_bind_vertex_array


    subroutine internal_gl_gen_buffers(n, buffers) bind(c, name = "glGenBuffers")
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: n
      !! This part is written wrong on purpose. I only want 1 not multiple.
      integer(c_int), intent(inout) :: buffers
    end subroutine internal_gl_gen_buffers


    subroutine internal_gl_delete_buffers(n, buffers) bind(c, name = "glDeleteBuffers")
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: n
      !! This part is written wrong on purpose. I only want 1 not multiple.
      integer(c_int), intent(in) :: buffers
    end subroutine internal_gl_delete_buffers


    subroutine gl_bind_buffer(target, buffer) bind(c, name = "glBindBuffer")
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: target
      integer(c_int), intent(in), value :: buffer
    end subroutine gl_bind_buffer


    subroutine internal_gl_buffer_data(target, size, data, usage) bind(c, name ="glBufferData")
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: target
      integer(c_int), intent(in), value :: size
      type(c_ptr), intent(in), value :: data
      integer(c_int), intent(in), value :: usage
    end subroutine internal_gl_buffer_data


    subroutine gl_enable_vertex_attrib_array(index) bind(c, name = "glEnableVertexAttribArray")
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: index
    end subroutine gl_enable_vertex_attrib_array


    subroutine gl_disable_vertex_attrib_array(index) bind(c, name = "glDisableVertexAttribArray")
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: index
    end subroutine gl_disable_vertex_attrib_array


    subroutine internal_gl_vertex_attrib_pointer(index, size, type, normalized, stride, pointer) bind(c, name = "glVertexAttribPointer")
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: index
      integer(c_int), intent(in), value :: size
      integer(c_int), intent(in), value :: type
      logical(c_bool), intent(in), value :: normalized
      integer(c_int), intent(in), value :: stride
      type(c_ptr), intent(in), optional :: pointer
    end subroutine internal_gl_vertex_attrib_pointer


    subroutine internal_gl_draw_elements(mode, count, type, indices) bind(c, name = "glDrawElements")
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: mode
      integer(c_int), intent(in), value :: count
      integer(c_int), intent(in), value :: type
      type(c_ptr), intent(in), optional :: indices
    end subroutine internal_gl_draw_elements


    subroutine internal_gl_uniform_matrix_4_fv(location, count, transpose, value) bind(c, name = "glUniformMatrix4fv")
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: location
      integer(c_int), intent(in), value :: count
      logical(c_bool), intent(in), value :: transpose
      type(c_ptr), intent(in), value :: value
    end subroutine internal_gl_uniform_matrix_4_fv


    subroutine gl_view_port(x, y, width, height) bind(c, name = "glViewport")
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: x
      integer(c_int), intent(in), value :: y
      integer(c_int), intent(in), value :: width
      integer(c_int), intent(in), value :: height
    end subroutine gl_view_port


    function gl_is_buffer(buffer) result(is_buffer) bind(c, name = "glIsBuffer")
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: buffer
      logical(c_bool) :: is_buffer
    end function gl_is_buffer


    function gl_is_vertex_array(array) result(is_array) bind(c, name = "glIsVertexArray")
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: array
      logical(c_bool) :: is_array
    end function gl_is_vertex_array


    subroutine internal_gl_gen_textures(n, textures) bind(c, name = "glGenTextures")
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: n
      !! This part is written wrong on purpose. I only want 1 not multiple.
      integer(c_int), intent(inout) :: textures
    end subroutine internal_gl_gen_textures


    subroutine gl_bind_texture(target, texture) bind(c, name = "glBindTexture")
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: target, texture
    end subroutine gl_bind_texture


    subroutine gl_tex_parameter_i(target, pname, param) bind(c, name = "glTexParameteri")
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: target, pname, param
    end subroutine gl_tex_parameter_i


    subroutine gl_tex_parameter_fv(target, pname, params) bind(c, name = "glTexParameterfv")
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: target, pname
      integer(c_int), dimension(:) :: params
    end subroutine gl_tex_parameter_fv


    subroutine gl_pixel_store_i(pname, param) bind(c, name = "glPixelStorei")
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: pname, param
    end subroutine gl_pixel_store_i


    subroutine internal_gl_tex_image_2d(target, level, internal_format, width, height, border, format, type, data) bind(c, name = "glTexImage2D")
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: target, level, internal_format, width, height, border, format, type
      type(c_ptr), intent(in), optional :: data
    end subroutine internal_gl_tex_image_2d


    function gl_is_texture(texture) result(is_texture) bind(c, name = "glIsTexture")
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: texture
      logical(c_bool) :: is_texture
    end function gl_is_texture


    subroutine gl_generate_mipmap(target) bind(c, name = "glGenerateMipmap")
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: target
    end subroutine gl_generate_mipmap


    subroutine internal_gl_delete_textures(n, textures) bind(c, name = "glDeleteTextures")
      use, intrinsic :: iso_c_binding
      implicit none

      !! This part is written wrong on purpose. I only want 1 not multiple.
      integer(c_int), intent(in), value :: n, textures
    end subroutine internal_gl_delete_textures


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
    use :: string
    use :: terminal
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
    use :: string
    use :: files
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
    use :: string
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
    use :: string
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
    use :: string
    implicit none

    integer :: program_id
    integer :: pname
    integer :: code

    call internal_gl_get_program_iv(program_id, pname, code)
  end function gl_get_program_iv


  integer function gl_get_uniform_location(program_id, uniform_name) result(location)
    use :: string
    implicit none

    integer, intent(in), value :: program_id
    character(len = *), intent(in) :: uniform_name

    location = internal_gl_get_uniform_location(program_id, into_c_string(uniform_name))
    ! print*,"uniform: ", uniform_name," | loc: ", location
  end function gl_get_uniform_location


  integer function gl_get_attrib_location(program_id, uniform_name) result(location)
    use :: string
    implicit none

    integer, intent(in), value :: program_id
    character(len = *), intent(in) :: uniform_name

    location = internal_gl_get_attrib_location(program_id, into_c_string(uniform_name))
    ! print*,"attrib: ", uniform_name," | loc: ", location
  end function gl_get_attrib_location


  !** Special note: I only use 1 at a time. So we're only going to use one at a time.
  !** This is written "wrong" on purpose.
  integer function gl_gen_vertex_arrays() result(location)
    implicit none

    call internal_gl_gen_vertex_arrays(1, location)
  end function gl_gen_vertex_arrays

  !** Special note: I only use 1 at a time. So we're only going to use one at a time.
  !** This is written "wrong" on purpose.
  subroutine gl_delete_vertex_arrays(location)
    implicit none

    integer(c_int), intent(in), value :: location

    call internal_gl_delete_vertex_arrays(1, location)
  end subroutine gl_delete_vertex_arrays


  !** Special note: I only use 1 at a time. So we're only going to use one at a time.
  !** This is written "wrong" on purpose.
  integer function gl_gen_buffers() result(location)
    implicit none

    call internal_gl_gen_buffers(1, location)
  end function gl_gen_buffers


  !** Special note: I only use 1 at a time. So we're only going to use one at a time.
  !** This is written "wrong" on purpose.
  subroutine gl_delete_buffers(location)
    implicit none

    integer(c_int), intent(in), value :: location

    call internal_gl_delete_buffers(1, location)
  end subroutine gl_delete_buffers


  !** This is a custom command to allow gl_buffer_data to use specific types.
  subroutine gl_buffer_float_array(float_array)
    use :: constants
    use, intrinsic :: iso_c_binding
    implicit none

    real(kind = c_float), dimension(:), target :: float_array
    integer(c_int) :: total_size
    integer(c_int) :: length_of_array

    length_of_array = size(float_array)
    total_size = F32_SIZE * length_of_array

    !! FIXME: Might be wrong.
    call internal_gl_buffer_data(GL_ARRAY_BUFFER, total_size, c_loc(float_array), GL_STATIC_DRAW)
  end subroutine gl_buffer_float_array


  !** This is a custom command to allow gl_buffer_data to use specific types.
  subroutine gl_buffer_vec3f_array(vec3f_array)
    use, intrinsic :: iso_c_binding
    use :: constants
    use :: vector_3f
    implicit none

    type(vec3f), dimension(:), target :: vec3f_array
    integer(c_int) :: total_size
    integer(c_int) :: length_of_array

    length_of_array = size(vec3f_array)
    total_size = F32_SIZE * length_of_array * 3

    !! FIXME: Might be wrong.
    call internal_gl_buffer_data(GL_ARRAY_BUFFER, total_size, c_loc(vec3f_array), GL_STATIC_DRAW)
  end subroutine gl_buffer_vec3f_array


  !** This is a custom command to allow gl_buffer_data to use specific types.
  subroutine gl_buffer_indices_array(indices_array)
    use :: constants
    use, intrinsic :: iso_c_binding
    implicit none

    integer(kind = c_int), dimension(:), target :: indices_array
    integer(c_int) :: total_size
    integer(c_int) :: length_of_array

    length_of_array = size(indices_array)
    total_size = I32_SIZE * length_of_array

    !! FIXME: Might be wrong.
    call internal_gl_buffer_data(GL_ELEMENT_ARRAY_BUFFER, total_size, c_loc(indices_array), GL_STATIC_DRAW)
  end subroutine gl_buffer_indices_array


  subroutine gl_vertex_attrib_pointer(index, size, type, normalized, stride)
    use, intrinsic :: iso_c_binding
    implicit none

    integer, intent(in), value :: index
    integer, intent(in), value :: size
    integer, intent(in), value :: type
    logical, intent(in), value :: normalized
    integer, intent(in), value :: stride
    logical(c_bool) :: final_normalized

    ! Convert
    final_normalized = normalized

    call internal_gl_vertex_attrib_pointer(index, size, type, final_normalized, stride, null())
  end subroutine gl_vertex_attrib_pointer


  subroutine gl_draw_elements(mode, count, type)
    implicit none

    integer, intent(in), value :: mode
    integer, intent(in), value :: count
    integer, intent(in), value :: type

    call internal_gl_draw_elements(mode, count, type, null())
  end subroutine gl_draw_elements


  subroutine gl_uniform_mat4f(location, value)
    use :: matrix_4f
    implicit none

    integer, intent(in), value :: location
    type(mat4f), intent(in), target :: value
    logical(c_bool) :: transpose

    transpose = .false.

    call internal_gl_uniform_matrix_4_fv(location, 1, transpose, c_loc(value))
  end subroutine gl_uniform_mat4f


  !** Special note: I only use 1 at a time. So we're only going to use one at a time.
  !** This is written "wrong" on purpose.
  integer function gl_gen_textures() result(location)
    implicit none

    call internal_gl_gen_textures(1, location)
  end function gl_gen_textures


  !** This is so you can buffer texture data without jumping through hoops.
  subroutine gl_tex_image_2d(target, level, internal_format, width, height, border, format, type, data)
    use, intrinsic :: iso_c_binding
    implicit none

    integer(c_int), intent(in), value :: target, level, internal_format, width, height, border, format, type
    integer(1), dimension(:), intent(in), target :: data

    call internal_gl_tex_image_2d(target, level, internal_format, width, height, border, format, type, c_loc(data))
  end subroutine gl_tex_image_2d


  !** Special note: I only use 1 at a time. So we're only going to use one at a time.
  !** This is written "wrong" on purpose.
  subroutine gl_delete_textures(location)
    use, intrinsic :: iso_c_binding
    implicit none

    integer(c_int), intent(in), value :: location

    call internal_gl_delete_textures(location, 1)
  end subroutine gl_delete_textures

end module opengl
