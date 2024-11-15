module opengl
  use :: forglad
  use, intrinsic :: iso_c_binding
  implicit none


  private


  ! OpenGL function pointer loader.

  public :: forglad_load_gl
  public :: forglad_gpu_supports_gl_debugging

  ! OpenGL function pointers.

  public :: gl_get_string
  public :: gl_clear_color_buffer
  public :: gl_clear_depth_buffer
  public :: gl_clear_color_and_depth_buffer
  public :: gl_enable
  public :: gl_disable
  public :: gl_clear_color
  public :: gl_clear_color_scalar
  public :: gl_set_debug_message_callback
  public :: gl_create_program
  public :: gl_delete_program
  public :: gl_is_program
  public :: gl_create_shader
  public :: gl_delete_shader
  public :: gl_shader_source
  public :: gl_compile_shader
  public :: gl_is_shader
  public :: gl_get_integer_v
  public :: gl_get_version
  public :: gl_attach_shader
  public :: gl_detach_shader
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
  public :: gl_pixel_store_i
  public :: gl_tex_image_2d
  public :: gl_is_texture
  public :: gl_generate_mipmap
  public :: gl_delete_textures
  public :: gl_depth_mask
  public :: gl_depth_func
  public :: gl_depth_range
  public :: gl_blend_equation
  public :: gl_blend_func
  public :: gl_blend_func_separate


  ! OpenGL Constants.

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
  public :: GL_DEPTH_TEST
  public :: GL_LESS
  public :: GL_CULL_FACE
  public :: GL_DEPTH_BUFFER_BIT
  public :: GL_BLEND
  public :: GL_FUNC_ADD
  public :: GL_SRC_ALPHA
  public :: GL_ONE_MINUS_SRC_ALPHA
  public :: GL_ONE


contains


  ! Here I'm just kind of using OpenGL the way I want to use it.


  subroutine gl_clear_color_buffer()
    implicit none

    call internal_gl_clear(GL_COLOR_BUFFER_BIT)
  end


  subroutine gl_clear_depth_buffer()
    implicit none

    call internal_gl_clear(GL_DEPTH_BUFFER_BIT)
  end subroutine gl_clear_depth_buffer


  subroutine gl_clear_color_and_depth_buffer()
    implicit none

    call internal_gl_clear(ior(GL_COLOR_BUFFER_BIT, GL_DEPTH_BUFFER_BIT))
  end subroutine gl_clear_color_and_depth_buffer


  subroutine gl_clear_color(r,g,b)
    implicit none

    real(c_float), intent(in), value :: r, g, b

    call internal_gl_clear_color(r, g, b, 1.0)
  end subroutine gl_clear_color


  subroutine gl_clear_color_scalar(i)
    implicit none

    real(c_float), intent(in), value :: i

    call internal_gl_clear_color(i, i, i, 1.0)
  end subroutine gl_clear_color_scalar


  !* NOTE: C is passing Fortran data here!
  !* NOTE: This function passed into C as a pointer!
  subroutine debug_message_callback(source, type, id, severity, message_pointer, user_param_pointer)
    use, intrinsic :: iso_c_binding
    use :: string_f90
    use :: terminal
    implicit none

    integer, intent(in), value :: source, type, id, severity
    type(c_ptr), intent(in), value :: message_pointer, user_param_pointer
    character(len = :, kind = c_char), pointer :: fortran_message
    character(len = :, kind = c_char), allocatable :: severity_text
    integer(c_int) :: severity_level

    ! Shut the compiler up.
    if (.false.) then
      print*,source,type,id,severity,user_param_pointer
    end if

    if (c_associated(message_pointer)) then
      fortran_message => string_from_c(message_pointer)
      if (len(fortran_message) > 0) then

        select case (severity)
         case (GL_DEBUG_SEVERITY_NOTIFICATION)
          severity_text = "NOTIFICATION"
          severity_level = NOTIFICATION
         case (GL_DEBUG_SEVERITY_LOW)
          severity_text = "LOW SEVERITY ERROR"
          severity_level = NOTIFICATION
         case (GL_DEBUG_SEVERITY_MEDIUM)
          severity_text = "MEDIUM SEVERITY ERROR"
          severity_level = WARNING
         case (GL_DEBUG_SEVERITY_HIGH)
          severity_text = "HIGH SEVERITY ERROR"
          severity_level = ERROR
         case default
        end select

        !? Make this print nicely.
        call print_color(severity_level, "[OpenGL] ("//severity_text//"): ("//int_to_string(source)//") "//fortran_message//".")
      end if
    end if
  end subroutine debug_message_callback


  subroutine gl_set_debug_message_callback()
    use, intrinsic :: iso_c_binding
    implicit none

    call internal_gl_debug_message_callback(c_funloc(debug_message_callback), null())
  end subroutine gl_set_debug_message_callback


  function gl_create_program() result(program_id)
    implicit none

    integer(c_int) :: program_id

    program_id = internal_gl_create_program()
  end function gl_create_program


  function gl_create_shader(shader_type) result(shader_id)
    implicit none

    integer(c_int), intent(in), value :: shader_type
    integer(c_int) :: shader_id

    shader_id = internal_gl_create_shader(shader_type)
  end function gl_create_shader


  subroutine gl_shader_source(shader_id, source_code_file_path)
    use :: string_f90
    use :: files_f90
    implicit none

    integer(c_int) :: shader_id
    character(len = *, kind = c_char) :: source_code_file_path
    type(file_reader) :: reader

    ! If this source code file doesn't exist, give up.
    if (.not. reader%read_file(source_code_file_path)) then
      error stop "[OpenGL] Error: Shader source code file path ["//source_code_file_path//"] does not exist."
    end if

    ! Send the source code into the OpenGL state machine.
    call internal_gl_shader_source(shader_id, 1, reader%file_string//achar(0), null())

    !? OpenGL docs:
    !? OpenGL copies the shader source code strings when glShaderSource is called,
    !? so an application may free its copy of the source code strings immediately after the function returns.
  end subroutine gl_shader_source


  subroutine gl_get_version()
    use, intrinsic :: iso_c_binding
    use :: string_f90
    implicit none

    integer(c_int) :: major, minor

    ! We're passing a pointer right into C to mutate it.
    call gl_get_integer_v(GL_MAJOR_VERSION, major)
    call gl_get_integer_v(GL_MINOR_VERSION, minor)

    print"(A)","[OpenGL] Version: "//int_to_string(major)//"."//int_to_string(minor)
  end subroutine gl_get_version


  !? This is to be used before attempting to call gl_get_error as OpenGL will always start off at error 1280.
  subroutine gl_clear_error_data()
    implicit none

    integer(c_int) :: i, error

    do i = 1,3
      error = gl_get_error()
    end do
  end subroutine gl_clear_error_data


  subroutine gl_get_shader_info_log(shader)
    use,intrinsic :: iso_c_binding
    use :: string_f90
    implicit none

    integer(c_int), intent(in), value :: shader
    integer(c_int) :: length
    type(c_ptr) :: c_string

    call internal_gl_get_shader_info_log(shader, 512, length, c_string)
  end subroutine gl_get_shader_info_log


  function gl_get_shader_iv(shader, pname) result(status)
    implicit none

    integer(c_int), intent(in), value :: shader, pname
    integer(c_int) :: status

    call internal_gl_get_shader_iv(shader, pname, status)
  end function gl_get_shader_iv


  function gl_get_program_iv(program_id, pname) result(status)
    use :: string_f90
    implicit none

    integer(c_int), intent(in), value :: program_id, pname
    integer(c_int) :: status

    call internal_gl_get_program_iv(program_id, pname, status)
  end function gl_get_program_iv


  integer function gl_get_uniform_location(program_id, uniform_name) result(location)
    use :: string_f90
    implicit none

    integer, intent(in), value :: program_id
    character(len = *, kind = c_char), intent(in) :: uniform_name

    location = internal_gl_get_uniform_location(program_id, into_c_string(uniform_name))
    ! print*,"uniform: ", uniform_name," | loc: ", location
  end function gl_get_uniform_location


  integer function gl_get_attrib_location(program_id, uniform_name) result(location)
    use :: string_f90
    implicit none

    integer, intent(in), value :: program_id
    character(len = *, kind = c_char), intent(in) :: uniform_name

    location = internal_gl_get_attrib_location(program_id, into_c_string(uniform_name))
    ! print*,"attrib: ", uniform_name," | loc: ", location
  end function gl_get_attrib_location


  !* Special note: I only use 1 at a time. So we're only going to use one at a time.
  !* This is written "wrong" on purpose.
  integer function gl_gen_vertex_arrays() result(location)
    implicit none

    call internal_gl_gen_vertex_arrays(1, location)
  end function gl_gen_vertex_arrays

  !* Special note: I only use 1 at a time. So we're only going to use one at a time.
  !* This is written "wrong" on purpose.
  subroutine gl_delete_vertex_arrays(location)
    implicit none

    integer(c_int), intent(in), value :: location

    call internal_gl_delete_vertex_arrays(1, location)
  end subroutine gl_delete_vertex_arrays


  !* Special note: I only use 1 at a time. So we're only going to use one at a time.
  !* This is written "wrong" on purpose.
  integer function gl_gen_buffers() result(location)
    implicit none

    call internal_gl_gen_buffers(1, location)
  end function gl_gen_buffers


  !* Special note: I only use 1 at a time. So we're only going to use one at a time.
  !* This is written "wrong" on purpose.
  subroutine gl_delete_buffers(location)
    implicit none

    integer(c_int), intent(in), value :: location

    call internal_gl_delete_buffers(1, location)
  end subroutine gl_delete_buffers


  !* This is a custom command to allow gl_buffer_data to use specific types.
  subroutine gl_buffer_float_array(float_array)
    use :: constants
    use, intrinsic :: iso_c_binding
    implicit none

    real(c_float), dimension(:), target :: float_array

    !! FIXME: Might be wrong.
    call internal_gl_buffer_data(GL_ARRAY_BUFFER, F32_SIZE * size(float_array), c_loc(float_array), GL_STATIC_DRAW)
  end subroutine gl_buffer_float_array


  !* This is a custom command to allow gl_buffer_data to use specific types.
  subroutine gl_buffer_vec3f_array(vec3f_array)
    use, intrinsic :: iso_c_binding
    use :: constants
    use :: vector_3f
    implicit none

    type(vec3f), dimension(:), target :: vec3f_array

    !! FIXME: Might be wrong.
    call internal_gl_buffer_data(GL_ARRAY_BUFFER, F32_SIZE * size(vec3f_array) * 3, c_loc(vec3f_array), GL_STATIC_DRAW)
  end subroutine gl_buffer_vec3f_array


  !* This is a custom command to allow gl_buffer_data to use specific types.
  subroutine gl_buffer_indices_array(indices_array)
    use :: constants
    use, intrinsic :: iso_c_binding
    implicit none

    integer(c_int), dimension(:), target :: indices_array
    integer(c_int) :: total_size, length_of_array

    length_of_array = size(indices_array)
    total_size = I32_SIZE * length_of_array

    !! FIXME: Might be wrong.
    call internal_gl_buffer_data(GL_ELEMENT_ARRAY_BUFFER, total_size, c_loc(indices_array), GL_STATIC_DRAW)
  end subroutine gl_buffer_indices_array


  subroutine gl_vertex_attrib_pointer(index, size, type, normalized, stride)
    use, intrinsic :: iso_c_binding
    implicit none

    integer, intent(in), value :: index, size, type, stride
    logical, intent(in), value :: normalized
    logical(c_bool) :: final_normalized

    ! Convert.
    final_normalized = normalized

    call internal_gl_vertex_attrib_pointer(index, size, type, final_normalized, stride, null())
  end subroutine gl_vertex_attrib_pointer


  subroutine gl_draw_elements(mode, count, type)
    implicit none

    integer, intent(in), value :: mode, count, type

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


  !* Special note: I only use 1 at a time. So we're only going to use one at a time.
  !* This is written "wrong" on purpose.
  integer function gl_gen_textures() result(location)
    implicit none

    call internal_gl_gen_textures(1, location)
  end function gl_gen_textures


  !* This is so you can buffer texture data without jumping through hoops.
  subroutine gl_tex_image_2d(target, level, internal_format, width, height, border, format, type, data)
    use, intrinsic :: iso_c_binding
    implicit none

    ! Implementation note: This is a hardwire for unsigned byte. If it's not working correctly in the future
    ! there should be additional implementations created.

    integer(c_int), intent(in), value :: target, level, internal_format, width, height, border, format, type
    integer(1), dimension(:), intent(in), target :: data

    call internal_gl_tex_image_2d(target, level, internal_format, width, height, border, format, type, c_loc(data))
  end subroutine gl_tex_image_2d


  !* Special note: I only use 1 at a time. So we're only going to use one at a time.
  !* This is written "wrong" on purpose.
  subroutine gl_delete_textures(location)
    use, intrinsic :: iso_c_binding
    implicit none

    integer(c_int), intent(in) :: location

    call internal_gl_delete_textures(1, location)
  end subroutine gl_delete_textures


  subroutine gl_depth_mask(flag)
    use, intrinsic :: iso_c_binding
    implicit none

    logical, intent(in), value :: flag
    logical(c_bool) :: c_flag

    c_flag = flag

    call internal_gl_depth_mask(c_flag)
  end subroutine gl_depth_mask


end module opengl
