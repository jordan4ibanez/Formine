module forglad
  use :: glfw
  use, intrinsic :: iso_c_binding
  implicit none


  private


  public :: forglad_load_gl


  integer, parameter, public :: GL_VERSION = int(z"1f02")
  integer, parameter, public :: GL_MAJOR_VERSION = int(z"821B")
  integer, parameter, public :: GL_MINOR_VERSION = int(z"821C")
  integer, parameter, public :: GL_TRUE = 1
  integer, parameter, public :: GL_FALSE = 0
  integer, parameter, public :: GL_FLOAT = int(z"1406")
  integer, parameter, public :: GL_COLOR_BUFFER_BIT = int(z"00004000")
  integer, parameter, public :: GL_DEBUG_OUTPUT_SYNCHRONOUS = int(z"8242")
  integer, parameter, public :: GL_VERTEX_SHADER = int(z"8B31")
  integer, parameter, public :: GL_FRAGMENT_SHADER = int(z"8B30")
  integer, parameter, public :: GL_COMPILE_STATUS = int(z"8B81")
  integer, parameter, public :: GL_LINK_STATUS = int(z"8B82")
  integer, parameter, public :: GL_VALIDATE_STATUS = int(z"8B83")
  integer, parameter, public :: GL_DEBUG_SEVERITY_NOTIFICATION = int(z"826B")
  integer, parameter, public :: GL_DEBUG_SEVERITY_LOW = int(z"9148")
  integer, parameter, public :: GL_DEBUG_SEVERITY_MEDIUM = int(z"9147")
  integer, parameter, public :: GL_DEBUG_SEVERITY_HIGH = int(z"9146")
  integer, parameter, public :: GL_STATIC_DRAW = int(z"88E4")
  integer, parameter, public :: GL_ARRAY_BUFFER = int(z"8892")
  integer, parameter, public :: GL_ELEMENT_ARRAY_BUFFER = int(z"8893")
  integer, parameter, public :: GL_TRIANGLES = int(z"0004")
  integer, parameter, public :: GL_UNSIGNED_INT = int(z"1405")
  integer, parameter, public :: GL_TEXTURE_2D = int(z"0DE1")
  integer, parameter, public :: GL_TEXTURE_WRAP_S = int(z"2802")
  integer, parameter, public :: GL_TEXTURE_WRAP_T = int(z"2803")
  integer, parameter, public :: GL_CLAMP_TO_BORDER = int(z"812D")
  integer, parameter, public :: GL_TEXTURE_BORDER_COLOR = int(z"1004")
  integer, parameter, public :: GL_NEAREST = int(z"2600")
  integer, parameter, public :: GL_NEAREST_MIPMAP_LINEAR = int(z"2702")
  integer, parameter, public :: GL_NEAREST_MIPMAP_NEAREST = int(z"2700")
  integer, parameter, public :: GL_TEXTURE_MIN_FILTER = int(z"2801")
  integer, parameter, public :: GL_TEXTURE_MAG_FILTER = int(z"2800")
  integer, parameter, public :: GL_UNPACK_ALIGNMENT = int(z"0CF5")
  integer, parameter, public :: GL_RGBA = int(z"1908")
  integer, parameter, public :: GL_UNSIGNED_BYTE = int(z"1401")
  integer, parameter, public :: GL_DEPTH_TEST = int(z"0B71")
  integer, parameter, public :: GL_LESS = int(z"0201")
  integer, parameter, public :: GL_CULL_FACE = int(z"0B44")
  integer, parameter, public :: GL_DEPTH_BUFFER_BIT = int(z"00000100")
  integer, parameter, public :: GL_BLEND = int(z"0BE2")
  integer, parameter, public :: GL_FUNC_ADD = int(z"8006")
  integer, parameter, public :: GL_SRC_ALPHA = int(z"0302")
  integer, parameter, public :: GL_ONE_MINUS_SRC_ALPHA = int(z"0303")
  integer, parameter, public :: GL_ONE = 1


  procedure(gl_clear_c_interface), public, pointer :: internal_gl_clear
  procedure(gl_clear_color_c_interface), public, pointer :: internal_gl_clear_color
  procedure(gl_enable_c_interface), public, pointer :: gl_enable
  procedure(gl_disable_c_interface), public, pointer :: gl_disable
  procedure(gl_debug_message_callback_c_interface), public, pointer :: internal_gl_debug_message_callback
  procedure(gl_create_program_c_interface), public, pointer :: internal_gl_create_program
  procedure(gl_delete_program_c_interface), public, pointer :: gl_delete_program
  procedure(gl_is_program_c_interface), public, pointer :: gl_is_program
  procedure(gl_create_shader_c_interface), public, pointer :: internal_gl_create_shader
  procedure(gl_delete_shader_c_interface), public, pointer :: gl_delete_shader
  procedure(gl_shader_source_c_interface), public, pointer :: internal_gl_shader_source
  procedure(gl_compile_shader_c_interface), public, pointer :: gl_compile_shader
  procedure(gl_is_shader_c_interface), public, pointer :: gl_is_shader
  procedure(gl_get_integer_v_c_interface), public, pointer :: gl_get_integer_v
  procedure(gl_attach_shader_c_interface), public, pointer :: gl_attach_shader
  procedure(gl_detach_shader_c_interface), public, pointer :: gl_detach_shader
  procedure(gl_link_program_c_interface), public, pointer :: gl_link_program
  procedure(gl_get_error_c_interface), public, pointer :: gl_get_error
  procedure(gl_get_shader_iv_c_interface), public, pointer :: internal_gl_get_shader_iv
  procedure(gl_get_shader_info_log_c_interface), public, pointer :: internal_gl_get_shader_info_log
  procedure(gl_get_program_iv_c_interface), public, pointer :: internal_gl_get_program_iv
  procedure(gl_validate_program_c_interface), public, pointer :: gl_validate_program
  procedure(gl_get_uniform_location_c_interface), public, pointer :: internal_gl_get_uniform_location
  procedure(gl_get_attrib_location_c_interface), public, pointer :: internal_gl_get_attrib_location
  procedure(gl_use_program_c_interface), public, pointer :: gl_use_program
  procedure(gl_gen_vertex_arrays_c_interface), public, pointer :: internal_gl_gen_vertex_arrays
  procedure(gl_delete_vertex_arrays_c_interface), public, pointer :: internal_gl_delete_vertex_arrays
  procedure(gl_bind_vertex_array_c_interface), public, pointer :: gl_bind_vertex_array
  procedure(gl_gen_buffers_c_interface), public, pointer :: internal_gl_gen_buffers
  procedure(gl_delete_buffers_c_interface), public, pointer :: internal_gl_delete_buffers
  procedure(gl_bind_buffer_c_interface), public, pointer :: gl_bind_buffer
  procedure(gl_buffer_data_c_interface), public, pointer :: internal_gl_buffer_data
  procedure(gl_enable_vertex_attrib_array_c_interface), public, pointer :: gl_enable_vertex_attrib_array
  procedure(gl_disable_vertex_attrib_array_c_interface), public, pointer :: gl_disable_vertex_attrib_array
  procedure(gl_vertex_attrib_pointer_c_interface), public, pointer :: internal_gl_vertex_attrib_pointer
  procedure(gl_draw_elements_c_interface), public, pointer :: internal_gl_draw_elements
  procedure(gl_uniform_matrix_4_fv_c_interface), public, pointer :: internal_gl_uniform_matrix_4_fv
  procedure(gl_view_port_c_interface), public, pointer :: gl_view_port
  procedure(gl_is_buffer_c_interface), public, pointer :: gl_is_buffer
  procedure(gl_is_vertex_array_c_interface), public, pointer :: gl_is_vertex_array
  procedure(gl_gen_textures_c_interface), public, pointer :: internal_gl_gen_textures
  procedure(gl_bind_texture_c_interface), public, pointer :: gl_bind_texture
  procedure(gl_tex_parameter_i_c_interface), public, pointer :: gl_tex_parameter_i
  procedure(gl_tex_parameter_fv_c_interface), public, pointer :: gl_tex_parameter_fv
  procedure(gl_pixel_store_i_c_interface), public, pointer :: gl_pixel_store_i
  procedure(gl_tex_image_2d_c_interface), public, pointer :: internal_gl_tex_image_2d
  procedure(gl_is_texture_c_interface), public, pointer :: gl_is_texture
  procedure(gl_generate_mipmap_c_interface), public, pointer :: gl_generate_mipmap
  procedure(gl_delete_textures_c_interface), public, pointer :: internal_gl_delete_textures
  procedure(gl_depth_mask_c_interface), public, pointer :: internal_gl_depth_mask
  procedure(gl_depth_func_c_interface), public, pointer :: gl_depth_func
  procedure(gl_depth_range_f_c_interface), public, pointer :: gl_depth_range_f
  procedure(gl_blend_equation_c_interface), public, pointer :: gl_blend_equation
  procedure(gl_blend_func_c_interface), public, pointer :: gl_blend_func
  procedure(gl_blend_func_separate_c_interface), public, pointer :: gl_blend_func_separate


  interface


!! DONE.
    subroutine gl_clear_c_interface(thing_to_clear) bind(c)
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: thing_to_clear
    end subroutine gl_clear_c_interface


!! DONE.
    subroutine gl_clear_color_c_interface(r,g,b,a) bind(c)
      use, intrinsic :: iso_c_binding
      implicit none

      real(c_float), intent(in), value :: r, g, b, a
    end subroutine gl_clear_color_c_interface


!! DONE.
    subroutine gl_enable_c_interface(cap) bind(c)
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: cap
    end subroutine gl_enable_c_interface


!! DONE.
    subroutine gl_disable_c_interface(cap) bind(c)
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: cap
    end subroutine gl_disable_c_interface

!! DONE.
    subroutine gl_debug_message_callback_c_interface(callback, user_param) bind(c)
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_funptr), intent(in), value :: callback
      type(c_ptr), intent(in), optional :: user_param
    end subroutine gl_debug_message_callback_c_interface

!! DONE.
    function gl_create_program_c_interface() result(program_id) bind(c)
      use, intrinsic :: iso_c_binding
      implicit none

      !? This might cause problems, it's a uint.
      integer(c_int) :: program_id
    end function gl_create_program_c_interface


!! DONE.
    subroutine gl_delete_program_c_interface(program_id) bind(c)
      use, intrinsic :: iso_c_binding
      implicit none

      !? This might cause problems, it's a uint.
      integer(c_int), intent(in), value :: program_id
    end subroutine gl_delete_program_c_interface


!! DONE.
    function gl_is_program_c_interface(program_id) result(is_program) bind(c)
      use, intrinsic :: iso_c_binding
      implicit none

      !? This might cause problems, it's a uint.
      integer(c_int), intent(in), value :: program_id
      logical(c_bool) :: is_program
    end function gl_is_program_c_interface


!! DONE.
    function gl_create_shader_c_interface(shader_type) result(shader_id) bind(c)
      use, intrinsic :: iso_c_binding
      implicit none

      !? This might cause problems, it's a uint.
      integer(c_int), intent(in), value :: shader_type
      integer(c_int) :: shader_id
    end function gl_create_shader_c_interface


!! DONE.
    subroutine gl_delete_shader_c_interface(shader) bind(c)
      use, intrinsic :: iso_c_binding
      implicit none

      !? This might cause problems, it's a uint.
      integer(c_int), intent(in), value :: shader
    end subroutine gl_delete_shader_c_interface


!! DONE.
    subroutine gl_shader_source_c_interface(shader_id, count, source_code, string_length) bind(c)
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: shader_id
      integer(c_int), intent(in), value :: count
      character(len = *, kind = c_char), intent(in) :: source_code
      !? Less than 0 represents that the string is null terminated. So use that only.
      type(c_ptr), intent(in), optional :: string_length
    end subroutine gl_shader_source_c_interface


!! DONE.
    subroutine gl_compile_shader_c_interface(shader_id) bind(c)
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: shader_id
    end subroutine gl_compile_shader_c_interface


!! DONE.
    function gl_is_shader_c_interface(shader_id) result(is_a_shader) bind(c)
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: shader_id
      logical(c_bool) :: is_a_shader
    end function gl_is_shader_c_interface


!! DONE.
    subroutine gl_get_integer_v_c_interface(pname, data) bind(c)
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: pname
      integer(c_int), intent(in), target :: data
    end subroutine gl_get_integer_v_c_interface


!! DONE.
    subroutine gl_attach_shader_c_interface(program, shader) bind(c)
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: program, shader
    end subroutine gl_attach_shader_c_interface


!! DONE.
    subroutine gl_detach_shader_c_interface(program, shader) bind(c)
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: program, shader
    end subroutine gl_detach_shader_c_interface


!! DONE.
    subroutine gl_link_program_c_interface(program) bind(c)
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: program
    end subroutine gl_link_program_c_interface


!! DONE.
    function gl_get_error_c_interface() result(error_code) bind(c)
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int) :: error_code
    end function gl_get_error_c_interface


!! DONE.
    subroutine gl_get_shader_iv_c_interface(shader, pname, params) bind(c)
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: shader, pname
      integer(c_int), intent(in) :: params
    end subroutine gl_get_shader_iv_c_interface


!! DONE.
    subroutine gl_get_shader_info_log_c_interface(shader, max_length, length, info_log) bind(c)
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: shader, max_length
      integer(c_int), intent(in) :: length
      type(c_ptr), intent(in) :: info_log
    end subroutine gl_get_shader_info_log_c_interface


!! DONE.
    subroutine gl_get_program_iv_c_interface(program, pname, params) bind(c)
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: program, pname
      integer(c_int), intent(in) :: params
    end subroutine gl_get_program_iv_c_interface


!! DONE.
    subroutine gl_validate_program_c_interface(program_id) bind(c)
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: program_id
    end subroutine gl_validate_program_c_interface


!! DONE.
    integer(c_int) function gl_get_uniform_location_c_interface(program_id, uniform_name) bind(c)
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: program_id
      character(len = 1, kind = c_char), intent(in) :: uniform_name
    end function gl_get_uniform_location_c_interface


!! DONE.
    integer(c_int) function gl_get_attrib_location_c_interface(program_id, attrib_name) bind(c)
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: program_id
      character(len = 1, kind = c_char), intent(in) :: attrib_name
    end function gl_get_attrib_location_c_interface


!! DONE.
    subroutine gl_use_program_c_interface(program_id) bind(c)
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: program_id
    end subroutine gl_use_program_c_interface


!! DONE.
    subroutine gl_gen_vertex_arrays_c_interface(n, arrays) bind(c)
      use,intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: n
      !? This part is written wrong on purpose. I only want 1 not multiple.
      integer(c_int), intent(inout) :: arrays
    end subroutine gl_gen_vertex_arrays_c_interface


!! DONE.
    subroutine gl_delete_vertex_arrays_c_interface(n, arrays) bind(c)
      use,intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: n
      !? This part is written wrong on purpose. I only want 1 not multiple.
      integer(c_int), intent(in) :: arrays
    end subroutine gl_delete_vertex_arrays_c_interface


!! DONE.
    subroutine gl_bind_vertex_array_c_interface(array) bind(c)
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: array
    end subroutine gl_bind_vertex_array_c_interface


!! DONE.
    subroutine gl_gen_buffers_c_interface(n, buffers) bind(c)
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: n
      !? This part is written wrong on purpose. I only want 1 not multiple.
      integer(c_int), intent(inout) :: buffers
    end subroutine gl_gen_buffers_c_interface


!! DONE.
    subroutine gl_delete_buffers_c_interface(n, buffers) bind(c)
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: n
      !? This part is written wrong on purpose. I only want 1 not multiple.
      integer(c_int), intent(in) :: buffers
    end subroutine gl_delete_buffers_c_interface


!! DONE.
    subroutine gl_bind_buffer_c_interface(target, buffer) bind(c)
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: target, buffer
    end subroutine gl_bind_buffer_c_interface


!! DONE.
    subroutine gl_buffer_data_c_interface(target, size, data, usage) bind(c)
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: target, size, usage
      type(c_ptr), intent(in), value :: data
    end subroutine gl_buffer_data_c_interface


!! DONE.
    subroutine gl_enable_vertex_attrib_array_c_interface(index) bind(c)
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: index
    end subroutine gl_enable_vertex_attrib_array_c_interface


!! DONE.
    subroutine gl_disable_vertex_attrib_array_c_interface(index) bind(c)
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: index
    end subroutine gl_disable_vertex_attrib_array_c_interface


!! DONE.
    subroutine gl_vertex_attrib_pointer_c_interface(index, size, type, normalized, stride, pointer) bind(c)
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: index, size, type, stride
      logical(c_bool), intent(in), value :: normalized
      type(c_ptr), intent(in), optional :: pointer
    end subroutine gl_vertex_attrib_pointer_c_interface


!! DONE.
    subroutine gl_draw_elements_c_interface(mode, count, type, indices) bind(c)
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: mode, count, type
      type(c_ptr), intent(in), optional :: indices
    end subroutine gl_draw_elements_c_interface


!! DONE.
    subroutine gl_uniform_matrix_4_fv_c_interface(location, count, transpose, value) bind(c)
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: location, count
      logical(c_bool), intent(in), value :: transpose
      type(c_ptr), intent(in), value :: value
    end subroutine gl_uniform_matrix_4_fv_c_interface


!! DONE.
    subroutine gl_view_port_c_interface(x, y, width, height) bind(c)
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: x, y, width, height
    end subroutine gl_view_port_c_interface


!! DONE.
    function gl_is_buffer_c_interface(buffer) result(is_buffer) bind(c)
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: buffer
      logical(c_bool) :: is_buffer
    end function gl_is_buffer_c_interface


!! DONE.
    function gl_is_vertex_array_c_interface(array) result(is_array) bind(c)
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: array
      logical(c_bool) :: is_array
    end function gl_is_vertex_array_c_interface


!! DONE.
    subroutine gl_gen_textures_c_interface(n, textures) bind(c)
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: n
      !? This part is written wrong on purpose. I only want 1 not multiple.
      integer(c_int), intent(inout) :: textures
    end subroutine gl_gen_textures_c_interface


!! DONE.
    subroutine gl_bind_texture_c_interface(target, texture) bind(c)
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: target, texture
    end subroutine gl_bind_texture_c_interface


!! DONE.
    subroutine gl_tex_parameter_i_c_interface(target, pname, param) bind(c)
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: target, pname, param
    end subroutine gl_tex_parameter_i_c_interface


!! DONE.
    subroutine gl_tex_parameter_fv_c_interface(target, pname, params) bind(c)
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: target, pname
      real(c_float), dimension(:) :: params
    end subroutine gl_tex_parameter_fv_c_interface


!! DONE.
    subroutine gl_pixel_store_i_c_interface(pname, param) bind(c)
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: pname, param
    end subroutine gl_pixel_store_i_c_interface


!! DONE.
    subroutine gl_tex_image_2d_c_interface(target, level, internal_format, width, height, border, format, type, data) bind(c)
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: target, level, format, width, height, border, internal_format, type
      type(c_ptr), intent(in), value :: data
    end subroutine gl_tex_image_2d_c_interface


!! DONE.
    function gl_is_texture_c_interface(texture) result(is_texture) bind(c)
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: texture
      logical(c_bool) :: is_texture
    end function gl_is_texture_c_interface


!! DONE.
    subroutine gl_generate_mipmap_c_interface(target) bind(c)
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: target
    end subroutine gl_generate_mipmap_c_interface


!! DONE.
    subroutine gl_delete_textures_c_interface(n, textures) bind(c)
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: n
      !? This part is written wrong on purpose. I only want 1 not multiple.
      integer(c_int), intent(in) :: textures
    end subroutine gl_delete_textures_c_interface


!! DONE.
    subroutine gl_depth_mask_c_interface(flag) bind(c)
      use, intrinsic :: iso_c_binding
      implicit none

      logical(c_bool), intent(in), value :: flag
    end subroutine gl_depth_mask_c_interface


!! DONE.
    subroutine gl_depth_func_c_interface(func) bind(c)
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: func
    end subroutine gl_depth_func_c_interface


!! DONE.
    subroutine gl_depth_range_f_c_interface(near_val, far_val) bind(c)
      use, intrinsic :: iso_c_binding
      implicit none

      real(c_float), intent(in), value :: near_val, far_val
    end subroutine gl_depth_range_f_c_interface


!! DONE.
    subroutine gl_blend_equation_c_interface(mode) bind(c)
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: mode
    end subroutine gl_blend_equation_c_interface


!! DONE.
    subroutine gl_blend_func_c_interface(s_factor, d_factor) bind(c)
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: s_factor, d_factor
    end subroutine gl_blend_func_c_interface


!! DONE.
    subroutine gl_blend_func_separate_c_interface(src_rgb, dst_rgb, src_alpha, dst_alpha) bind(c)
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: src_rgb, dst_rgb, src_alpha, dst_alpha
    end subroutine gl_blend_func_separate_c_interface


  end interface


contains


  subroutine forglad_load_gl()
    use :: string
    implicit none

    type(c_funptr) :: function_pointer

    ! todo: could make this a clone of glad in fortran, maybe.

    function_pointer = glfw_get_proc_address("glClear"//achar(0))
    call c_f_procpointer(function_pointer, internal_gl_clear)

    function_pointer = glfw_get_proc_address("glClearColor"//achar(0))
    call c_f_procpointer(function_pointer, internal_gl_clear_color)

    function_pointer = glfw_get_proc_address("glEnable"//achar(0))
    call c_f_procpointer(function_pointer, gl_enable)

    function_pointer = glfw_get_proc_address("glDisable"//achar(0))
    call c_f_procpointer(function_pointer, gl_disable)

    function_pointer = glfw_get_proc_address("glDebugMessageCallback"//achar(0))
    call c_f_procpointer(function_pointer, internal_gl_debug_message_callback)

    function_pointer = glfw_get_proc_address("glCreateProgram"//achar(0))
    call c_f_procpointer(function_pointer, internal_gl_create_program)

    function_pointer = glfw_get_proc_address("glDeleteProgram"//achar(0))
    call c_f_procpointer(function_pointer, gl_delete_program)

    function_pointer = glfw_get_proc_address("glIsProgram"//achar(0))
    call c_f_procpointer(function_pointer, gl_is_program)

    function_pointer = glfw_get_proc_address("glCreateShader"//achar(0))
    call c_f_procpointer(function_pointer, internal_gl_create_shader)

    function_pointer = glfw_get_proc_address("glDeleteShader"//achar(0))
    call c_f_procpointer(function_pointer, gl_delete_shader)

    function_pointer = glfw_get_proc_address("glShaderSource"//achar(0))
    call c_f_procpointer(function_pointer, internal_gl_shader_source)

    function_pointer = glfw_get_proc_address("glCompileShader"//achar(0))
    call c_f_procpointer(function_pointer, gl_compile_shader)

    function_pointer = glfw_get_proc_address("glIsShader"//achar(0))
    call c_f_procpointer(function_pointer, gl_is_shader)

    function_pointer = glfw_get_proc_address("glGetIntegerv"//achar(0))
    call c_f_procpointer(function_pointer, gl_get_integer_v)

    function_pointer = glfw_get_proc_address("glAttachShader"//achar(0))
    call c_f_procpointer(function_pointer, gl_attach_shader)

    function_pointer = glfw_get_proc_address("glDetachShader"//achar(0))
    call c_f_procpointer(function_pointer, gl_detach_shader)

    function_pointer = glfw_get_proc_address("glLinkProgram"//achar(0))
    call c_f_procpointer(function_pointer, gl_link_program)

    function_pointer = glfw_get_proc_address("glGetError"//achar(0))
    call c_f_procpointer(function_pointer, gl_get_error)

    function_pointer = glfw_get_proc_address("glGetShaderiv"//achar(0))
    call c_f_procpointer(function_pointer, internal_gl_get_shader_iv)

    function_pointer = glfw_get_proc_address("glGetShaderInfoLog"//achar(0))
    call c_f_procpointer(function_pointer, internal_gl_get_shader_info_log)

    function_pointer = glfw_get_proc_address("glGetProgramiv"//achar(0))
    call c_f_procpointer(function_pointer, internal_gl_get_program_iv)

    function_pointer = glfw_get_proc_address("glValidateProgram"//achar(0))
    call c_f_procpointer(function_pointer, gl_validate_program)

    function_pointer = glfw_get_proc_address("glGetUniformLocation"//achar(0))
    call c_f_procpointer(function_pointer, internal_gl_get_uniform_location)

    function_pointer = glfw_get_proc_address("glGetAttribLocation"//achar(0))
    call c_f_procpointer(function_pointer, internal_gl_get_attrib_location)

    function_pointer = glfw_get_proc_address("glUseProgram"//achar(0))
    call c_f_procpointer(function_pointer, gl_use_program)

    function_pointer = glfw_get_proc_address("glGenVertexArrays"//achar(0))
    call c_f_procpointer(function_pointer, internal_gl_gen_vertex_arrays)

    function_pointer = glfw_get_proc_address("glDeleteVertexArrays"//achar(0))
    call c_f_procpointer(function_pointer, internal_gl_delete_vertex_arrays)

    function_pointer = glfw_get_proc_address("glBindVertexArray"//achar(0))
    call c_f_procpointer(function_pointer, gl_bind_vertex_array)

    function_pointer = glfw_get_proc_address("glGenBuffers"//achar(0))
    call c_f_procpointer(function_pointer, internal_gl_gen_buffers)

    function_pointer = glfw_get_proc_address("glDeleteBuffers"//achar(0))
    call c_f_procpointer(function_pointer, internal_gl_delete_buffers)

    function_pointer = glfw_get_proc_address("glBindBuffer"//achar(0))
    call c_f_procpointer(function_pointer, gl_bind_buffer)

    function_pointer = glfw_get_proc_address("glBufferData"//achar(0))
    call c_f_procpointer(function_pointer, internal_gl_buffer_data)

    function_pointer = glfw_get_proc_address("glEnableVertexAttribArray"//achar(0))
    call c_f_procpointer(function_pointer, gl_enable_vertex_attrib_array)

    function_pointer = glfw_get_proc_address("glDisableVertexAttribArray"//achar(0))
    call c_f_procpointer(function_pointer, gl_disable_vertex_attrib_array)

    function_pointer = glfw_get_proc_address("glVertexAttribPointer"//achar(0))
    call c_f_procpointer(function_pointer, internal_gl_vertex_attrib_pointer)

    function_pointer = glfw_get_proc_address("glDrawElements"//achar(0))
    call c_f_procpointer(function_pointer, internal_gl_draw_elements)

    function_pointer = glfw_get_proc_address("glUniform4fv"//achar(0))
    call c_f_procpointer(function_pointer, internal_gl_uniform_matrix_4_fv)

    function_pointer = glfw_get_proc_address("glViewport"//achar(0))
    call c_f_procpointer(function_pointer, gl_view_port)

    function_pointer = glfw_get_proc_address("glIsBuffer"//achar(0))
    call c_f_procpointer(function_pointer, gl_is_buffer)

    function_pointer = glfw_get_proc_address("glIsVertexArray"//achar(0))
    call c_f_procpointer(function_pointer, gl_is_vertex_array)

    function_pointer = glfw_get_proc_address("glGenTextures"//achar(0))
    call c_f_procpointer(function_pointer, internal_gl_gen_textures)

    function_pointer = glfw_get_proc_address("glBindTexture"//achar(0))
    call c_f_procpointer(function_pointer, gl_bind_texture)

    function_pointer = glfw_get_proc_address("glTextureParameteri"//achar(0))
    call c_f_procpointer(function_pointer, gl_tex_parameter_i)

    function_pointer = glfw_get_proc_address("glTexParameterfv"//achar(0))
    call c_f_procpointer(function_pointer, gl_tex_parameter_fv)

    function_pointer = glfw_get_proc_address("glPixelStorei"//achar(0))
    call c_f_procpointer(function_pointer, gl_pixel_store_i)

    function_pointer = glfw_get_proc_address("glTexImage2D"//achar(0))
    call c_f_procpointer(function_pointer, internal_gl_tex_image_2d)

    function_pointer = glfw_get_proc_address("glIsTexture"//achar(0))
    call c_f_procpointer(function_pointer, gl_is_texture)

    function_pointer = glfw_get_proc_address("glGenerateMipmap"//achar(0))
    call c_f_procpointer(function_pointer, gl_generate_mipmap)

    function_pointer = glfw_get_proc_address("glDeleteTextures"//achar(0))
    call c_f_procpointer(function_pointer, internal_gl_delete_textures)

    function_pointer = glfw_get_proc_address("glDepthMask"//achar(0))
    call c_f_procpointer(function_pointer, internal_gl_depth_mask)

    function_pointer = glfw_get_proc_address("glDepthFunc"//achar(0))
    call c_f_procpointer(function_pointer, gl_depth_func)

    function_pointer = glfw_get_proc_address("glDepthRangef"//achar(0))
    call c_f_procpointer(function_pointer, gl_depth_range_f)

    function_pointer = glfw_get_proc_address("glBlendEquation"//achar(0))
    call c_f_procpointer(function_pointer, gl_blend_equation)

    function_pointer = glfw_get_proc_address("glBlendFunc"//achar(0))
    call c_f_procpointer(function_pointer, gl_blend_func)

    function_pointer = glfw_get_proc_address("glBlendFuncSeparate"//achar(0))
    call c_f_procpointer(function_pointer, gl_blend_func_separate)
  end subroutine forglad_load_gl


end module forglad
