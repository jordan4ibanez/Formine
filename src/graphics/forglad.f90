module forglad
  use, intrinsic :: iso_c_binding
  implicit none


  private


  public :: forglad_load_gl
  public :: forglad_gpu_supports_gl_debugging


  ! These are OpenGL constants.

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


  ! These are the actual function pointers for OpenGL.
  !? They start off as undefined.

  procedure(gl_get_string_c_interface), public, pointer :: gl_get_string
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
  procedure(gl_depth_range_c_interface), public, pointer :: gl_depth_range
  procedure(gl_blend_equation_c_interface), public, pointer :: gl_blend_equation
  procedure(gl_blend_func_c_interface), public, pointer :: gl_blend_func
  procedure(gl_blend_func_separate_c_interface), public, pointer :: gl_blend_func_separate


  interface


    function gl_get_string_c_interface(name) result(string_pointer)
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: name
      type(c_ptr) :: string_pointer
    end function gl_get_string_c_interface


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
    subroutine gl_depth_range_c_interface(near_val, far_val) bind(c)
      use, intrinsic :: iso_c_binding
      implicit none

      real(c_double), intent(in), value :: near_val, far_val
    end subroutine gl_depth_range_c_interface


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


    !* This is a hackjob to prevent circular dependencies.
    !* This mimics interface: glfw_get_proc_address
    function proc_address_finder_func(procname) result(address)
      use, intrinsic :: iso_c_binding
      implicit none

      character(len = 1, kind = c_char), intent(in), optional :: procname
      type(c_funptr) :: address
    end function proc_address_finder_func


  end interface


contains


  !* This finds the base version of OpenGL that your GPU supports.
  !* Will check if it is greater than or equal to OpenGL 4.1.
  function is_opengl_4_1_capable() result(success)
    use :: string_f90
    implicit none

    integer(c_int) :: major, minor
    logical(c_bool) :: success

    success = .false.

    call gl_get_integer_v(GL_MAJOR_VERSION, major)
    call gl_get_integer_v(GL_MINOR_VERSION, minor)

    if (major < 4) then
      return
    end if
    if (minor < 1) then
      return
    end if

    success = .true.
  end function is_opengl_4_1_capable


  !* A GPU needs OpenGL 4.3 support to enable debugging.
  function forglad_gpu_supports_gl_debugging() result(ok_to_debug)
    use :: string_f90
    implicit none

    integer(c_int) :: major, minor
    logical(c_bool) :: ok_to_debug

    ok_to_debug = .false.

    call gl_get_integer_v(GL_MAJOR_VERSION, major)
    call gl_get_integer_v(GL_MINOR_VERSION, minor)

    if (major < 4) then
      return
    end if
    if (minor < 3) then
      return
    end if

    ok_to_debug = .true.
  end function forglad_gpu_supports_gl_debugging


  !* Loads up the function pointers for OpenGL.
  !* This gets a function pointer passed into it to prevent a circular dependency.
  subroutine forglad_load_gl(proc_address_finder_raw)
    use :: string_f90
    implicit none

    type(c_funptr), intent(in), value :: proc_address_finder_raw
    procedure(proc_address_finder_func), pointer :: proc_address_finder
    type(c_funptr) :: function_pointer

    ! todo: could make this a clone of glad in fortran, maybe.

    ! Transfer the function pointer into something fortran understands.
    call c_f_procpointer(proc_address_finder_raw, proc_address_finder)

    ! glGetIntegerv is supported in OpenGL 2.0.
    ! If your machine is older than this, I'm surprised it was able to get this far in the program.
    function_pointer = proc_address_finder("glGetIntegerv"//achar(0))
    call c_f_procpointer(function_pointer, gl_get_integer_v)

    if (.not. is_opengl_4_1_capable()) then
      error stop "[Forglad] Critical Error: GPU not capable of OpenGL 4.1 context."
    end if

    function_pointer = proc_address_finder("glGetString"//achar(0))
    call c_f_procpointer(function_pointer, gl_get_string)

    function_pointer = proc_address_finder("glClear"//achar(0))
    call c_f_procpointer(function_pointer, internal_gl_clear)

    function_pointer = proc_address_finder("glClearColor"//achar(0))
    call c_f_procpointer(function_pointer, internal_gl_clear_color)

    function_pointer = proc_address_finder("glEnable"//achar(0))
    call c_f_procpointer(function_pointer, gl_enable)

    function_pointer = proc_address_finder("glDisable"//achar(0))
    call c_f_procpointer(function_pointer, gl_disable)

    ! Don't bother loading this pointer if it doesn't exist.
    if (forglad_gpu_supports_gl_debugging()) then
      function_pointer = proc_address_finder("glDebugMessageCallback"//achar(0))
      call c_f_procpointer(function_pointer, internal_gl_debug_message_callback)
    end if

    function_pointer = proc_address_finder("glCreateProgram"//achar(0))
    call c_f_procpointer(function_pointer, internal_gl_create_program)

    function_pointer = proc_address_finder("glDeleteProgram"//achar(0))
    call c_f_procpointer(function_pointer, gl_delete_program)

    function_pointer = proc_address_finder("glIsProgram"//achar(0))
    call c_f_procpointer(function_pointer, gl_is_program)

    function_pointer = proc_address_finder("glCreateShader"//achar(0))
    call c_f_procpointer(function_pointer, internal_gl_create_shader)

    function_pointer = proc_address_finder("glDeleteShader"//achar(0))
    call c_f_procpointer(function_pointer, gl_delete_shader)

    function_pointer = proc_address_finder("glShaderSource"//achar(0))
    call c_f_procpointer(function_pointer, internal_gl_shader_source)

    function_pointer = proc_address_finder("glCompileShader"//achar(0))
    call c_f_procpointer(function_pointer, gl_compile_shader)

    function_pointer = proc_address_finder("glIsShader"//achar(0))
    call c_f_procpointer(function_pointer, gl_is_shader)

    function_pointer = proc_address_finder("glAttachShader"//achar(0))
    call c_f_procpointer(function_pointer, gl_attach_shader)

    function_pointer = proc_address_finder("glDetachShader"//achar(0))
    call c_f_procpointer(function_pointer, gl_detach_shader)

    function_pointer = proc_address_finder("glLinkProgram"//achar(0))
    call c_f_procpointer(function_pointer, gl_link_program)

    function_pointer = proc_address_finder("glGetError"//achar(0))
    call c_f_procpointer(function_pointer, gl_get_error)

    function_pointer = proc_address_finder("glGetShaderiv"//achar(0))
    call c_f_procpointer(function_pointer, internal_gl_get_shader_iv)

    function_pointer = proc_address_finder("glGetShaderInfoLog"//achar(0))
    call c_f_procpointer(function_pointer, internal_gl_get_shader_info_log)

    function_pointer = proc_address_finder("glGetProgramiv"//achar(0))
    call c_f_procpointer(function_pointer, internal_gl_get_program_iv)

    function_pointer = proc_address_finder("glValidateProgram"//achar(0))
    call c_f_procpointer(function_pointer, gl_validate_program)

    function_pointer = proc_address_finder("glGetUniformLocation"//achar(0))
    call c_f_procpointer(function_pointer, internal_gl_get_uniform_location)

    function_pointer = proc_address_finder("glGetAttribLocation"//achar(0))
    call c_f_procpointer(function_pointer, internal_gl_get_attrib_location)

    function_pointer = proc_address_finder("glUseProgram"//achar(0))
    call c_f_procpointer(function_pointer, gl_use_program)

    function_pointer = proc_address_finder("glGenVertexArrays"//achar(0))
    call c_f_procpointer(function_pointer, internal_gl_gen_vertex_arrays)

    function_pointer = proc_address_finder("glDeleteVertexArrays"//achar(0))
    call c_f_procpointer(function_pointer, internal_gl_delete_vertex_arrays)

    function_pointer = proc_address_finder("glBindVertexArray"//achar(0))
    call c_f_procpointer(function_pointer, gl_bind_vertex_array)

    function_pointer = proc_address_finder("glGenBuffers"//achar(0))
    call c_f_procpointer(function_pointer, internal_gl_gen_buffers)

    function_pointer = proc_address_finder("glDeleteBuffers"//achar(0))
    call c_f_procpointer(function_pointer, internal_gl_delete_buffers)

    function_pointer = proc_address_finder("glBindBuffer"//achar(0))
    call c_f_procpointer(function_pointer, gl_bind_buffer)

    function_pointer = proc_address_finder("glBufferData"//achar(0))
    call c_f_procpointer(function_pointer, internal_gl_buffer_data)

    function_pointer = proc_address_finder("glEnableVertexAttribArray"//achar(0))
    call c_f_procpointer(function_pointer, gl_enable_vertex_attrib_array)

    function_pointer = proc_address_finder("glDisableVertexAttribArray"//achar(0))
    call c_f_procpointer(function_pointer, gl_disable_vertex_attrib_array)

    function_pointer = proc_address_finder("glVertexAttribPointer"//achar(0))
    call c_f_procpointer(function_pointer, internal_gl_vertex_attrib_pointer)

    function_pointer = proc_address_finder("glDrawElements"//achar(0))
    call c_f_procpointer(function_pointer, internal_gl_draw_elements)

    function_pointer = proc_address_finder("glUniformMatrix4fv"//achar(0))
    call c_f_procpointer(function_pointer, internal_gl_uniform_matrix_4_fv)

    function_pointer = proc_address_finder("glViewport"//achar(0))
    call c_f_procpointer(function_pointer, gl_view_port)

    function_pointer = proc_address_finder("glIsBuffer"//achar(0))
    call c_f_procpointer(function_pointer, gl_is_buffer)

    function_pointer = proc_address_finder("glIsVertexArray"//achar(0))
    call c_f_procpointer(function_pointer, gl_is_vertex_array)

    function_pointer = proc_address_finder("glGenTextures"//achar(0))
    call c_f_procpointer(function_pointer, internal_gl_gen_textures)

    function_pointer = proc_address_finder("glBindTexture"//achar(0))
    call c_f_procpointer(function_pointer, gl_bind_texture)

    function_pointer = proc_address_finder("glTexParameteri"//achar(0))
    call c_f_procpointer(function_pointer, gl_tex_parameter_i)

    function_pointer = proc_address_finder("glTexParameterfv"//achar(0))
    call c_f_procpointer(function_pointer, gl_tex_parameter_fv)

    function_pointer = proc_address_finder("glPixelStorei"//achar(0))
    call c_f_procpointer(function_pointer, gl_pixel_store_i)

    function_pointer = proc_address_finder("glTexImage2D"//achar(0))
    call c_f_procpointer(function_pointer, internal_gl_tex_image_2d)

    function_pointer = proc_address_finder("glIsTexture"//achar(0))
    call c_f_procpointer(function_pointer, gl_is_texture)

    function_pointer = proc_address_finder("glGenerateMipmap"//achar(0))
    call c_f_procpointer(function_pointer, gl_generate_mipmap)

    function_pointer = proc_address_finder("glDeleteTextures"//achar(0))
    call c_f_procpointer(function_pointer, internal_gl_delete_textures)

    function_pointer = proc_address_finder("glDepthMask"//achar(0))
    call c_f_procpointer(function_pointer, internal_gl_depth_mask)

    function_pointer = proc_address_finder("glDepthFunc"//achar(0))
    call c_f_procpointer(function_pointer, gl_depth_func)

    function_pointer = proc_address_finder("glDepthRange"//achar(0))
    call c_f_procpointer(function_pointer, gl_depth_range)

    function_pointer = proc_address_finder("glBlendEquation"//achar(0))
    call c_f_procpointer(function_pointer, gl_blend_equation)

    function_pointer = proc_address_finder("glBlendFunc"//achar(0))
    call c_f_procpointer(function_pointer, gl_blend_func)

    function_pointer = proc_address_finder("glBlendFuncSeparate"//achar(0))
    call c_f_procpointer(function_pointer, gl_blend_func_separate)
  end subroutine forglad_load_gl


end module forglad
