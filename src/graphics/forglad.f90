module forglad
  use :: glfw
  use, intrinsic :: iso_c_binding
  implicit none


  private


  public :: forglad_init


  procedure(gl_clear_c_interface), public, pointer :: gl_clear
  procedure(gl_clear_color_c_interface), public, pointer :: gl_clear_color
  procedure(gl_enable_c_interface), public, pointer :: gl_enable
  procedure(gl_disable_c_interface), public, pointer :: gl_disable
  procedure(gl_debug_message_callback_c_interface), public, pointer :: gl_debug_message_callback
  procedure(gl_create_program_c_interface), public, pointer :: gl_create_program
  procedure(gl_delete_program_c_interface), public, pointer :: gl_delete_program
  procedure(gl_is_program_c_interface), public, pointer :: gl_is_program
  procedure(gl_create_shader_c_interface), public, pointer :: gl_create_shader
  procedure(gl_delete_shader_c_interface), public, pointer :: gl_delete_shader
  procedure(gl_shader_source_c_interface), public, pointer :: gl_shader_source
  procedure(gl_compile_shader_c_interface), public, pointer :: gl_compile_shader
  procedure(gl_is_shader_c_interface), public, pointer :: gl_is_shader
  procedure(gl_get_integer_v_c_interface), public, pointer :: gl_get_integer_v
  procedure(gl_attach_shader_c_interface), public, pointer :: gl_attach_shader
  procedure(gl_link_program_c_interface), public, pointer :: gl_link_program
  procedure(gl_get_error_c_interface), public, pointer :: gl_get_error
  procedure(gl_get_shader_iv_c_interface), public, pointer :: gl_get_shader_iv
  procedure(gl_get_shader_info_log_c_interface), public, pointer :: gl_get_shader_info_log
  procedure(gl_get_program_iv_c_interface), public, pointer :: gl_get_program_iv
  procedure(gl_validate_program_c_interface), public, pointer :: gl_validate_program
  procedure(gl_get_uniform_location_c_interface), public, pointer :: gl_get_uniform_location
  procedure(gl_get_attrib_location_c_interface), public, pointer :: gl_get_attrib_location
  procedure(gl_use_program_c_interface), public, pointer :: gl_use_program
  procedure(gl_gen_vertex_arrays_c_interface), public, pointer :: gl_gen_vertex_arrays
  procedure(gl_delete_vertex_arrays_c_interface), public, pointer :: gl_delete_vertex_arrays
  procedure(gl_bind_vertex_array_c_interface), public, pointer :: gl_bind_vertex_array
  procedure(gl_gen_buffers_c_interface), public, pointer :: gl_gen_buffers
  procedure(gl_delete_buffers_c_interface), public, pointer :: gl_delete_buffers
  procedure(gl_bind_buffer_c_interface), public, pointer :: gl_bind_buffer
  procedure(gl_buffer_data_c_interface), public, pointer :: gl_buffer_data
  procedure(gl_enable_vertex_attrib_array_c_interface), public, pointer :: gl_enable_vertex_attrib_array
  procedure(gl_disable_vertex_attrib_array_c_interface), public, pointer :: gl_disable_vertex_attrib_array
  procedure(gl_vertex_attrib_pointer_c_interface), public, pointer :: gl_vertex_attrib_pointer
  procedure(gl_draw_elements_c_interface), public, pointer :: gl_draw_elements
  procedure(gl_uniform_matrix_4_fv_c_interface), public, pointer :: gl_uniform_matrix_4_fv
  procedure(gl_view_port_c_interface), public, pointer :: gl_view_port
  procedure(gl_is_buffer_c_interface), public, pointer :: gl_is_buffer
  procedure(gl_is_vertex_array_c_interface), public, pointer :: gl_is_vertex_array
  procedure(gl_gen_textures_c_interface), public, pointer :: gl_gen_textures
  procedure(gl_bind_texture_c_interface), public, pointer :: gl_bind_texture
  procedure(gl_tex_parameter_i_c_interface), public, pointer :: gl_tex_parameter_i
  procedure(gl_tex_parameter_fv_c_interface), public, pointer :: gl_tex_parameter_fv
  procedure(gl_pixel_store_i_c_interface), public, pointer :: gl_pixel_store_i
  procedure(gl_tex_image_2d_c_interface), public, pointer :: gl_tex_image_2d
  procedure(gl_is_texture_c_interface), public, pointer :: gl_is_texture
  procedure(gl_generate_mipmap_c_interface), public, pointer :: gl_generate_mipmap
  procedure(gl_delete_textures_c_interface), public, pointer :: gl_delete_textures
  procedure(gl_depth_mask_c_interface), public, pointer :: gl_depth_mask
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


  subroutine forglad_init()
    use :: string
    implicit none

    type(c_funptr) :: function_pointer

    ! todo: could make this a clone of glad in fortran, maybe.

    ! todo: just use achar(0) so this is more portable with less allocations.



    function_pointer = glfw_get_proc_address(into_c_string("glClear"))
    call c_f_procpointer(function_pointer, gl_clear)

    function_pointer = glfw_get_proc_address(into_c_string("glClearColor"))
    call c_f_procpointer(function_pointer, gl_clear_color)

    function_pointer = glfw_get_proc_address(into_c_string("glEnable"))
    call c_f_procpointer(function_pointer, gl_enable)

    function_pointer = glfw_get_proc_address(into_c_string("glDisable"))
    call c_f_procpointer(function_pointer, gl_disable)

    function_pointer = glfw_get_proc_address(into_c_string("glDebugMessageCallback"))
    call c_f_procpointer(function_pointer, gl_debug_message_callback)

    function_pointer = glfw_get_proc_address(into_c_string("glCreateProgram"))
    call c_f_procpointer(function_pointer, gl_create_program)

    function_pointer = glfw_get_proc_address(into_c_string("glDeleteProgram"))
    call c_f_procpointer(function_pointer, gl_delete_program)

    function_pointer = glfw_get_proc_address(into_c_string("glIsProgram"))
    call c_f_procpointer(function_pointer, gl_is_program)

    function_pointer = glfw_get_proc_address(into_c_string("glCreateShader"))
    call c_f_procpointer(function_pointer, gl_create_shader)

    function_pointer = glfw_get_proc_address(into_c_string("glDeleteShader"))
    call c_f_procpointer(function_pointer, gl_delete_shader)

    function_pointer = glfw_get_proc_address(into_c_string("glShaderSource"))
    call c_f_procpointer(function_pointer, gl_shader_source)

    function_pointer = glfw_get_proc_address(into_c_string("glCompileShader"))
    call c_f_procpointer(function_pointer, gl_compile_shader)

    function_pointer = glfw_get_proc_address(into_c_string("glIsShader"))
    call c_f_procpointer(function_pointer, gl_is_shader)

    function_pointer = glfw_get_proc_address(into_c_string("glGetIntegerv"))
    call c_f_procpointer(function_pointer, gl_get_integer_v)

    function_pointer = glfw_get_proc_address(into_c_string("glAttachShader"))
    call c_f_procpointer(function_pointer, gl_attach_shader)

    function_pointer = glfw_get_proc_address(into_c_string("glLinkProgram"))
    call c_f_procpointer(function_pointer, gl_link_program)

    function_pointer = glfw_get_proc_address(into_c_string("glGetError"))
    call c_f_procpointer(function_pointer, gl_get_error)

    function_pointer = glfw_get_proc_address(into_c_string("glGetShaderiv"))
    call c_f_procpointer(function_pointer, gl_get_shader_iv)

    function_pointer = glfw_get_proc_address(into_c_string("glGetShaderInfoLog"))
    call c_f_procpointer(function_pointer, gl_get_shader_info_log)

    function_pointer = glfw_get_proc_address(into_c_string("glGetProgramiv"))
    call c_f_procpointer(function_pointer, gl_get_program_iv)

    function_pointer = glfw_get_proc_address(into_c_string("glValidateProgram"))
    call c_f_procpointer(function_pointer, gl_validate_program)

    function_pointer = glfw_get_proc_address(into_c_string("glGetUniformLocation"))
    call c_f_procpointer(function_pointer, gl_get_uniform_location)

    function_pointer = glfw_get_proc_address(into_c_string("glGetAttribLocation"))
    call c_f_procpointer(function_pointer, gl_get_attrib_location)

    function_pointer = glfw_get_proc_address(into_c_string("glUseProgram"))
    call c_f_procpointer(function_pointer, gl_use_program)

    function_pointer = glfw_get_proc_address(into_c_string("glGenVertexArrays"))
    call c_f_procpointer(function_pointer, gl_gen_vertex_arrays)

    function_pointer = glfw_get_proc_address(into_c_string("glDeleteVertexArrays"))
    call c_f_procpointer(function_pointer, gl_delete_vertex_arrays)

    function_pointer = glfw_get_proc_address(into_c_string("glBindVertexArray"))
    call c_f_procpointer(function_pointer, gl_bind_vertex_array)

    function_pointer = glfw_get_proc_address(into_c_string("glGenBuffers"))
    call c_f_procpointer(function_pointer, gl_gen_buffers)

    function_pointer = glfw_get_proc_address(into_c_string("glDeleteBuffers"))
    call c_f_procpointer(function_pointer, gl_delete_buffers)

    function_pointer = glfw_get_proc_address(into_c_string("glBindBuffer"))
    call c_f_procpointer(function_pointer, gl_bind_buffer)

    function_pointer = glfw_get_proc_address(into_c_string("glBufferData"))
    call c_f_procpointer(function_pointer, gl_buffer_data)

    function_pointer = glfw_get_proc_address(into_c_string("glEnableVertexAttribArray"))
    call c_f_procpointer(function_pointer, gl_enable_vertex_attrib_array)

    function_pointer = glfw_get_proc_address(into_c_string("glDisableVertexAttribArray"))
    call c_f_procpointer(function_pointer, gl_disable_vertex_attrib_array)

    function_pointer = glfw_get_proc_address(into_c_string("glVertexAttribPointer"))
    call c_f_procpointer(function_pointer, gl_vertex_attrib_pointer)

    function_pointer = glfw_get_proc_address(into_c_string("glDrawElements"))
    call c_f_procpointer(function_pointer, gl_draw_elements)

    function_pointer = glfw_get_proc_address(into_c_string("glUniform4fv"))
    call c_f_procpointer(function_pointer, gl_uniform_matrix_4_fv)

    function_pointer = glfw_get_proc_address(into_c_string("glViewport"))
    call c_f_procpointer(function_pointer, gl_view_port)

    function_pointer = glfw_get_proc_address(into_c_string("glIsBuffer"))
    call c_f_procpointer(function_pointer, gl_is_buffer)

    function_pointer = glfw_get_proc_address(into_c_string("glIsVertexArray"))
    call c_f_procpointer(function_pointer, gl_is_vertex_array)

    function_pointer = glfw_get_proc_address(into_c_string("glGenTextures"))
    call c_f_procpointer(function_pointer, gl_gen_textures)

    function_pointer = glfw_get_proc_address(into_c_string("glBindTexture"))
    call c_f_procpointer(function_pointer, gl_bind_texture)

    function_pointer = glfw_get_proc_address(into_c_string("glTextureParameteri"))
    call c_f_procpointer(function_pointer, gl_tex_parameter_i)

    function_pointer = glfw_get_proc_address(into_c_string("glTexParameterfv"))
    call c_f_procpointer(function_pointer, gl_tex_parameter_fv)

    function_pointer = glfw_get_proc_address(into_c_string("glPixelStorei"))
    call c_f_procpointer(function_pointer, gl_pixel_store_i)

    function_pointer = glfw_get_proc_address(into_c_string("glTexImage2D"))
    call c_f_procpointer(function_pointer, gl_tex_image_2d)

    function_pointer = glfw_get_proc_address(into_c_string("glIsTexture"))
    call c_f_procpointer(function_pointer, gl_is_texture)

    function_pointer = glfw_get_proc_address(into_c_string("glGenerateMipmap"))
    call c_f_procpointer(function_pointer, gl_generate_mipmap)

    function_pointer = glfw_get_proc_address(into_c_string("glDeleteTextures"))
    call c_f_procpointer(function_pointer, gl_delete_textures)

    function_pointer = glfw_get_proc_address(into_c_string("glDepthMask"))
    call c_f_procpointer(function_pointer, gl_depth_mask)

    function_pointer = glfw_get_proc_address(into_c_string("glDepthFunc"))
    call c_f_procpointer(function_pointer, gl_depth_func)

    function_pointer = glfw_get_proc_address(into_c_string("glDepthRangef"))
    call c_f_procpointer(function_pointer, gl_depth_range_f)

    function_pointer = glfw_get_proc_address(into_c_string("glBlendEquation"))
    call c_f_procpointer(function_pointer, gl_blend_equation)

    function_pointer = glfw_get_proc_address(into_c_string("glBlendFunc"))
    call c_f_procpointer(function_pointer, gl_blend_func)

    function_pointer = glfw_get_proc_address(into_c_string("glBlendFuncSeparate"))
    call c_f_procpointer(function_pointer, gl_blend_func_separate)
  end subroutine forglad_init


end module forglad
