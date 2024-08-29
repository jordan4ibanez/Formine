module forglad
  use :: glfw
  use, intrinsic :: iso_c_binding
  implicit none

  procedure(gl_clear_c_interface), pointer :: gl_clear


  interface


    subroutine gl_clear_c_interface(thing_to_clear) bind(c)
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: thing_to_clear
    end subroutine gl_clear_c_interface


    subroutine gl_clear_color_c_interface(r,g,b,a) bind(c)
      use, intrinsic :: iso_c_binding
      implicit none

      real(c_float), intent(in), value :: r, g, b, a
    end subroutine gl_clear_color_c_interface


    subroutine gl_enable_c_interface(cap) bind(c)
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: cap
    end subroutine gl_enable_c_interface


    subroutine gl_disable_c_interface(cap) bind(c)
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: cap
    end subroutine gl_disable_c_interface


    subroutine gl_debug_message_callback_c_interface(callback, user_param) bind(c)
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_funptr), intent(in), value :: callback
      type(c_ptr), intent(in), optional :: user_param
    end subroutine gl_debug_message_callback_c_interface


    function gl_create_program_c_interface() result(program_id) bind(c)
      use, intrinsic :: iso_c_binding
      implicit none

      !! This might cause problems, it's a uint.
      integer(c_int) :: program_id
    end function gl_create_program_c_interface


    subroutine gl_delete_program_c_interface(program_id) bind(c)
      use, intrinsic :: iso_c_binding
      implicit none

      !! This might cause problems, it's a uint.
      integer(c_int), intent(in), value :: program_id
    end subroutine gl_delete_program_c_interface


    function gl_is_program_c_interface(program_id) result(is_program) bind(c)
      use, intrinsic :: iso_c_binding
      implicit none

      !! This might cause problems, it's a uint.
      integer(c_int), intent(in), value :: program_id
      logical(c_bool) :: is_program
    end function gl_is_program_c_interface


    function gl_create_shader_c_interface(shader_type) result(shader_id) bind(c)
      use, intrinsic :: iso_c_binding
      implicit none

      !! This might cause problems, it's a uint.
      integer(c_int), intent(in), value :: shader_type
      integer(c_int) :: shader_id
    end function gl_create_shader_c_interface


    subroutine gl_delete_shader_c_interface(shader) bind(c)
      use, intrinsic :: iso_c_binding
      implicit none

      !! This might cause problems, it's a uint.
      integer(c_int), intent(in), value :: shader
    end subroutine gl_delete_shader_c_interface


    subroutine gl_shader_source_c_interface(shader_id, count, source_code, string_length) bind(c)
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: shader_id
      integer(c_int), intent(in), value :: count
      character(len = *, kind = c_char), intent(in) :: source_code
      !? Less than 0 represents that the string is null terminated. So use that only.
      type(c_ptr), intent(in), optional :: string_length
    end subroutine gl_shader_source_c_interface


    subroutine gl_compile_shader_c_interface(shader_id) bind(c)
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: shader_id
    end subroutine gl_compile_shader_c_interface


    function gl_is_shader_c_interface(shader_id) result(is_a_shader) bind(c)
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: shader_id
      logical(c_bool) :: is_a_shader
    end function gl_is_shader_c_interface


    subroutine gl_get_integer_v_c_interface(pname, data) bind(c)
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: pname
      integer(c_int), intent(in), target :: data
    end subroutine gl_get_integer_v_c_interface


    subroutine gl_attach_shader_c_interface(program, shader) bind(c)
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: program, shader
    end subroutine gl_attach_shader_c_interface


    subroutine gl_detach_shader_c_interface(program, shader) bind(c)
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: program, shader
    end subroutine gl_detach_shader_c_interface


    subroutine gl_link_program_c_interface(program) bind(c)
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: program
    end subroutine gl_link_program_c_interface


    function gl_get_error_c_interface() result(error_code) bind(c)
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int) :: error_code
    end function gl_get_error_c_interface


    subroutine gl_get_shader_iv_c_interface(shader, pname, params) bind(c)
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: shader, pname
      integer(c_int), intent(in) :: params
    end subroutine gl_get_shader_iv_c_interface


    subroutine gl_get_shader_info_log_c_interface(shader, max_length, length, info_log) bind(c)
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: shader, max_length
      integer(c_int), intent(in) :: length
      type(c_ptr), intent(in) :: info_log
    end subroutine gl_get_shader_info_log_c_interface


    subroutine gl_get_program_iv_c_interface(program, pname, params) bind(c)
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: program, pname
      integer(c_int), intent(in) :: params
    end subroutine gl_get_program_iv_c_interface


    subroutine gl_validate_program_c_interface(program_id) bind(c)
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: program_id
    end subroutine gl_validate_program_c_interface


    integer(c_int) function gl_get_uniform_location_c_interface(program_id, uniform_name) bind(c)
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: program_id
      character(len = 1, kind = c_char), intent(in) :: uniform_name
    end function gl_get_uniform_location_c_interface


    integer(c_int) function gl_get_attrib_location_c_interface(program_id, attrib_name) bind(c)
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: program_id
      character(len = 1, kind = c_char), intent(in) :: attrib_name
    end function gl_get_attrib_location_c_interface


    subroutine gl_use_program_c_interface(program_id) bind(c)
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: program_id
    end subroutine gl_use_program_c_interface


    subroutine gl_gen_vertex_arrays_c_interface(n, arrays) bind(c)
      use,intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: n
      !! This part is written wrong on purpose. I only want 1 not multiple.
      integer(c_int), intent(inout) :: arrays
    end subroutine gl_gen_vertex_arrays_c_interface


    subroutine gl_delete_vertex_arrays_c_interface(n, arrays) bind(c)
      use,intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: n
      !! This part is written wrong on purpose. I only want 1 not multiple.
      integer(c_int), intent(in) :: arrays
    end subroutine gl_delete_vertex_arrays_c_interface


    subroutine gl_bind_vertex_array_c_interface(array) bind(c)
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: array
    end subroutine gl_bind_vertex_array_c_interface


    subroutine gl_gen_buffers_c_interface(n, buffers) bind(c)
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: n
      !! This part is written wrong on purpose. I only want 1 not multiple.
      integer(c_int), intent(inout) :: buffers
    end subroutine gl_gen_buffers_c_interface


    subroutine gl_delete_buffers_c_interface(n, buffers) bind(c)
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: n
      !! This part is written wrong on purpose. I only want 1 not multiple.
      integer(c_int), intent(in) :: buffers
    end subroutine gl_delete_buffers_c_interface


    subroutine gl_bind_buffer_c_interface(target, buffer) bind(c)
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: target, buffer
    end subroutine gl_bind_buffer_c_interface


    subroutine gl_buffer_data_c_interface(target, size, data, usage) bind(c)
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: target, size, usage
      type(c_ptr), intent(in), value :: data
    end subroutine gl_buffer_data_c_interface


    subroutine gl_enable_vertex_attrib_array_c_interface(index) bind(c)
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: index
    end subroutine gl_enable_vertex_attrib_array_c_interface


    subroutine gl_disable_vertex_attrib_array(index) bind(c)
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: index
    end subroutine gl_disable_vertex_attrib_array


    subroutine gl_vertex_attrib_pointer_c_interface(index, size, type, normalized, stride, pointer) bind(c)
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: index, size, type, stride
      logical(c_bool), intent(in), value :: normalized
      type(c_ptr), intent(in), optional :: pointer
    end subroutine gl_vertex_attrib_pointer_c_interface


    subroutine gl_draw_elements_c_interface(mode, count, type, indices) bind(c)
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: mode, count, type
      type(c_ptr), intent(in), optional :: indices
    end subroutine gl_draw_elements_c_interface


    subroutine gl_uniform_matrix_4_fv_c_interface(location, count, transpose, value) bind(c)
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: location, count
      logical(c_bool), intent(in), value :: transpose
      type(c_ptr), intent(in), value :: value
    end subroutine gl_uniform_matrix_4_fv_c_interface


    subroutine gl_view_port_c_interface(x, y, width, height) bind(c)
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: x, y, width, height
    end subroutine gl_view_port_c_interface


    function gl_is_buffer_c_interface(buffer) result(is_buffer) bind(c)
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: buffer
      logical(c_bool) :: is_buffer
    end function gl_is_buffer_c_interface


    function gl_is_vertex_array_c_interface(array) result(is_array) bind(c)
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: array
      logical(c_bool) :: is_array
    end function gl_is_vertex_array_c_interface


    subroutine gl_gen_textures_c_interface(n, textures) bind(c)
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: n
      !! This part is written wrong on purpose. I only want 1 not multiple.
      integer(c_int), intent(inout) :: textures
    end subroutine gl_gen_textures_c_interface


    subroutine gl_bind_texture_c_interface(target, texture) bind(c)
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: target, texture
    end subroutine gl_bind_texture_c_interface


    subroutine gl_tex_parameter_i_c_interface(target, pname, param) bind(c)
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: target, pname, param
    end subroutine gl_tex_parameter_i_c_interface


    subroutine gl_tex_parameter_fv_c_interface(target, pname, params) bind(c)
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: target, pname
      real(c_float), dimension(:) :: params
    end subroutine gl_tex_parameter_fv_c_interface


    subroutine gl_pixel_store_i_c_interface(pname, param) bind(c)
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: pname, param
    end subroutine gl_pixel_store_i_c_interface


    subroutine gl_tex_image_2d_c_interface(target, level, internal_format, width, height, border, format, type, data) bind(c)
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: target, level, format, width, height, border, internal_format, type
      type(c_ptr), intent(in), value :: data
    end subroutine gl_tex_image_2d_c_interface


    function gl_is_texture_c_interface(texture) result(is_texture) bind(c)
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: texture
      logical(c_bool) :: is_texture
    end function gl_is_texture_c_interface


    subroutine gl_generate_mipmap_c_interface(target) bind(c)
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: target
    end subroutine gl_generate_mipmap_c_interface


    subroutine gl_delete_textures_c_interface(n, textures) bind(c)
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: n
      !! This part is written wrong on purpose. I only want 1 not multiple.
      integer(c_int), intent(in) :: textures
    end subroutine gl_delete_textures_c_interface


    subroutine gl_depth_mask_c_interface(flag) bind(c)
      use, intrinsic :: iso_c_binding
      implicit none

      logical(c_bool), intent(in), value :: flag
    end subroutine gl_depth_mask_c_interface


    subroutine gl_depth_func_c_interface(func) bind(c)
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: func
    end subroutine gl_depth_func_c_interface


    subroutine gl_depth_range_f_c_interface(near_val, far_val) bind(c)
      use, intrinsic :: iso_c_binding
      implicit none

      real(c_float), intent(in), value :: near_val, far_val
    end subroutine gl_depth_range_f_c_interface


    subroutine gl_blend_equation_c_interface(mode) bind(c)
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: mode
    end subroutine gl_blend_equation_c_interface


    subroutine gl_blend_func_c_interface(s_factor, d_factor) bind(c)
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: s_factor, d_factor
    end subroutine gl_blend_func_c_interface


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
    character(len = :, kind = c_char), allocatable :: function_name

    function_name = into_c_string("glClear")
    function_pointer = glfw_get_proc_address(function_name)
    call c_f_procpointer(function_pointer, gl_clear)

  end subroutine forglad_init

end module forglad
