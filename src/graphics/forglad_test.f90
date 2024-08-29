module forglad_test
  use :: glfw
  use, intrinsic :: iso_c_binding
  implicit none

  procedure(gl_clear_c), pointer :: gl_clear

  interface

    function glad_load_gl(glfw_get_proc_address) result(version) bind(c, name = "gladLoadGL")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_funptr), intent(in), value :: glfw_get_proc_address
      integer(c_int) :: version
    end function glad_load_gl


    subroutine internal_gl_clear(thing_to_clear) bind(c, name = "glad_glClear")
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: thing_to_clear
    end subroutine internal_gl_clear


    subroutine internal_gl_clear_color(r,g,b,a) bind(c, name = "glad_glClearColor")
      use, intrinsic :: iso_c_binding
      implicit none

      real(c_float), intent(in), value :: r, g, b, a
    end subroutine internal_gl_clear_color


    subroutine gl_enable(cap) bind(c, name = "glad_glEnable")
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: cap
    end subroutine gl_enable


    subroutine gl_disable(cap) bind(c, name = "glad_glDisable")
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: cap
    end subroutine gl_disable


    subroutine internal_gl_debug_message_callback(callback, user_param) bind(c, name = "glad_glDebugMessageCallback")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_funptr), intent(in), value :: callback
      type(c_ptr), intent(in), optional :: user_param
    end subroutine internal_gl_debug_message_callback


    function internal_gl_create_program() result(program_id) bind(c, name = "glad_glCreateProgram")
      use, intrinsic :: iso_c_binding
      implicit none

      !! This might cause problems, it's a uint.
      integer(c_int) :: program_id
    end function internal_gl_create_program


    subroutine gl_delete_program(program_id) bind(c, name = "glad_glDeleteProgram")
      use, intrinsic :: iso_c_binding
      implicit none

      !! This might cause problems, it's a uint.
      integer(c_int), intent(in), value :: program_id
    end subroutine gl_delete_program


    function gl_is_program(program_id) result(is_program) bind(c, name = "glad_glIsProgram")
      use, intrinsic :: iso_c_binding
      implicit none

      !! This might cause problems, it's a uint.
      integer(c_int), intent(in), value :: program_id
      logical(c_bool) :: is_program
    end function gl_is_program


    function internal_gl_create_shader(shader_type) result(shader_id) bind(c, name = "glad_glCreateShader")
      use, intrinsic :: iso_c_binding
      implicit none

      !! This might cause problems, it's a uint.
      integer(c_int), intent(in), value :: shader_type
      integer(c_int) :: shader_id
    end function internal_gl_create_shader


    subroutine gl_delete_shader(shader) bind(c, name = "glad_glDeleteShader")
      use, intrinsic :: iso_c_binding
      implicit none

      !! This might cause problems, it's a uint.
      integer(c_int), intent(in), value :: shader
    end subroutine gl_delete_shader


    subroutine internal_gl_shader_source(shader_id, count, source_code, string_length) bind(c, name = "glad_glShaderSource")
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: shader_id
      integer(c_int), intent(in), value :: count
      character(len = *, kind = c_char), intent(in) :: source_code
      !? Less than 0 represents that the string is null terminated. So use that only.
      type(c_ptr), intent(in), optional :: string_length
    end subroutine internal_gl_shader_source


    subroutine gl_compile_shader(shader_id) bind(c, name = "glad_glCompileShader")
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: shader_id
    end subroutine gl_compile_shader


    function gl_is_shader(shader_id) result(is_a_shader) bind(c, name = "glad_glIsShader")
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: shader_id
      logical(c_bool) :: is_a_shader
    end function gl_is_shader


    subroutine gl_get_integer_v(pname, data) bind(c, name = "glad_glGetIntegerv")
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: pname
      integer(c_int), intent(in), target :: data
    end subroutine gl_get_integer_v


    subroutine gl_attach_shader(program, shader) bind(c, name = "glad_glAttachShader")
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: program, shader
    end subroutine gl_attach_shader


    subroutine gl_detach_shader(program, shader) bind(c, name = "glad_glDetachShader")
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: program, shader
    end subroutine gl_detach_shader


    subroutine gl_link_program(program) bind(c, name = "glad_glLinkProgram")
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: program
    end subroutine gl_link_program


    function gl_get_error() result(error_code) bind(c, name = "glad_glGetError")
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int) :: error_code
    end function gl_get_error


    subroutine internal_gl_get_shader_iv(shader, pname, params) bind(c, name = "glad_glGetShaderiv")
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: shader, pname
      integer(c_int), intent(in) :: params
    end subroutine internal_gl_get_shader_iv


    subroutine internal_gl_get_shader_info_log(shader, max_length, length, info_log) bind(c, name = "glad_glGetShaderInfoLog")
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: shader, max_length
      integer(c_int), intent(in) :: length
      type(c_ptr), intent(in) :: info_log
    end subroutine internal_gl_get_shader_info_log


    subroutine internal_gl_get_program_iv(program, pname, params) bind(c, name = "glad_glGetProgramiv")
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: program, pname
      integer(c_int), intent(in) :: params
    end subroutine internal_gl_get_program_iv


    subroutine gl_validate_program(program_id) bind(c, name = "glad_glValidateProgram")
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: program_id
    end subroutine gl_validate_program


    integer(c_int) function internal_gl_get_uniform_location(program_id, uniform_name) bind(c, name = "glad_glGetUniformLocation")
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: program_id
      character(len = 1, kind = c_char), intent(in) :: uniform_name
    end function internal_gl_get_uniform_location


    integer(c_int) function internal_gl_get_attrib_location(program_id, attrib_name) bind(c, name = "glad_glGetAttribLocation")
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: program_id
      character(len = 1, kind = c_char), intent(in) :: attrib_name
    end function internal_gl_get_attrib_location


    subroutine gl_use_program(program_id) bind(c, name = "glad_glUseProgram")
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: program_id
    end subroutine gl_use_program


    subroutine internal_gl_gen_vertex_arrays(n, arrays) bind(c, name = "glad_glGenVertexArrays")
      use,intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: n
      !! This part is written wrong on purpose. I only want 1 not multiple.
      integer(c_int), intent(inout) :: arrays
    end subroutine internal_gl_gen_vertex_arrays


    subroutine internal_gl_delete_vertex_arrays(n, arrays) bind(c, name = "glad_glDeleteVertexArrays")
      use,intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: n
      !! This part is written wrong on purpose. I only want 1 not multiple.
      integer(c_int), intent(in) :: arrays
    end subroutine internal_gl_delete_vertex_arrays


    subroutine gl_bind_vertex_array(array) bind(c, name = "glad_glBindVertexArray")
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: array
    end subroutine gl_bind_vertex_array


    subroutine internal_gl_gen_buffers(n, buffers) bind(c, name = "glad_glGenBuffers")
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: n
      !! This part is written wrong on purpose. I only want 1 not multiple.
      integer(c_int), intent(inout) :: buffers
    end subroutine internal_gl_gen_buffers


    subroutine internal_gl_delete_buffers(n, buffers) bind(c, name = "glad_glDeleteBuffers")
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: n
      !! This part is written wrong on purpose. I only want 1 not multiple.
      integer(c_int), intent(in) :: buffers
    end subroutine internal_gl_delete_buffers


    subroutine gl_bind_buffer(target, buffer) bind(c, name = "glad_glBindBuffer")
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: target, buffer
    end subroutine gl_bind_buffer


    subroutine internal_gl_buffer_data(target, size, data, usage) bind(c, name ="glad_glBufferData")
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: target, size, usage
      type(c_ptr), intent(in), value :: data
    end subroutine internal_gl_buffer_data


    subroutine gl_enable_vertex_attrib_array(index) bind(c, name = "glad_glEnableVertexAttribArray")
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: index
    end subroutine gl_enable_vertex_attrib_array


    subroutine gl_disable_vertex_attrib_array(index) bind(c, name = "glad_glDisableVertexAttribArray")
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: index
    end subroutine gl_disable_vertex_attrib_array


    subroutine internal_gl_vertex_attrib_pointer(index, size, type, normalized, stride, pointer) bind(c, name = "glad_glVertexAttribPointer")
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: index, size, type, stride
      logical(c_bool), intent(in), value :: normalized
      type(c_ptr), intent(in), optional :: pointer
    end subroutine internal_gl_vertex_attrib_pointer


    subroutine internal_gl_draw_elements(mode, count, type, indices) bind(c, name = "glad_glDrawElements")
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: mode, count, type
      type(c_ptr), intent(in), optional :: indices
    end subroutine internal_gl_draw_elements


    subroutine internal_gl_uniform_matrix_4_fv(location, count, transpose, value) bind(c, name = "glad_glUniformMatrix4fv")
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: location, count
      logical(c_bool), intent(in), value :: transpose
      type(c_ptr), intent(in), value :: value
    end subroutine internal_gl_uniform_matrix_4_fv


    subroutine gl_view_port(x, y, width, height) bind(c, name = "glad_glViewport")
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: x, y, width, height
    end subroutine gl_view_port


    function gl_is_buffer(buffer) result(is_buffer) bind(c, name = "glad_glIsBuffer")
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: buffer
      logical(c_bool) :: is_buffer
    end function gl_is_buffer


    function gl_is_vertex_array(array) result(is_array) bind(c, name = "glad_glIsVertexArray")
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: array
      logical(c_bool) :: is_array
    end function gl_is_vertex_array


    subroutine internal_gl_gen_textures(n, textures) bind(c, name = "glad_glGenTextures")
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: n
      !! This part is written wrong on purpose. I only want 1 not multiple.
      integer(c_int), intent(inout) :: textures
    end subroutine internal_gl_gen_textures


    subroutine gl_bind_texture(target, texture) bind(c, name = "glad_glBindTexture")
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: target, texture
    end subroutine gl_bind_texture


    subroutine gl_tex_parameter_i(target, pname, param) bind(c, name = "glad_glTexParameteri")
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: target, pname, param
    end subroutine gl_tex_parameter_i


    subroutine gl_tex_parameter_fv(target, pname, params) bind(c, name = "glad_glTexParameterfv")
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: target, pname
      real(c_float), dimension(:) :: params
    end subroutine gl_tex_parameter_fv


    subroutine gl_pixel_store_i(pname, param) bind(c, name = "glad_glPixelStorei")
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: pname, param
    end subroutine gl_pixel_store_i


    subroutine internal_gl_tex_image_2d(target, level, internal_format, width, height, border, format, type, data) bind(c, name = "glad_glTexImage2D")
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: target, level, internal_format, width, height, border, format, type
      type(c_ptr), intent(in), value :: data
    end subroutine internal_gl_tex_image_2d


    function gl_is_texture(texture) result(is_texture) bind(c, name = "glad_glIsTexture")
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: texture
      logical(c_bool) :: is_texture
    end function gl_is_texture


    subroutine gl_generate_mipmap(target) bind(c, name = "glad_glGenerateMipmap")
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: target
    end subroutine gl_generate_mipmap


    subroutine internal_gl_delete_textures(n, textures) bind(c, name = "glad_glDeleteTextures")
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: n
      !! This part is written wrong on purpose. I only want 1 not multiple.
      integer(c_int), intent(in) :: textures
    end subroutine internal_gl_delete_textures


    subroutine internal_gl_depth_mask(flag) bind(c, name = "glad_glDepthMask")
      use, intrinsic :: iso_c_binding
      implicit none

      logical(c_bool), intent(in), value :: flag
    end subroutine internal_gl_depth_mask


    subroutine gl_depth_func(func) bind(c, name = "glad_glDepthFunc")
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: func
    end subroutine gl_depth_func


    subroutine gl_depth_range_f(near_val, far_val) bind(c, name = "glad_glDepthRangef")
      use, intrinsic :: iso_c_binding
      implicit none

      real(c_float), intent(in), value :: near_val, far_val
    end subroutine gl_depth_range_f


    subroutine gl_blend_equation(mode) bind(c, name = "glad_glBlendEquation")
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: mode
    end subroutine gl_blend_equation


    subroutine gl_blend_func(s_factor, d_factor) bind(c, name = "glad_glBlendFunc")
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: s_factor, d_factor
    end subroutine gl_blend_func


    subroutine gl_blend_func_separate(src_rgb, dst_rgb, src_alpha, dst_alpha) bind(c, name = "glad_glBlendFuncSeparate")
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int), intent(in), value :: src_rgb, dst_rgb, src_alpha, dst_alpha
    end subroutine gl_blend_func_separate


  end interface

contains


  subroutine forglad_test_init()
    use :: string
    implicit none

    type(c_funptr) :: function_pointer
    character(len = :, kind = c_char), allocatable :: function_name

    function_name = into_c_string("glClear")
    function_pointer = glfw_get_proc_address(function_name)
    call c_f_procpointer(function_pointer, gl_clear)

  end subroutine forglad_test_init

end module forglad_test
