module shader
  use :: hashmap_str
  use, intrinsic :: iso_c_binding
  use :: opengl
  implicit none


  private

  public :: shader_module_initialize
  public :: shader_create
  public :: shader_start
  public :: shader_destroy_database

  public :: ATTRIBUTE_POSITION
  public :: ATTRIBUTE_TEXTURE_COORDINATE
  public :: ATTRIBUTE_COLOR

  public :: UNIFORM_CAMERA_MATRIX
  public :: UNIFORM_OBJECT_MATRIX


  integer(c_int), parameter :: ATTRIBUTE_POSITION = 0
  integer(c_int), parameter :: ATTRIBUTE_TEXTURE_COORDINATE = 1
  integer(c_int), parameter :: ATTRIBUTE_COLOR = 2

  integer(c_int) :: UNIFORM_CAMERA_MATRIX
  integer(c_int) :: UNIFORM_OBJECT_MATRIX

  !* Type: Integer(c_int)
  type(hashmap_string_key) :: shader_database


contains


  !* Initialize the shader database.
  subroutine shader_module_initialize()
    implicit none

    shader_database = new_hashmap_string_key(sizeof(10), gc_shader_database)
  end subroutine shader_module_initialize


  !* This is a simple way to check if a shader is null. (0)
  !? Makes the code easier to read.
  !? This also is making it so the program that uses it can return the success and work logic on it at the same time.
  logical function creation_succeeded(input) result(success)
    implicit none

    integer, intent(in), value :: input

    !? 0 means a failure in this context.
    success = input /= GL_FALSE
  end function creation_succeeded


  !* This is a simple variation of shader_creation_succeeded with gl_check_error as our helper.
  !? Same docs as in shader_creation_success minus the input.
  logical function shader_compilation_succeeded(shader_id) result(success)
    implicit none

    integer, intent(in), value :: shader_id

    !? 1 means OK in this context.
    success = gl_get_shader_iv(shader_id, GL_COMPILE_STATUS) == GL_TRUE
  end function shader_compilation_succeeded


  !* Create a named shader program from vertex and fragment code file paths.
  !? Will return false if it fails, true if it succeeds.
  subroutine shader_create(shader_name, vertex_code_file_path, fragment_code_file_path)
    use :: string_f90
    implicit none

    character(len = *, kind = c_char), intent(in) :: shader_name
    character(len = *, kind = c_char), intent(in) :: vertex_code_file_path
    character(len = *, kind = c_char), intent(in) :: fragment_code_file_path
    integer(c_int) :: vertex_id, fragment_id
    integer(c_int) :: program_id

    print"(A)","[Shader]: Begin creating shader ["//shader_name//"]."

    ! Program creation.
    program_id = gl_create_program()

    if (.not. creation_succeeded(program_id)) then
      error stop "[Shader] Error: Failed to create program for shader ["//shader_name//"]."
    else
      print"(A)","[Shader]: Successfully created program for shader ["//shader_name//"] successfully at ID ["//int_to_string(program_id)//"]."
    end if

    ! Vertex shader compilation.
    vertex_id = gl_create_shader(GL_VERTEX_SHADER)
    if (.not. creation_succeeded(vertex_id)) then
      error stop "[Shader] Error: Failed to create vertex for shader ["//shader_name//"]."
    else
      print"(A)","[Shader]: Successfully created vertex for shader ["//shader_name//"] successfully at ID ["//int_to_string(vertex_id)//"]."
    end if

    call gl_shader_source(vertex_id, vertex_code_file_path)
    call gl_compile_shader(vertex_id)

    if (.not. shader_compilation_succeeded(vertex_id)) then
      error stop "[Shader] Error: Failed to compile vertex for shader ["//shader_name//"]."
    else
      print"(A)","[Shader]: Successfully compiled vertex for shader ["//shader_name//"]."
    end if

    ! Fragment shader compilation.
    fragment_id = gl_create_shader(GL_FRAGMENT_SHADER)
    if (.not. creation_succeeded(fragment_id)) then
      error stop "[Shader] Error: Failed to create fragment for shader ["//shader_name//"]."
    else
      print"(A)","[Shader]: Successfully created fragment for shader ["//shader_name//"] successfully at ID ["//int_to_string(fragment_id)//"]."
    end if

    call gl_shader_source(fragment_id, fragment_code_file_path)
    call gl_compile_shader(fragment_id)

    if (.not. shader_compilation_succeeded(fragment_id)) then
      error stop "[Shader] Error: Failed to compile fragment for shader ["//shader_name//"]."
    else
      print"(A)","[Shader]: Successfully compiled fragment for shader ["//shader_name//"]."
    end if

    ! Attach and link.
    call gl_attach_shader(program_id, vertex_id)
    call gl_attach_shader(program_id, fragment_id)
    call gl_link_program(program_id)

    ! We check that this think linked.
    if (gl_get_program_iv(program_id, GL_LINK_STATUS) == GL_FALSE) then
      error stop "[Shader] Error: Failed to link shader ["//shader_name//"]."
    else
      print"(A)","[Shader]: Successfully linked shader ["//shader_name//"]."
    end if

    ! Now remove the shaders objects because they're already compiled into the program.
    ! We're also going to verify that they're deleted.
    call gl_detach_shader(program_id, vertex_id)
    call gl_delete_shader(vertex_id)
    if (gl_is_shader(vertex_id)) then
      error stop "[Shader] Error: Failed to delete the vertex shader object."
    end if

    call gl_detach_shader(program_id, fragment_id)
    call gl_delete_shader(fragment_id)
    if (gl_is_shader(fragment_id)) then
      error stop "[Shader] Error: Failed to delete the fragment shader object."
    end if

    UNIFORM_CAMERA_MATRIX = gl_get_uniform_location(program_id, "camera_matrix"//achar(0))
    UNIFORM_OBJECT_MATRIX = gl_get_uniform_location(program_id, "object_matrix"//achar(0))

    print"(A)","[Shader]: Shader ["//shader_name//"] created successfully."

    ! Store it in the hash table for later use.
    call shader_database%set(shader_name, program_id)
  end subroutine shader_create


  !* Get a shader from the hash table.
  !* The shader is a clone. To update, set_shader().
  function get_shader(shader_name, gotten_program) result(exists)
    implicit none

    character(len = *, kind = c_char), intent(in) :: shader_name
    integer(c_int), intent(inout) :: gotten_program
    logical(c_bool) :: exists
    type(c_ptr) :: raw_c_ptr
    integer(c_int), pointer :: program_id

    exists = .false.

    if (.not. shader_database%get(shader_name, raw_c_ptr)) then
      print"(A)","[Shader] Warning: ["//shader_name//"] does not exist."
      return
    end if

    call c_f_pointer(raw_c_ptr, program_id)
    gotten_program = program_id
    exists = .true.
  end function get_shader


  !* Check if a shader exists in the database.
  logical function shader_exists(shader_name) result(exists)
    use :: string_f90
    implicit none

    character(len = *, kind = c_char), intent(in) :: shader_name

    ! All we must do is check the shader result and return the existence in the result.
    exists = shader_database%has_key(shader_name)
  end function shader_exists


  !* Start up a shader program.
  subroutine shader_start(shader_name)
    implicit none

    character(len = *, kind = c_char), intent(in) :: shader_name
    integer(c_int) :: current_program_id

    ! If the shader does not exist, bail out.
    if (.not. get_shader(shader_name, current_program_id)) then
      error stop "[Shader] Error: Cannot start shader ["//shader_name//"], it does not exist."
    end if

    call gl_use_program(current_program_id)
  end subroutine shader_start


  !* Completely wipe out all existing shaders. This might be slow.
  subroutine shader_destroy_database()
    implicit none

    call shader_database%destroy()
  end subroutine shader_destroy_database


  subroutine gc_shader_database(raw_c_ptr)
    use :: string_f90
    implicit none

    type(c_ptr), intent(in), value :: raw_c_ptr
    integer(c_int), pointer :: shader_id

    call c_f_pointer(raw_c_ptr, shader_id)

    ! Unbind from the currently used shader.
    call gl_use_program(0)

    call gl_delete_program(shader_id)

    if (gl_is_program(shader_id)) then
      error stop "[Shader] Error: Failed to delete program for shader ["//int_to_string(shader_id)//"]"
    end if
  end subroutine gc_shader_database


end module shader
