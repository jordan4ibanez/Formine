module shader
  implicit none

  private

  public ::create_shader

  type shader_program
    character(len=:), allocatable :: name
    integer :: program_id
    integer :: vertex_id
    integer :: fragment_id
  end type shader_program

contains

  !** This is a simple way to check if a shader is null. (0)
  !? Makes the code easier to read.
  !? This also is making it so the program that uses it can return the success and work logic on it at the same time.
  logical function shader_creation_succeeded(input, root_success) result(success)
    use string
    implicit none

    integer, intent(in), value :: input
    logical :: root_success

    root_success = input /= 0
    success = root_success
  end function shader_creation_succeeded

  !** This is a simple variation of shader_creation_succeeded with gl_check_error as our helper.
  !? Same docs as in shader_creation_success minus the input.
  logical function shader_compilation_succeeded(root_success, shader_id) result(success)
    use opengl
    implicit none

    logical :: root_success
    integer :: shader_id

    !? 0 means OK in OpenGL.
    root_success = gl_get_shader_iv(shader_id, GL_COMPILE_STATUS) == GL_TRUE
    success = root_success
  end function shader_compilation_succeeded


  !** Create a named shader program from vertex and fragment code locations
  !? Will return false if it fails, true if it succeeds.
  logical function create_shader(shader_name, vertex_code_location, fragment_code_location) result(success)
    use opengl
    use string
    use, intrinsic :: iso_c_binding
    implicit none

    character(len = *), intent(in) :: shader_name
    character(len = *), intent(in) :: vertex_code_location
    character(len = *), intent(in) :: fragment_code_location
    type(shader_program), allocatable :: program
    integer :: program_id
    integer :: vertex_shader_id
    integer :: fragment_shader_id

    print"(A)","[Shader]: Begin creating shader ["//shader_name//"]."

    success = .false.

    allocate(program)

    ! Program creation.
    program_id = gl_create_program()
    if (.not. shader_creation_succeeded(program_id, success)) then
      print"(A)","[Shader] Error: Failed to create program for shader ["//shader_name//"]."
      return
    else
      print"(A)","[Shader]: Successfully created program for shader ["//shader_name//"] successfully at ID ["//int_to_string(program_id)//"]."
    end if

    ! Vertex shader compilation.
    vertex_shader_id = gl_create_shader(GL_VERTEX_SHADER)
    if (.not. shader_creation_succeeded(vertex_shader_id, success)) then
      print"(A)","[Shader] Error: Failed to create vertex for shader ["//shader_name//"]."
      return
    else
      print"(A)","[Shader]: Successfully created vertex for shader ["//shader_name//"] successfully at ID ["//int_to_string(vertex_shader_id)//"]."
    end if

    call gl_shader_source(vertex_shader_id, vertex_code_location)
    call gl_compile_shader(vertex_shader_id)

    if (.not. shader_compilation_succeeded(success, vertex_shader_id)) then
      print"(A)","[Shader] Error: Failed to compile vertex for shader ["//shader_name//"]."
      return
    else
      print"(A)","[shader]: Successfully compiled vertex for shader ["//shader_name//"]."
    end if

    ! ! Fragment shader compilation.
    fragment_shader_id = gl_create_shader(GL_FRAGMENT_SHADER)
    if (.not. shader_creation_succeeded(fragment_shader_id, success)) then
      print"(A)","[Shader] Error: Failed to create fragment for shader ["//shader_name//"]."
      return
    else
      print"(A)","[Shader]: Successfully created fragment for shader ["//shader_name//"] successfully at ID ["//int_to_string(fragment_shader_id)//"]."
    end if

    call gl_shader_source(fragment_shader_id, fragment_code_location)
    call gl_compile_shader(fragment_shader_id)

    if (.not. shader_compilation_succeeded(success, fragment_shader_id)) then
      print"(A)","[Shader] Error: Failed to compile fragment for shader ["//shader_name//"]."
      return
    else
      print"(A)","[shader]: Successfully compiled fragment for shader ["//shader_name//"]."
    end if

    ! Attach and link.
    call gl_attach_shader(program_id, vertex_shader_id)
    call gl_attach_shader(program_id, fragment_shader_id)
    call gl_link_program(program_id)

    ! Now we check that this think linked.
    if (gl_get_program_iv(program_id, GL_LINK_STATUS) /= GL_TRUE) then
      print"(A)","[Shader] Error: Failed to link shader ["//shader_name//"]."
      success = .false.
      return
    else
      print"(A)","[shader]: Successfully linked shader ["//shader_name//"]."
    end if

    print"(A)","[Shader]: Shader ["//shader_name//"] created successfully."

  end function create_shader

end module shader
