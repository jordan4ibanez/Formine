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
  function shader_creation_succeeded(input, success_mutation) result(success)
    implicit none

    integer, intent(in), value :: input
    ! We want to mutate both these variables.
    logical :: success_mutation
    logical :: success

    success = input /= 0
    success_mutation = success
  end function shader_creation_succeeded

  !** This is a simple variation of shader_creation_succeeded with gl_check_error as our helper.
  !? Same docs as in shader_creation_success minus the input.
  function shader_compilation_succeeded(success_mutation) result(success)
    use opengl
    use string
    implicit none

    ! We want to mutate both these variables.
    logical :: success_mutation
    logical :: success

    !? 0 means OK in OpenGL.
    success = gl_get_error() == 0
    success_mutation = success
  end function shader_compilation_succeeded

  !** Create a named shader program from vertex and fragment code locations
  !! CAN FAIL. If something blows up or doesn't exist, this will halt the program. (required to render)
  function create_shader(shader_name, vertex_code_location, fragment_code_location) result(success)
    use opengl
    use string
    implicit none

    character(len = *), intent(in) :: shader_name
    character(len = *), intent(in) :: vertex_code_location
    character(len = *), intent(in) :: fragment_code_location
    logical :: success
    type(shader_program), allocatable :: program
    integer :: program_id
    integer :: vertex_shader_id
    integer :: fragment_shader_id

    success = .false.

    allocate(program)

    program_id = gl_create_program()
    if (.not. shader_creation_succeeded(program_id, success)) then
      print"(A)","[Shader] Error: Failed to create program for shader ["//shader_name//"]."
      return
    else
      print"(A)","[Shader]: Created program for shader ["//shader_name//"] successfully at ID ["//int_to_string(program_id)//"]."
    end if

    ! Vertex shader
    vertex_shader_id = gl_create_shader(GL_VERTEX_SHADER)
    if (.not. shader_creation_succeeded(vertex_shader_id, success)) then
      print"(A)","[Shader] Error: Failed to create vertex for shader ["//shader_name//"]."
      return
    else
      print"(A)","[Shader]: Created vertex for shader ["//shader_name//"] successfully at ID ["//int_to_string(vertex_shader_id)//"]."
    end if
    call gl_shader_source(vertex_shader_id, vertex_code_location)
    call gl_compile_shader(vertex_shader_id)
    if (.not. shader_compilation_succeeded(success)) then
      print"(A)","[Shader] Error: Failed to compile vertex for shader ["//shader_name//"]."
      return
    else
      print"(A)","[shader]: Compiled vertex for shader ["//shader_name//"]."
    end if


    ! Fragment shader
    fragment_shader_id = gl_create_shader(GL_FRAGMENT_SHADER)
    if (.not. shader_creation_succeeded(fragment_shader_id, success)) then
      print"(A)","[Shader] Error: Failed to create fragment for shader ["//shader_name//"]."
      return
    else
      print"(A)","[Shader]: Created fragment for shader ["//shader_name//"] successfully at ID ["//int_to_string(fragment_shader_id)//"]."
    end if
    call gl_shader_source(fragment_shader_id, fragment_code_location)
    call gl_compile_shader(fragment_shader_id)
    if (.not. shader_compilation_succeeded(success)) then
      print"(A)","[Shader] Error: Failed to compile fragment for shader ["//shader_name//"]."
      return
    else
      print"(A)","[shader]: Compiled fragment for shader ["//shader_name//"]."
    end if

    ! Now we attach and link.
    call gl_attach_shader(program_id, vertex_shader_id)
    call gl_attach_shader(program_id, fragment_shader_id)
    call gl_link_program(program_id)

    print"(A)","[Shader]: Created shader ["//shader_name//"] successfully."

  end function create_shader

end module shader
