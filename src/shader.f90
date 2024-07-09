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
  logical function shader_creation_succeeded(input, success)
    use string
    implicit none

    integer, intent(in), value :: input
    ! We want to mutate and return this.
    logical :: success

    success = input /= 0
    shader_creation_succeeded = success
  end function shader_creation_succeeded

  !** This is a simple variation of shader_creation_succeeded with gl_check_error as our helper.
  !? Same docs as in shader_creation_success minus the input.
  logical function shader_compilation_succeeded(success)
    use opengl
    implicit none

    ! We want to mutate and return this.
    logical :: success

    !? 0 means OK in OpenGL.
    success = gl_get_error() == 0
    shader_compilation_succeeded = success
  end function shader_compilation_succeeded

  logical function attempt_shader_compile(success, shader_name, shader_id, shader_type_name, shader_code_location)
    use string
    use opengl
    implicit none

    ! We want this to return and mutate at the same time.
    logical, intent(inout) :: success
    character(len = *) :: shader_name
    integer, intent(in), value :: shader_id
    character(len = *), intent(in) :: shader_type_name
    character(len = *), intent(in) :: shader_code_location

    success = .false.

    if (.not. shader_creation_succeeded(shader_id, success)) then
      print"(A)","[Shader] Error: Failed to create "//shader_type_name//" for shader ["//shader_name//"]."
      return
    else
      print"(A)","[Shader]: Successfully created "//shader_type_name//" for shader ["//shader_name//"] successfully at ID ["//int_to_string(shader_id)//"]."
    end if
    call gl_shader_source(shader_id, shader_code_location)
    call gl_compile_shader(shader_id)
    if (.not. shader_compilation_succeeded(success)) then
      print"(A)","[Shader] Error: Failed to compile "//shader_type_name//" for shader ["//shader_name//"]."
      return
    else
      print"(A)","[shader]: Successfully compiled "//shader_type_name//" for shader ["//shader_name//"]."
    end if
  end function attempt_shader_compile

  !** Create a named shader program from vertex and fragment code locations
  !? Will return false if it fails, true if it succeeds.
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
    if (.not. attempt_shader_compile(success, shader_name, vertex_shader_id, "vertex", vertex_code_location)) then
      return
    end if

    ! Fragment shader compilation.
    fragment_shader_id = gl_create_shader(GL_FRAGMENT_SHADER)
    if (.not. attempt_shader_compile(success, shader_name, fragment_shader_id, "fragment", fragment_code_location)) then
      return
    end if


    ! Now we attach and link.
    call gl_attach_shader(program_id, vertex_shader_id)
    call gl_attach_shader(program_id, fragment_shader_id)
    call gl_link_program(program_id)
    !? I'm counting attaching and linking as part of the compilation.
    if (.not. shader_compilation_succeeded(success)) then
      print"(A)","[Shader] Error: Failed to link shader ["//shader_name//"]."
      return
    else
      print"(A)","[shader]: Successfully linked shader ["//shader_name//"]."
    end if

    print"(A)","[Shader]: Shader ["//shader_name//"] created successfully."

  end function create_shader

end module shader
