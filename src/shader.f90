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

  !** Create a named shader program from vertex and fragment code locations
  !! CAN FAIL. If something blows up or doesn't exist, this will halt the program. (required to render)
  subroutine create_shader(shader_name, vertex_code_location, fragment_code_location)
    use opengl
    use string
    implicit none

    character(len = *) :: shader_name
    character(len = *) :: vertex_code_location
    character(len = *) :: fragment_code_location
    logical :: success
    type(shader_program), allocatable :: program

    integer :: program_id
    integer :: vertex_shader_id
    integer :: fragment_shader_id

    allocate(program)

    !? Note: needs a 0 check.

    program_id = gl_create_program()
    print"(A)","Shader Program ID: "//int_to_string(program_id)

    ! Vertex shader
    vertex_shader_id = gl_create_shader(GL_VERTEX_SHADER)
    print"(A)","Vertex Shader ID: "//int_to_string(vertex_shader_id)
    call gl_shader_source(vertex_shader_id, vertex_code_location)
    call gl_compile_shader(vertex_shader_id)

    ! Fragment shader
    fragment_shader_id = gl_create_shader(GL_FRAGMENT_SHADER)
    print"(A)","Fragment Shader ID: "//int_to_string(fragment_shader_id)
    call gl_shader_source(fragment_shader_id, fragment_code_location)
    call gl_compile_shader(fragment_shader_id)

    ! Now we attach and link.
    call gl_attach_shader(program_id, vertex_shader_id)
    call gl_attach_shader(program_id, fragment_shader_id)
    call gl_link_program(program_id)

    print"(A)","[Shader]: Created shader ["//shader_name//"] successfully."

  end subroutine create_shader

end module shader
