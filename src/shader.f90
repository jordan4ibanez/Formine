module shader
  use fhash, only: fhash_tbl_t, key => fhash_key
  implicit none

  
  private


  type(fhash_tbl_t) :: shader_programs


  public ::create_shader


  type shader_result
    class(shader_program), allocatable :: program
    logical :: exists
  end type shader_result


  type shader_program
    character(len=:), allocatable :: shader_name
    integer :: program_id
    integer :: vertex_id
    integer :: fragment_id
  end type shader_program


contains


  !** This is a simple way to check if a shader is null. (0)
  !? Makes the code easier to read.
  !? This also is making it so the program that uses it can return the success and work logic on it at the same time.
  logical function creation_succeeded(input, root_success) result(success)
    use string
    use opengl
    implicit none

    integer, intent(in), value :: input
    logical :: root_success

    ! Check for 0. This means a failure.
    root_success = input /= GL_FALSE
    success = root_success
  end function creation_succeeded


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
  logical function create_shader(name, vertex_code_location, fragment_code_location) result(success)
    use opengl
    use string
    use, intrinsic :: iso_c_binding
    implicit none

    character(len = *), intent(in) :: name
    character(len = *), intent(in) :: vertex_code_location
    character(len = *), intent(in) :: fragment_code_location
    type(shader_program), allocatable :: shader

    logical :: contains_thing
    type(shader_result) :: result

    allocate(shader)

    shader%shader_name = name

    print"(A)","[Shader]: Begin creating shader ["//shader%shader_name//"]."

    success = .false.

    ! Program creation.
    shader%program_id = gl_create_program()
    if (.not. creation_succeeded(shader%program_id, success)) then
      print"(A)","[Shader] Error: Failed to create program for shader ["//shader%shader_name//"]."
      return
    else
      print"(A)","[Shader]: Successfully created program for shader ["//shader%shader_name//"] successfully at ID ["//int_to_string(shader%program_id)//"]."
    end if

    ! Vertex shader compilation.
    shader%vertex_id = gl_create_shader(GL_VERTEX_SHADER)
    if (.not. creation_succeeded(shader%vertex_id, success)) then
      print"(A)","[Shader] Error: Failed to create vertex for shader ["//shader%shader_name//"]."
      return
    else
      print"(A)","[Shader]: Successfully created vertex for shader ["//shader%shader_name//"] successfully at ID ["//int_to_string(shader%vertex_id)//"]."
    end if

    call gl_shader_source(shader%vertex_id, vertex_code_location)
    call gl_compile_shader(shader%vertex_id)

    if (.not. shader_compilation_succeeded(success, shader%vertex_id)) then
      print"(A)","[Shader] Error: Failed to compile vertex for shader ["//shader%shader_name//"]."
      return
    else
      print"(A)","[Shader]: Successfully compiled vertex for shader ["//shader%shader_name//"]."
    end if

    ! ! Fragment shader compilation.
    shader%fragment_id = gl_create_shader(GL_FRAGMENT_SHADER)
    if (.not. creation_succeeded(shader%fragment_id, success)) then
      print"(A)","[Shader] Error: Failed to create fragment for shader ["//shader%shader_name//"]."
      return
    else
      print"(A)","[Shader]: Successfully created fragment for shader ["//shader%shader_name//"] successfully at ID ["//int_to_string(shader%fragment_id)//"]."
    end if

    call gl_shader_source(shader%fragment_id, fragment_code_location)
    call gl_compile_shader(shader%fragment_id)

    if (.not. shader_compilation_succeeded(success, shader%fragment_id)) then
      print"(A)","[Shader] Error: Failed to compile fragment for shader ["//shader%shader_name//"]."
      return
    else
      print"(A)","[Shader]: Successfully compiled fragment for shader ["//shader%shader_name//"]."
    end if

    ! Attach and link.
    call gl_attach_shader(shader%program_id, shader%vertex_id)
    call gl_attach_shader(shader%program_id, shader%fragment_id)
    call gl_link_program(shader%program_id)

    ! Finally, we check that this think linked.
    if (gl_get_program_iv(shader%program_id, GL_LINK_STATUS) /= GL_TRUE) then
      print"(A)","[Shader] Error: Failed to link shader ["//shader%shader_name//"]."
      success = .false.
      return
    else
      print"(A)","[Shader]: Successfully linked shader ["//shader%shader_name//"]."
    end if

    ! Woooo!
    print"(A)","[Shader]: Shader ["//shader%shader_name//"] created successfully."

    ! Store it in the hash table for later use.
    call set_shader(name, shader)
  end function create_shader


  logical function shader_exists(shader_name) result(existence)
    use string
    implicit none

    character(len = *) :: shader_name
    integer :: status = 0
    class(*), allocatable :: generic

    ! First, attempt to get the shader program.
    call shader_programs%get_raw(key(shader_name), generic, stat = status)

    ! Next check if we have an error.
    existence = status == 0

    if (.not. existence) then
      return
    end if

    ! Finally, ensure that it's of type shader_program.
    if (existence) then
      select type(generic)
       type is (shader_program)
        existence = .true.
        ! print*,"shader_program: "//int_to_string(generic%fragment_id)
       class default
        existence = .false.
        ! print*,"[Shader] Error: ["//shader_name//"] is not a shader program."
      end select
    end if
  end function shader_exists


  subroutine set_shader(name, shader)
    implicit none

    character(len = *) :: name
    type(shader_program), allocatable :: shader

    call shader_programs%set(key(name), shader)
  end subroutine set_shader


  ! Get a shader from the hash table.
  !** The shader is a clone. To update, set_shader().
  type(shader_result) function get_shader(shader_name) result(program_result)
    implicit none

    character(len = *) :: shader_name
    class(*), allocatable :: generic
    integer :: status

    program_result%exists = .false.

    call shader_programs%get_raw(key(shader_name), generic, stat = status)

    if (status /= 0) then
      ! print"(A)","[Shader] Error: ["//shader_name//"] does not exist."
      return
    end if

    select type(generic)
     type is (shader_program)
      program_result%exists = .true.
      program_result%program = generic
     class default
      ! print"(A)","[Shader] Error: ["//shader_name//"] has the wrong type."
      return
    end select
  end function get_shader


end module shader
