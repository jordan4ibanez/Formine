module shader
  use :: fhash, only: fhash_tbl_t, key => fhash_key
  use, intrinsic :: iso_c_binding, only: c_int
  implicit none


  private


  public :: LAYOUT_POSITION
  public :: LAYOUT_TEXTURE_COORDINATE
  public :: LAYOUT_COLOR


  integer(c_int), parameter :: LAYOUT_POSITION = 0
  integer(c_int), parameter :: LAYOUT_TEXTURE_COORDINATE = 1
  integer(c_int), parameter :: LAYOUT_COLOR = 2


  public :: shader_create
  public :: shader_create_uniform_locations
  public :: shader_get_uniform
  public :: shader_start
  public :: shader_clear_database


  !* A shader object. This holds all required shader components to run a shader.
  type shader_program
    character(len=:), allocatable :: shader_name
    integer :: program_id
    !* Attribute locations.
    type(fhash_tbl_t) :: attributes
    logical :: attributes_created = .false.
    !* Uniform locations.
    type(fhash_tbl_t) :: uniforms
    logical :: uniforms_created = .false.
  end type shader_program


  type(fhash_tbl_t) :: shader_database


contains


  !* This is a simple way to check if a shader is null. (0)
  !? Makes the code easier to read.
  !? This also is making it so the program that uses it can return the success and work logic on it at the same time.
  logical function creation_succeeded(input) result(success)
    use :: opengl
    implicit none

    integer, intent(in), value :: input

    !? 0 means a failure in this context.
    success = input /= GL_FALSE
  end function creation_succeeded


  !* This is a simple variation of shader_creation_succeeded with gl_check_error as our helper.
  !? Same docs as in shader_creation_success minus the input.
  logical function shader_compilation_succeeded(shader_id) result(success)
    use :: opengl
    implicit none

    integer, intent(in), value :: shader_id

    !? 1 means OK in this context.
    success = gl_get_shader_iv(shader_id, GL_COMPILE_STATUS) == GL_TRUE
  end function shader_compilation_succeeded


  !* Create a named shader program from vertex and fragment code locations
  !? Will return false if it fails, true if it succeeds.
  subroutine shader_create(name, vertex_code_location, fragment_code_location)
    use :: opengl
    use :: string
    use, intrinsic :: iso_c_binding
    implicit none

    character(len = *), intent(in) :: name
    character(len = *), intent(in) :: vertex_code_location
    character(len = *), intent(in) :: fragment_code_location
    integer(c_int) :: vertex_id, fragment_id
    type(shader_program), pointer :: shader

    allocate(shader)

    shader%shader_name = name

    print"(A)","[Shader]: Begin creating shader ["//shader%shader_name//"]."

    ! Program creation.
    shader%program_id = gl_create_program()

    if (.not. creation_succeeded(shader%program_id)) then
      error stop "[Shader] Error: Failed to create program for shader ["//shader%shader_name//"]."
    else
      print"(A)","[Shader]: Successfully created program for shader ["//shader%shader_name//"] successfully at ID ["//int_to_string(shader%program_id)//"]."
    end if

    ! Vertex shader compilation.
    vertex_id = gl_create_shader(GL_VERTEX_SHADER)
    if (.not. creation_succeeded(vertex_id)) then
      error stop "[Shader] Error: Failed to create vertex for shader ["//shader%shader_name//"]."
    else
      print"(A)","[Shader]: Successfully created vertex for shader ["//shader%shader_name//"] successfully at ID ["//int_to_string(vertex_id)//"]."
    end if

    call gl_shader_source(vertex_id, vertex_code_location)
    call gl_compile_shader(vertex_id)

    if (.not. shader_compilation_succeeded(vertex_id)) then
      error stop "[Shader] Error: Failed to compile vertex for shader ["//shader%shader_name//"]."
    else
      print"(A)","[Shader]: Successfully compiled vertex for shader ["//shader%shader_name//"]."
    end if

    ! Fragment shader compilation.
    fragment_id = gl_create_shader(GL_FRAGMENT_SHADER)
    if (.not. creation_succeeded(fragment_id)) then
      error stop "[Shader] Error: Failed to create fragment for shader ["//shader%shader_name//"]."
    else
      print"(A)","[Shader]: Successfully created fragment for shader ["//shader%shader_name//"] successfully at ID ["//int_to_string(fragment_id)//"]."
    end if

    call gl_shader_source(fragment_id, fragment_code_location)
    call gl_compile_shader(fragment_id)

    if (.not. shader_compilation_succeeded(fragment_id)) then
      error stop "[Shader] Error: Failed to compile fragment for shader ["//shader%shader_name//"]."
    else
      print"(A)","[Shader]: Successfully compiled fragment for shader ["//shader%shader_name//"]."
    end if

    ! Attach and link.
    call gl_attach_shader(shader%program_id, vertex_id)
    call gl_attach_shader(shader%program_id, fragment_id)
    call gl_link_program(shader%program_id)

    ! We check that this think linked.
    if (gl_get_program_iv(shader%program_id, GL_LINK_STATUS) == GL_FALSE) then
      error stop "[Shader] Error: Failed to link shader ["//shader%shader_name//"]."
    else
      print"(A)","[Shader]: Successfully linked shader ["//shader%shader_name//"]."
    end if

    ! Now remove the shaders objects because they're already compiled into the program.
    ! We're also going to verify that they're deleted.
    call gl_detach_shader(shader%program_id, vertex_id)
    call gl_delete_shader(vertex_id)
    if (gl_is_shader(vertex_id)) then
      error stop "[Shader] Error: Failed to delete the vertex shader object."
    end if

    call gl_detach_shader(shader%program_id, fragment_id)
    call gl_delete_shader(fragment_id)
    if (gl_is_shader(fragment_id)) then
      error stop "[Shader] Error: Failed to delete the fragment shader object."
    end if

    ! Finally validate this whole thing.
    call gl_validate_program(shader%program_id)
    if (gl_get_program_iv(shader%program_id, GL_VALIDATE_STATUS) == GL_FALSE) then
      error stop "[Shader] Error: Failed to validate shader ["//shader%shader_name//"]."
    else
      print"(A)","[Shader]: Successfully validated shader ["//shader%shader_name//"]."
    end if

    ! Woooo!
    print"(A)","[Shader]: Shader ["//shader%shader_name//"] created successfully."

    ! Store it in the hash table for later use.
    call shader_database%set_ptr(key(name), shader)
  end subroutine shader_create


  !* Get a shader from the hash table.
  !* The shader is a clone. To update, set_shader().
  function get_shader(shader_name, exists) result(gotten_program)
    implicit none

    character(len = *), intent(in) :: shader_name
    logical, intent(inout) :: exists
    class(*), pointer :: generic
    integer :: status
    type(shader_program), pointer :: gotten_program

    exists = .false.

    call shader_database%get_raw_ptr(key(shader_name), generic, stat = status)

    if (status /= 0) then
      ! print"(A)","[Shader] Error: ["//shader_name//"] does not exist."
      return
    end if

    select type(generic)
     type is (shader_program)
      exists = .true.
      gotten_program => generic
     class default
      ! print"(A)","[Shader] Error: ["//shader_name//"] has the wrong type."
      return
    end select
  end function get_shader


  !* Check if a shader exists in the database.
  logical function shader_exists(shader_name) result(exists)
    use :: string
    implicit none

    character(len = *), intent(in) :: shader_name
    type(shader_program), pointer :: poller

    ! All we must do is check the shader result and return the existence in the result.
    poller => get_shader(shader_name, exists)
  end function shader_exists


  !* Create the database of uniform locations, inside the shader program.
  subroutine shader_create_uniform_locations(shader_name, uniform_array)
    use :: opengl
    use :: string
    implicit none

    character(len = *), intent(in) :: shader_name
    type(heap_string), dimension(:) :: uniform_array
    type(shader_program), pointer :: current_program
    character(len = :), allocatable :: temp_string
    logical :: exists
    integer :: i
    integer :: location

    current_program => get_shader(shader_name, exists)

    ! If a non-existent shader is gotten, bail out.
    if (.not. exists) then
      error stop "[Shader] Error: Shader ["//shader_name//"] does not exist. Cannot create uniform locations."
    end if

    ! If we already created the uniform locations, bail out.
    if (current_program%uniforms_created) then
      error stop "[Shader] Error: Tried to create uniform locations more than once in shader ["//shader_name//"]"
    end if

    ! Now attempt to get all the shader uniform locations.
    do i = 1,size(uniform_array)

      temp_string = uniform_array(i)%get()
      location = gl_get_uniform_location(current_program%program_id, into_c_string(temp_string))

      ! If location is -1 that's OpenGL saying it couldn't find it basically.
      if (location < 0) then
        error stop "[Shader] Error: Shader["//shader_name//"] uniform ["//temp_string//"] does not exist in the shader. Got: "//int_to_string(location)//"."
      else
        print"(A)","[Shader]: Shader ["//shader_name//"] uniform ["//temp_string//"] created at location ["//int_to_string(location)//"]."
        call current_program%uniforms%set(key(temp_string), location)
      end if
    end do
  end subroutine shader_create_uniform_locations


  !* Get the integral position of a shader uniform.
  integer function shader_get_uniform(shader_name, uniform_name) result(location)
    implicit none

    character(len = *) :: shader_name
    character(len = *) :: uniform_name
    type(shader_program), pointer :: current_program
    logical :: exists
    integer :: status

    current_program => get_shader(shader_name, exists)

    ! If the shader does not exist, bail out.
    if (.not. exists) then
      error stop "[Shader] Error: Shader ["//shader_name//"] does not exist. Cannot get uniform location of ["//uniform_name//"]."
    end if

    ! Now let's try to get it.
    call current_program%uniforms%get(key(uniform_name), location, stat=status)

    ! Uh oh.
    if (status /= 0) then
      error stop "[Shader] Error: Shader ["//shader_name//"] does not contain uniform ["//uniform_name//"]."
    end if
  end function shader_get_uniform


  !* Start up a shader program.
  subroutine shader_start(shader_name)
    use :: opengl
    implicit none

    character(len = *), intent(in) :: shader_name
    type(shader_program), pointer :: current_program
    logical :: exists

    current_program => get_shader(shader_name, exists)

    ! If the shader does not exist, bail out.
    if (.not. exists) then
      error stop "[Shader] Error: Cannot start shader ["//shader_name//"], it does not exist."
    end if

    call gl_use_program(current_program%program_id)
  end subroutine shader_start


  !* Completely wipe out all existing shaders. This might be slow.
  subroutine shader_clear_database()
    use :: fhash, only: fhash_iter_t, fhash_key_t
    use :: opengl
    use :: string
    use :: terminal
    implicit none

    type(heap_string), dimension(:), allocatable :: key_array
    type(fhash_iter_t) :: iterator
    class(fhash_key_t), allocatable :: generic_key
    class(*), allocatable :: generic_data
    integer :: i, remaining_size, status
    class(*), pointer :: generic_pointer

    ! Start with a size of 0.
    allocate(key_array(0))

    ! Create the iterator.
    iterator = fhash_iter_t(shader_database)

    ! Unbind from the currently used shader.
    call gl_use_program(0)

    ! Now we will collect the keys from the iterator.
    do while(iterator%next(generic_key, generic_data))
      ! We will delete the programs as we go.
      select type(generic_data)
       type is (shader_program)
        call gl_delete_program(generic_data%program_id)
        if (gl_is_program(generic_data%program_id)) then
          error stop "[Shader] Error: Failed to delete program for shader ["//generic_key%to_string()//"]"
        end if
       class default
        error stop "[Shader] Error: The wrong type was inserted for shader ["//generic_key%to_string()//"]"
      end select
      ! Appending. Allocatable will clean up the old data.
      key_array = [key_array, heap_string_array(generic_key%to_string())]
    end do

    ! Now clear the database out.
    do i = 1,size(key_array)

      ! Since we are manually managing the memory, we must manually clean up what we have used.
      call shader_database%get_raw_ptr(key(key_array(i)%get()), generic_pointer, stat = status)
      if (status /= 0) then
        print"(A)", colorize_rgb("[Shader] Error: Failed to deallocate shader program ["//key_array(i)%get()//"]", 255, 0, 0)
      else
        deallocate(generic_pointer)
      end if

      call shader_database%unset(key(key_array(i)%get()))
    end do

    !* We will always check that the remaining size is 0. This will protect us from random issues.
    call shader_database%stats(num_items = remaining_size)

    if (remaining_size /= 0) then
      print"(A)", colorize_rgb("[Shader] Error: Did not delete all shaders! Expected size: [0] | Actual: ["//int_to_string(remaining_size)//"]", 255, 0, 0)
    else
      print"(A)", "[Shader]: Successfully cleared the shader database."
    end if

  end subroutine shader_clear_database

end module shader
