module block_repo
  use :: luajit
  use :: string
  use :: fhash, only: fhash_tbl_t, key => fhash_key
  use, intrinsic :: iso_c_binding
  implicit none


  private


  public :: block_repo_deploy_lua_api
  public :: register_block


  !* Bake the module name into the executable.

  character(len = 12, kind =c_char), parameter :: module_name = "[Block Repo]"


  !* Block draw types.

  ! This is a simple range check that can be used to verify input draw_type.
  ! If new draw_types are added, syncronize the max.
  integer(c_int), parameter :: DRAW_TYPE_MIN = 0
  integer(c_int), parameter :: DRAW_TYPE_MAX = 1

  integer(c_int), parameter :: DRAW_TYPE_AIR = 0
  integer(c_int), parameter :: DRAW_TYPE_NORMAL = 1


  !* Block definition.

  type block_definition
    character(len = :, kind = c_char), allocatable :: name
    character(len = :, kind = c_char), allocatable :: description
    type(heap_string), dimension(6) :: textures
    integer(c_int) :: draw_type
  end type block_definition


  !* Block database.

  type(fhash_tbl_t) :: block_database


contains


  !* This hooks the required fortran functions into the LuaJIT "blocks" table.
  subroutine block_repo_deploy_lua_api(state)
    implicit none

    type(c_ptr), intent(in), value :: state


    ! Memory layout: (Stack grows down.)
    ! -1 - blocks = {}
    ! then moves to:
    ! -3 - blocks = {}
    ! -2 - table key string.
    ! -1 - function pointers.
    ! Then we pop -2 and -1 off the stack, shifting blocks back to -1.


    call lua_getglobal(state, "block")

    if (.not. lua_istable(state, -1)) then
      error stop "[Block Repo] Error: Can't initialize function pointers. [blocks] table is missing!"
    end if

    ! Swap the declaration with the actual fortran function.
    call luajit_swap_table_function(state, "register", c_funloc(register_block))




    ! Now clear the stack. We're done with the block LuaJIT table.
    call lua_pop(state, lua_gettop(state))
  end subroutine block_repo_deploy_lua_api


  !* This allows you to register a block into the engine from LuaJIT.
  !* See the LuaJIT API [./api/init.lua] for the layout of block_definition.
  subroutine register_block(state)
    use :: string
    use :: array, only: string_array
    implicit none

    type(c_ptr), intent(in), value :: state
    ! We're going to be using the status quite a lot.
    integer(c_int) :: status
    ! block_definition fields.
    type(heap_string) :: name, description
    type(string_array) :: textures
    integer(c_int) :: draw_type
    !* The pointer where we will store the block definiton.
    !* We will only allocate this after a successful data query from LuaJIT.
    type(block_definition), pointer :: definition


    status = LUAJIT_GET_OK

    ! Enforce the first and only argument to be a table.
    if (.not. lua_istable(state, -1)) then
      call luajit_error_stop(state, module_name//" Error: Cannot register block. Not a table.")
    end if

    ! Name is required.
    call luajit_table_get_key_required(state, module_name, "definition", "name", name, "string")

    ! Description is required.
    call luajit_table_get_key_required(state, module_name, "definition", "description", description, "string")

    ! Now we need to get the table which contains the textures.
    call luajit_put_table_in_table_on_stack_required(state, module_name, "definition", "textures", "Array<string>")

    associate(value => luajit_get_generic(state, -1, textures))
      if (value /= LUAJIT_GET_OK) then
        if (value == LUAJIT_GET_MISSING) then
          call luajit_error_stop(state, module_name//" error: Table [definition] key table [textures] is missing.")
        else
          call luajit_error_stop(state, module_name//" error: Table [definition] key table [textures] has a non-string element.")
        end if
      end if
    end associate

    ! Now we get rid of the string table.
    call lua_pop(state, 1)

    ! We're back into the block_definition table.

    ! draw_type is required. This will auto push and pop the target table so
    ! we're still at the definition table being at -1.
    call luajit_table_get_key_required(state, module_name, "definition", "draw_type", draw_type, "draw_type")


    !* todo: can add in more definition components here. :)


    ! Clean up the stack. We are done with the LuaJIT stack.
    !? The definition table has now disappeared.
    call lua_pop(state, lua_gettop(state))


    ! We have completed a successful query of the definition table from LuaJIT.
    ! Put all the data into the fortran database.
    allocate(definition)


    definition%name = name%get()
    definition%description = description%get()
    definition%textures = textures%data
    definition%draw_type = draw_type

    print"(A)", module_name//": Current Block definition:"
    print"(A)", "Name: "//definition%name
    print"(A)", "Description: "//definition%description
    print*, "Textures: [",definition%textures,"]"
    print"(A)", "draw_type: "//int_to_string(definition%draw_type)

    ! // TODO: Collect this data into a database.
  end subroutine register_block


end module block_repo
