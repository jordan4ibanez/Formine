module block_repo
  use :: luajit
  use, intrinsic :: iso_c_binding
  implicit none


  private


  public :: block_repo_deploy_lua_api
  public :: register_block

  ! Bake the module name into the executable.
  character(len = 12, kind =c_char), parameter :: module_name = "[Block Repo]"


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
    implicit none

    type(c_ptr), intent(in), value :: state
    ! We're going to be using the status quite a lot.
    integer(c_int) :: status
    ! block_definition fields.
    type(heap_string) :: name, description
    integer(c_int) :: draw_type

    status = LUAJIT_GET_OK

    ! Enforce the first and only argument to be a table.
    if (.not. lua_istable(state, -1)) then
      call luajit_error_stop(state, module_name//" Error: Cannot register block. Not a table.")
    end if

    ! Name is required.
    call luajit_table_get_required(state, module_name, "definition", "name", name, "string")

    ! Description is required.
    call luajit_table_get_required(state, module_name, "definition", "description", description, "string")



    print"(A)", module_name//": Current Block definition:"
    print"(A)", "Name: "//name%get()
    print"(A)", "Description: "//description%get()



    if (.not. lua_istable(state, -1)) then
      call luajit_error_stop(state, module_name//" Error: Cannot register block. Not a table.")
    end if



  end subroutine register_block


end module block_repo
