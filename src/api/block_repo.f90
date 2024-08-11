module block_repo
  use :: luajit
  use, intrinsic :: iso_c_binding
  implicit none


  private


  public :: block_repo_deploy_lua_api
  public :: register_block


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
    implicit none

    type(c_ptr), intent(in), value :: state

    call lua_pushstring(state, "hooplah")
    if (lua_error(state) == LUA_OK) then
      return
    end if

    ! error stop "[Block Repo] Error: Can't initialize function pointers. [blocks] table is missing!"

    if (.not. lua_istable(state, -1)) then
      error stop "[Block Repo] Error: Can't initialize function pointers. [blocks] table is missing!"
    end if


  end subroutine register_block


end module block_repo
