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
      error stop "[Blocks Repo] Error: Can't initialize function pointers. [blocks] table is missing!"
    end if

    ! Swap the declaration with the actual fortran function.
    call luajit_swap_table_function(state, "register_block", c_funloc(register_block))


    ! Now clear the stack. We're done with the block LuaJIT table.
    call lua_pop(state, lua_gettop(state))
  end subroutine block_repo_deploy_lua_api


  !* Will intake the following components from LuaJIT:
  !* name, data_table
  subroutine register_block(state)
    implicit none

    type(c_ptr) :: state

    print"(A)","Hello, I am register_block!"
  end subroutine register_block


end module block_repo
