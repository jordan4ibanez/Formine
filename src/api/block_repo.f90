module block_repo
  use :: luajit
  use, intrinsic :: iso_c_binding
  implicit none


  private

  public :: block_repo_deploy_lua_api
  public :: register_block


contains

  !* This hooks a bunch of functions into the LuaJIT "blocks" table.
  function block_repo_deploy_lua_api(state) result(success)
    implicit none

    type(c_ptr), intent(in), value :: state
    logical :: success

    ! Memory layout: (Stack grows down.)
    ! -1 - blocks = {}
    ! then moves to:
    ! -3 - blocks = {}
    ! -2 - table key string.
    ! -1 - function pointers.
    ! Then we pop -2 and -1 off the stack, shifting blocks back to -1.


    call lua_getglobal(state, "blocks")

    if (.not. lua_istable(state, -1)) then
      print"(A)", "[Blocks Repo] Error: Can't initialize function pointers. [blocks] table is missing!"
      success = .false.
      return
    end if

    ! Swap the declaration with the actual fortran function.
    call luajit_swap_table_function(state, "register_block", c_funloc(register_block))


    ! Now clear the stack.
    call lua_pop(state, lua_gettop(state))

    ! ! Let's try to call it.
    ! call lua_pushstring(state, "register_block")
    ! ! Table is now at -2.
    ! call lua_gettable(state, -2)

    ! if (lua_pcall(state, 0, 0, 0) == LUA_OK) then
    !   print*,"it worked"
    ! end if

    ! The pcall has removed the function off the stack.
    ! The table is now back at -1.
    ! if (.not. lua_istable(state, -1)) then
    !   print"(A)", "this is no longer a table"
    ! else
    !   print"(A)", "that's still a table"
    ! end if
  end function block_repo_deploy_lua_api


  !* Will intake the following components from LuaJIT:
  !* name, data_table
  subroutine register_block(state)
    implicit none

    type(c_ptr) :: state

    print"(A)","Hello, I am register_block!"
  end subroutine register_block


end module block_repo
