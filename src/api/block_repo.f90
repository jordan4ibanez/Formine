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
    real(c_double) :: test

    test = 0.0

    ! Memory layout: (Stack grows down.)
    ! -1 - blocks = {}
    ! then moves to:
    ! -2 - blocks = {}
    ! -1 - function pointers.
    ! Then we pop -1 off the stack.


    call lua_getglobal(state, "blocks")

    if (.not. lua_istable(state, 1)) then
      print"(A)", "[Blocks Repo] Error: Can't initialize function pointers. [blocks] table is missing!"
      success = .false.
      return
    end if

    ! We push our variable into the stack like a caveman, lol.
    call lua_pushstring(state, "test")
    ! Then this is called as: -2 = blocks[test]
    call lua_gettable(state, -2)

    if (.not. lua_isnumber(state, -1)) then
      print"(A)", "[Blocks Repo] Error: [test] is not a number!"
      return
    end if

    ! Tada. 5.5 (at least during the test it was)
    test = lua_tonumber(state, -1)

    ! Now we can remove this.
    call lua_remove(state, -1)

    ! The table is now back in -1.



    print*,test





    ! if (.not. lua_isfunction(state, 2)) then
    !   print*,"uh oh"
    ! end if


    ! if(lua_pcall(state, 0, 0, 0) == LUA_OK) then
    !   print*," nice"
    ! end if


  end function block_repo_deploy_lua_api


  !* Will intake the following components from LuaJIT:
  !* name, data_table
  subroutine register_block(state)
    implicit none

    type(c_ptr) :: state

    print*,"hi"
  end subroutine register_block


end module block_repo
