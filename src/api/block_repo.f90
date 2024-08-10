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
    character(len = :, kind = c_char), allocatable :: test_string

    test = 0.0

    ! Memory layout: (Stack grows down.)
    ! -1 - blocks = {}
    ! then moves to:
    ! -2 - blocks = {}
    ! -1 - function pointers.
    ! Then we pop -1 off the stack.


    call lua_getglobal(state, "blocks")

    if (.not. lua_istable(state, -1)) then
      print"(A)", "[Blocks Repo] Error: Can't initialize function pointers. [blocks] table is missing!"
      success = .false.
      return
    end if

    ! Swap the declaration with the actual fortran function.
    call luajit_swap_table_function(state, "register_block", c_funloc(register_block))

    ! Let's try to call it.
    call lua_pushstring(state, "register_block")
    ! Table is now at -2.
    call lua_gettable(state, -2)

    if (lua_pcall(state, 0, 0, 0) == LUA_OK) then
      print*,"it worked"
    end if

    ! We push our variable into the stack like a caveman, lol.
    ! call lua_pushstring(state, "test")
    ! call lua_pushstring(state, "I am a test!")
    ! ! Then this is called as: -3 = blocks[test]
    ! call lua_settable(state, -3)

    ! ! Remove the values from the stack.
    ! call lua_pop(state, -2)

    ! if (.not. lua_istable(state, -1)) then
    !   print"(A)", "this is no longer a table"
    ! end if


    ! ! Table is back at -1.
    ! call lua_pushstring(state, "test")

    ! ! Then this is called as: -2 = blocks[test]
    ! call lua_gettable(state, -2)

    ! if (.not. lua_isstring(state, -1)) then
    !   print*,"That's not a string"
    ! end if


    ! test_string = lua_tostring(state, -1)

    ! print"(A)","["//test_string//"]"






    ! print*,test





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

    print"(A)","Hello, I am register_block!"
  end subroutine register_block


end module block_repo
