module api
  use :: luajit
  use, intrinsic :: iso_c_binding
  !* LuaJIT API compatiblemodules.
  use :: block_repo
  implicit none


  !*
  !* This LuaJIT API has been written with love and care. :)
  !*
  !* Everything you see get loaded in, you can follow.
  !* I will try to document it the best I can.
  !*
  !* I am using the tables as a "LuaJIT preprocessor".
  !* Therefore, they will use replace instead of insert
  !* in the api functions.
  !*


  private


  public :: api_initialize
  public :: api_destroy


  type(c_ptr) :: lua_state


contains


  function api_initialize() result(success)
    implicit none

    logical :: success

    success = .false.

    call luajit_initialize(lua_state)

    success = luajit_run_file(lua_state, "./api/init.lua")

    ! Someone removed the api init file, eh?
    if (.not. success) then
      return
    end if

    !* Initialize LuaJIT compatible modules.
    if (.not. block_repo_deploy_lua_api(lua_state)) then
      return
    end if
  end function api_initialize


  subroutine api_destroy()
    implicit none

    call luajit_destroy(lua_state)
  end subroutine api_destroy


end module api
