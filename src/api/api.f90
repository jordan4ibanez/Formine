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
  public :: api_run_file


  type(c_ptr) :: lua_state


contains


  !* Initialize the API.
  subroutine api_initialize()
    use :: directory
    implicit none

    type(directory_reader) :: testing


    call testing%read_directory("./")

    call luajit_initialize(lua_state)

    if (.not. luajit_run_file(lua_state, "./api/init.lua")) then
      ! Someone removed the api init file, eh?
      error stop "[API] Error: Failed to load the init file."
    end if

    !* Initialize LuaJIT compatible modules.
    call block_repo_deploy_lua_api(lua_state)
  end subroutine api_initialize


  !* Clean up the API data.
  subroutine api_destroy()
    implicit none

    call luajit_destroy(lua_state)
  end subroutine api_destroy


  !* Run a LuaJIT file.
  function api_run_file(file_path) result(success)
    implicit none

    character(len = *, kind = c_char), intent(in) :: file_path
    logical :: success

    success = luajit_run_file(lua_state, file_path)
  end function api_run_file


end module api
