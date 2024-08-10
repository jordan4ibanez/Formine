module api
  use :: luajit
  use, intrinsic :: iso_c_binding
  implicit none


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

    success = luajit_run_file(lua_state, "./api/inift.lua")

    ! Someone removed the api init file, eh?
    if (.not. success) then
      return
    end if


  end function api_initialize


  subroutine api_destroy()
    implicit none

    call luajit_destroy(lua_state)
  end subroutine api_destroy


end module api
