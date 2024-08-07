module luajit
  use, intrinsic :: iso_c_binding
  implicit none


  private

  !* Why yes, I did have to read the way too much documentation to do this.
  ! Reference: https://lucasklassmann.com/blog/2019-02-02-embedding-lua-in-c/

  public :: luajit_initialize


  type(c_ptr) :: lua_state





  interface


    function lual_newstate() result(new_state) bind(c, name = "luaL_newstate")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr) :: new_state
    end function lual_newstate


  end interface


contains


  !* This subroutine will create the actual lua state that we will use.
  subroutine luajit_initialize()
    implicit none

    lua_state = lual_newstate()

    if (.not. c_associated(lua_state)) then
      error stop "[LuaJIT] Error: Failed to initialize."
    end if
  end subroutine luajit_initialize


end module luajit
