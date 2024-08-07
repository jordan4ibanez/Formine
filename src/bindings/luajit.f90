module luajit
  use, intrinsic :: iso_c_binding
  implicit none


  private

  !* Why yes, I did have to read the way too much documentation to do this.
  ! Reference: https://lucasklassmann.com/blog/2019-02-02-embedding-lua-in-c/

  public :: luajit_initialize
  public :: luajit_destroy


  type(c_ptr) :: lua_state





  interface


    function lual_newstate() result(new_state) bind(c, name = "luaL_newstate")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr) :: new_state
    end function lual_newstate


    !* This makes the LuaJIT standard library available.
    subroutine lual_openlibs(state) bind(c, name = "luaL_openlibs")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: state
    end subroutine lual_openlibs


    subroutine lua_close(state) bind(c, name = "lua_close")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: state
    end subroutine lua_close


  end interface


contains


  !* Create the actual LuaJIT state that we will use.
  subroutine luajit_initialize()
    implicit none

    lua_state = lual_newstate()

    if (.not. c_associated(lua_state)) then
      error stop "[LuaJIT] Error: Failed to initialize."
    end if

    ! Make the entire standard library available.
    !! Is this safe for the end user when using external mods? HELL NO.
    call lual_openlibs(lua_state)
  end subroutine luajit_initialize


  !* Clean up the memory used by LuaJIT and destroy it.
  subroutine luajit_destroy()
    implicit none

    call lua_close(lua_state)
  end subroutine luajit_destroy


end module luajit
