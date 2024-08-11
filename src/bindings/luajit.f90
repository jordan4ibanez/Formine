module luajit
  use, intrinsic :: iso_c_binding
  implicit none


  private


  !* Why yes, I did have to read the way too much documentation to do this.
  ! References:
  ! https://lucasklassmann.com/blog/2019-02-02-embedding-lua-in-c/
  ! https://github.com/LuaJIT/LuaJIT/blob/v2.1/src/lua.h#L43
  ! https://github.com/LuaJIT/LuaJIT/blob/v2.1/src/luajit.c
  ! https://www.lua.org/manual/2.4/node18.html
  ! And I pretty much have to search through the LuaJIT source code for anything else.
  !
  !! For Fortran function see: https://www.lua.org/pil/26.1.html
  ! Lua Fortran functions:
  ! typedef int (*lua_CFunction) (lua_State *L);
  !
  !! For LuaJIT types see: https://github.com/LuaJIT/LuaJIT/blob/v2.1/src/luaconf.h
  !
  !? Going to need to learn how to handle userdata.
  !
  !? Going to need to learn how to handle metatables.
  !
  !* Note: The LuaJIT stack grows negative with return values and
  !* pushes things backwards as it grows.


  !* LuaJIT constants.

  public :: LUA_OK
  public :: LUA_YIELD
  public :: LUA_ERRRUN
  public :: LUA_ERRSYNTAX
  public :: LUA_ERRMEM
  public :: LUA_ERRERR
  public :: LUA_REGISTRYINDEX
  public :: LUA_ENVIRONINDEX
  public :: LUA_GLOBALSINDEX
  public :: LUA_TNONE
  public :: LUA_TNIL
  public :: LUA_TBOOLEAN
  public :: LUA_TLIGHTUSERDATA
  public :: LUA_TNUMBER
  public :: LUA_TSTRING
  public :: LUA_TTABLE
  public :: LUA_TFUNCTION
  public :: LUA_TUSERDATA
  public :: LUA_TTHREAD


  !* LuaJIT raw bindings.

  public :: luajit_closure
  public :: lua_gettop
  public :: lua_settop
  public :: lua_pushvalue
  public :: lua_remove
  public :: lua_insert
  public :: lua_replace
  public :: lua_checkstack
  public :: lua_isnumber
  public :: lua_isstring
  public :: lua_iscfunction
  public :: lua_type
  public :: lua_equal
  public :: lua_rawequal
  public :: lua_lessthan
  public :: lua_tonumber
  public :: lua_tointeger
  public :: lua_toboolean
  public :: lua_tolstring
  public :: lua_objlen
  public :: lua_tocfunction
  public :: lua_pushnil
  public :: lua_pushnumber
  public :: lua_pushinteger
  public :: lua_pushlstring
  public :: lua_pushstring
  public :: lua_pushcclosure
  public :: lua_pushboolean
  public :: lua_gettable
  public :: lua_getfield
  public :: lua_rawget
  public :: lua_rawgeti
  public :: lua_createtable
  public :: lua_settable
  public :: lua_rawset
  public :: lua_rawseti
  public :: lual_loadstring
  public :: lua_call
  public :: lua_pcall
  public :: lua_cpcall
  public :: lua_error
  public :: lua_next
  public :: lua_concat
  public :: lual_error


  !* Custom and macros.

  public :: lua_pop
  public :: lua_newtable
  public :: lua_register
  public :: lua_pushcfunction
  public :: lua_isfunction
  public :: lua_istable
  public :: lua_islightuserdata
  public :: lua_isboolean
  public :: lua_isthread
  public :: lua_isnone
  public :: lua_isnoneornil
  public :: lua_setglobal
  public :: lua_getglobal
  public :: lua_tostring
  public :: lua_typename
  public :: lua_setfield
  public :: luajit_error_stop
  public :: luajit_initialize
  public :: luajit_destroy
  public :: luajit_run_string
  public :: luajit_run_file
  public :: luajit_push_generic
  public :: luajit_swap_table_function
  public :: luajit_call_function
  public :: luajit_get_generic
  public :: luajit_table_get
  public :: luajit_require_table_field

  public :: LUA_RETURNINDEX
  public :: LUAJIT_GET_OK
  public :: LUAJIT_GET_MISSING
  public :: LUAJIT_GET_WRONG_TYPE

  !* LuaJIT constants.

  integer(c_int), parameter :: LUA_OK = 0
  integer(c_int), parameter :: LUA_YIELD = 1
  integer(c_int), parameter :: LUA_ERRRUN = 2
  integer(c_int), parameter :: LUA_ERRSYNTAX = 3
  integer(c_int), parameter :: LUA_ERRMEM = 4
  integer(c_int), parameter :: LUA_ERRERR = 5

  integer(c_int), parameter :: LUA_REGISTRYINDEX = (-10000)
  integer(c_int), parameter :: LUA_ENVIRONINDEX = (-10001)
  integer(c_int), parameter :: LUA_GLOBALSINDEX = (-10002)

  integer(c_int), parameter :: LUA_TNONE = (-1)

  integer(c_int), parameter :: LUA_TNIL = 0
  integer(c_int), parameter :: LUA_TBOOLEAN = 1
  integer(c_int), parameter :: LUA_TLIGHTUSERDATA = 2
  integer(c_int), parameter :: LUA_TNUMBER = 3
  integer(c_int), parameter :: LUA_TSTRING = 4
  integer(c_int), parameter :: LUA_TTABLE = 5
  integer(c_int), parameter :: LUA_TFUNCTION = 6
  integer(c_int), parameter :: LUA_TUSERDATA = 7
  integer(c_int), parameter :: LUA_TTHREAD = 8

  !* Custom constants

  !* This is a custom parameter to indicate we're getting the result from a function.
  integer(c_int), parameter :: LUA_RETURNINDEX = -1
  integer(c_int), parameter :: LUAJIT_GET_OK = 0
  integer(c_int), parameter :: LUAJIT_GET_MISSING = 1
  integer(c_int), parameter :: LUAJIT_GET_WRONG_TYPE = 2


  !* A custom wrapper type to allow X amount of arguments to be associated
  !* With a fortran function.
  type luajit_closure
    type(c_funptr) :: pointer
    integer(c_int) :: argument_count
  end type luajit_closure


  interface

!? BEGIN RAW BINDINGS. =================================================================================


!* STATE MANIPULATION. =================================================================================


    function internal_lual_newstate() result(new_state) bind(c, name = "luaL_newstate")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr) :: new_state
    end function internal_lual_newstate


    !* This makes the LuaJIT standard library available.
    subroutine internal_lual_openlibs(state) bind(c, name = "luaL_openlibs")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: state
    end subroutine internal_lual_openlibs


    subroutine internal_lua_close(state) bind(c, name = "lua_close")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: state
    end subroutine internal_lua_close


!* STACK MANIPULATION. =================================================================================


    function lua_gettop(state) result(index) bind(c, name = "lua_gettop")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: state
      integer(c_int) :: index
    end function lua_gettop


    subroutine lua_settop(state, index) bind(c, name = "lua_settop")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: state
      integer(c_int), intent(in), value :: index
    end subroutine lua_settop


    subroutine lua_pushvalue(state, index) bind(c, name = "lua_pushvalue")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: state
      integer(c_int), intent(in), value :: index
    end subroutine lua_pushvalue


    subroutine lua_remove(state, index) bind(c, name = "lua_remove")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: state
      integer(c_int), intent(in), value :: index
    end subroutine lua_remove


    subroutine lua_insert(state, index) bind(c, name = "lua_insert")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: state
      integer(c_int), intent(in), value :: index
    end subroutine lua_insert


    subroutine lua_replace(state, index) bind(c, name = "lua_replace")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: state
      integer(c_int), intent(in), value :: index
    end subroutine lua_replace


    function lua_checkstack(state, extra_slots) result(can_grow) bind(c, name = "lua_checkstack")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: state
      integer(c_int), intent(in), value :: extra_slots
      logical(c_bool) :: can_grow
    end function lua_checkstack


!* ACCESS FUNCTIONS. =================================================================================


    function lua_isnumber(state, index) result(is_a_number) bind(c, name = "lua_isnumber")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: state
      integer(c_int), intent(in), value :: index
      logical(c_bool) :: is_a_number
    end function lua_isnumber


    function lua_isstring(state, index) result(is_a_string) bind(c, name = "lua_isstring")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: state
      integer(c_int), intent(in), value :: index
      logical(c_bool) :: is_a_string
    end function lua_isstring


    function lua_iscfunction(state, index) result(is_a_c_function) bind(c, name = "lua_iscfunction")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: state
      integer(c_int), intent(in), value :: index
      logical(c_bool) :: is_a_c_function
    end function lua_iscfunction


    function lua_type(state, index) result(type_index) bind(c, name = "lua_type")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: state
      integer(c_int), intent(in), value :: index
      integer(c_int) :: type_index
    end function lua_type


    function internal_lua_typename(state, lua_type_index) result(type_name_pointer) bind(c, name = "lua_typename")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: state
      integer(c_int), intent(in), value :: lua_type_index
      type(c_ptr) :: type_name_pointer
    end function internal_lua_typename


    function lua_equal(state, index_1, index_2) result(equality) bind(c, name = "lua_equal")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: state
      integer(c_int), intent(in), value :: index_1, index_2
      logical(c_bool) :: equality
    end function lua_equal


    function lua_rawequal(state, index_1, index_2) result(raw_equality) bind(c, name = "lua_rawequal")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: state
      integer(c_int), intent(in), value :: index_1, index_2
      logical(c_bool) :: raw_equality
    end function lua_rawequal


    function lua_lessthan(state, index_1, index_2) result(is_less_than) bind(c, name = "lua_lessthan")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: state
      integer(c_int), intent(in), value :: index_1, index_2
      logical(c_bool) :: is_less_than
    end function lua_lessthan


    function lua_tonumber(state, index) result(output_double) bind(c, name = "lua_tonumber")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: state
      integer(c_int), intent(in), value :: index
      real(c_double) :: output_double
    end function lua_tonumber


    function lua_tointeger(state, index) result(output_integer) bind(c, name = "lua_tointeger")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: state
      integer(c_int), intent(in), value :: index
      integer(c_ptrdiff_t) :: output_integer
    end function lua_tointeger


    function lua_toboolean(state, index) result(output_boolean) bind(c, name = "lua_toboolean")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: state
      integer(c_int), intent(in), value :: index
      logical(c_bool) :: output_boolean
    end function lua_toboolean


    function lua_tolstring(state, index, length_of_returning_string) result(new_string_pointer) bind(c, name = "lua_tolstring")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: state
      integer(c_int), intent(in), value :: index
      !? This was originally c_size_t, but I don't think strings are going to go past 2 billion characters.
      integer(c_int), intent(inout) :: length_of_returning_string
      type(c_ptr) :: new_string_pointer
    end function lua_tolstring


    function lua_objlen(state, index) result(output_length) bind(c, name = "lua_objlen")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: state
      integer(c_int), intent(in), value :: index
      integer(c_size_t) :: output_length
    end function lua_objlen


    function lua_tocfunction(state, index) result(output_function_pointer) bind(c, name = "lua_tocfunction")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: state
      integer(c_int), intent(in), value :: index
      type(c_funptr) :: output_function_pointer
    end function lua_tocfunction


!* PUSH FUNCTIONS (FORTRAN -> STACK) =================================================================================


    subroutine lua_pushnil(state) bind(c, name = "lua_pushnil")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: state
    end subroutine lua_pushnil


    subroutine lua_pushnumber(state, double_number) bind(c, name = "lua_pushnumber")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: state
      real(c_double), intent(in), value :: double_number
    end subroutine lua_pushnumber


    subroutine lua_pushinteger(state, long_number) bind(c, name = "lua_pushinteger")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: state
      integer(c_ptrdiff_t), intent(in), value :: long_number
    end subroutine lua_pushinteger


    subroutine lua_pushlstring(state, string, string_length) bind(c, name = "lua_pushlstring")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: state
      character(kind = c_char), intent(in) :: string
      integer(c_size_t), intent(in), value :: string_length
    end subroutine lua_pushlstring


    subroutine internal_lua_pushstring(state, string) bind(c, name = "lua_pushstring")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: state
      character(kind = c_char), intent(in) :: string
    end subroutine internal_lua_pushstring


    subroutine lua_pushcclosure(state, lua_c_function, index) bind(c, name = "lua_pushcclosure")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: state
      type(c_funptr), intent(in), value :: lua_c_function
      integer(c_int), intent(in), value :: index
    end subroutine lua_pushcclosure


    subroutine lua_pushboolean(state, boolean) bind(c, name = "lua_pushboolean")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: state
      logical(c_bool), intent(in), value :: boolean
    end subroutine lua_pushboolean


!* GET FUNCTIONS. =================================================================================


    subroutine lua_gettable(state, index) bind(c, name = "lua_gettable")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: state
      integer(c_int), intent(in), value :: index
    end subroutine lua_gettable


    subroutine lua_getfield(state, index, key_string) bind(c, name = "lua_getfield")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: state
      integer(c_int), intent(in), value :: index
      character(kind = c_char), intent(in) :: key_string
    end subroutine lua_getfield


    subroutine lua_rawget(state, index) bind(c, name = "lua_rawget")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: state
      integer(c_int), intent(in), value :: index
    end subroutine lua_rawget


    subroutine lua_rawgeti(state, index, number_of_elements) bind(c, name = "lua_rawgeti")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: state
      integer(c_int), intent(in), value :: index, number_of_elements
    end subroutine lua_rawgeti


    subroutine lua_createtable(state, index, preallocation_amount) bind(c, name = "lua_createtable")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: state
      integer(c_int), intent(in), value :: index, preallocation_amount
    end subroutine lua_createtable


!* SET FUNCTIONS. =================================================================================


    subroutine lua_settable(state, index) bind(c, name = "lua_settable")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: state
      integer(c_int), intent(in), value :: index
    end subroutine lua_settable


    subroutine internal_lua_setfield(state, index, key_string) bind(c, name = "lua_setfield")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: state
      integer(c_int), intent(in), value :: index
      ! Was: [const char *k]
      character(kind = c_char), intent(in) :: key_string
    end subroutine internal_lua_setfield


    subroutine lua_rawset(state, index) bind(c, name = "lua_rawset")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: state
      integer(c_int), intent(in), value :: index
    end subroutine lua_rawset


    subroutine lua_rawseti(state, index, number_of_elements) bind(c, name = "lua_rawseti")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: state
      integer(c_int), intent(in), value :: index, number_of_elements
    end subroutine lua_rawseti


!* LOAD AND CALL. =================================================================================


    function lual_loadstring(state, string) result(status) bind(c, name = "luaL_loadstring")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: state
      character(kind = c_char), intent(in) :: string
      integer(c_int) :: status
    end function lual_loadstring


    function lua_call(state, number_of_args, number_of_results) result(status) bind(c, name = "lua_call")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: state
      integer(c_int), intent(in), value :: number_of_args, number_of_results
      integer(c_int) :: status
    end function lua_call


    function lua_pcall(state, number_of_args, number_of_results, error_function) result(status) bind(c, name = "lua_pcall")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: state
      integer(c_int), intent(in), value :: number_of_args, number_of_results, error_function
      integer(c_int) :: status
    end function lua_pcall


    function lua_cpcall(state, func, unknown_data) result(status) bind(c, name = "lua_cpcall")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: state
      type(c_funptr), intent(in), value :: func
      type(c_ptr), intent(inout) :: unknown_data
      integer(c_int) :: status
    end function lua_cpcall


!* MISCELLANEOUS FUNCTIONS. =================================================================================


    function lua_error(state) result(status) bind(c, name = "lua_error")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: state
      integer(c_int) :: status
    end function lua_error


    function lua_next(state, index) result(status) bind(c, name = "lua_next")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: state
      integer(c_int), intent(in), value :: index
      integer(c_int) :: status
    end function lua_next


    function lua_concat(state, number_of_things_to_concat) result(status) bind(c, name = "lua_concat")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: state
      integer(c_int), intent(in), value :: number_of_things_to_concat
      integer(c_int) :: status
    end function lua_concat


    function internal_lual_error(state, format_string, string) result(status) bind(c, name = "luaL_error")
      use, intrinsic :: iso_c_binding
      implicit none

      !? This is written horribly wrong because I only use this to print errors.

      type(c_ptr), intent(in), value :: state
      character(kind = c_char), intent(in) :: format_string, string
      integer(c_int) :: status
    end function internal_lual_error


!? END RAW BINDINGS. =================================================================================


  end interface


contains


!? BEGIN TRANSLATED MACROS. =================================================================================


  !* Pop the stack. This was a macro in LuaJIT.
  subroutine lua_pop(state, index)
    implicit none

    type(c_ptr), intent(in), value :: state
    integer(c_int), intent(in), value :: index

    call lua_settop(state, -(index) - 1)
  end subroutine lua_pop


  !* Create a new blank table. This was a macro in LuaJIT.
  subroutine lua_newtable(state)
    implicit none

    type(c_ptr), intent(in), value :: state

    call lua_createtable(state, 0, 0)
  end subroutine lua_newtable


  !* Register a global function. This was a macro in LuaJIT.
  subroutine lua_register(state, function_name, function_pointer)
    implicit none

    type(c_ptr), intent(in), value :: state
    character(len = *, kind = c_char) :: function_name
    type(c_funptr), intent(in), value :: function_pointer

    call lua_pushcfunction(state, (function_pointer))
    call lua_setglobal(state, function_name)
  end subroutine lua_register


  !* Push a fortran function pointer into the stack. This was a macro in LuaJIT.
  subroutine lua_pushcfunction(state, function_pointer)
    implicit none

    type(c_ptr), intent(in), value :: state
    type(c_funptr), intent(in), value :: function_pointer

    call lua_pushcclosure(state, (function_pointer), 0)
  end subroutine lua_pushcfunction


  !* Get if a variable is a function. This was a macro in LuaJIT.
  function lua_isfunction(state, index) result(is_a_function)
    use, intrinsic :: iso_c_binding
    implicit none

    type(c_ptr), intent(in), value :: state
    integer(c_int), intent(in), value :: index
    logical(c_bool) :: is_a_function

    is_a_function = (lua_type(state, (index)) == LUA_TFUNCTION)
  end function lua_isfunction


  !* Get if a variable is a table. This was a macro in LuaJIT.
  function lua_istable(state, index) result(is_a_table)
    use, intrinsic :: iso_c_binding
    implicit none

    type(c_ptr), intent(in), value :: state
    integer(c_int), intent(in), value :: index
    logical(c_bool) :: is_a_table

    is_a_table = (lua_type(state, (index)) == LUA_TTABLE)
  end function lua_istable


  !* Get if a variable is userdata. This was a macro in LuaJIT.
  function lua_islightuserdata(state, index) result(is_userdata)
    use, intrinsic :: iso_c_binding
    implicit none

    type(c_ptr), intent(in), value :: state
    integer(c_int), intent(in), value :: index
    logical(c_bool) :: is_userdata

    is_userdata = (lua_type(state, (index)) == LUA_TLIGHTUSERDATA)
  end function lua_islightuserdata


  !* Get if a variable is a boolean. This was a macro in LuaJIT.
  function lua_isboolean(state, index) result(is_a_boolean)
    use, intrinsic :: iso_c_binding
    implicit none

    type(c_ptr), intent(in), value :: state
    integer(c_int), intent(in), value :: index
    logical(c_bool) :: is_a_boolean

    is_a_boolean = (lua_type(state, (index)) == LUA_TBOOLEAN)
  end function lua_isboolean


  !* Get if a variable is a thread. This was a macro in LuaJIT.
  function lua_isthread(state, index) result(is_a_thread)
    use, intrinsic :: iso_c_binding
    implicit none

    type(c_ptr), intent(in), value :: state
    integer(c_int), intent(in), value :: index
    logical(c_bool) :: is_a_thread

    is_a_thread = (lua_type(state, (index)) == LUA_TTHREAD)
  end function lua_isthread


  !* Get if a variable is none. This was a macro in LuaJIT.
  function lua_isnone(state, index) result(is_none)
    use, intrinsic :: iso_c_binding
    implicit none

    type(c_ptr), intent(in), value :: state
    integer(c_int), intent(in), value :: index
    logical(c_bool) :: is_none

    is_none = (lua_type(state, (index)) == LUA_TNONE)
  end function lua_isnone


  !* Get if a variable is none or nil. This was a macro in LuaJIT.
  function lua_isnoneornil(state, index) result(is_none_or_nil)
    use, intrinsic :: iso_c_binding
    implicit none

    type(c_ptr), intent(in), value :: state
    integer(c_int), intent(in), value :: index
    logical(c_bool) :: is_none_or_nil

    is_none_or_nil = (lua_type(state, (index)) <= 0)
  end function lua_isnoneornil


  !* Set a global. This was a macro in LuaJIT.
  subroutine lua_setglobal(state, global_name)
    use :: string, only: into_c_string
    implicit none

    type(c_ptr), intent(in), value :: state
    character(len = *, kind = c_char), intent(in) :: global_name
    character(len = :, kind = c_char), allocatable :: c_string

    c_string = into_c_string(global_name)

    call lua_setfield(state, LUA_GLOBALSINDEX, (c_string))
  end subroutine lua_setglobal


  !* Get a global. This was a macro in LuaJIT.
  subroutine lua_getglobal(state, global_name)
    use :: string, only: into_c_string
    implicit none

    type(c_ptr), intent(in), value :: state
    character(len = *, kind = c_char), intent(in) :: global_name
    character(len = :, kind = c_char), allocatable :: c_string

    c_string = into_c_string(global_name)

    call lua_getfield(state, LUA_GLOBALSINDEX, (c_string))
  end subroutine lua_getglobal


  !* Get a string from lua. This was a macro in LuaJIT.
  !* This has been reconfigured to work with Fortran.
  function lua_tostring(state, index) result(new_string)
    use :: string
    implicit none

    type(c_ptr), intent(in), value :: state
    integer(c_int), intent(in), value :: index
    character(len = :, kind = c_char), allocatable :: new_string
    integer(c_int) :: lua_string_length
    type(c_ptr) :: c_string_pointer

    c_string_pointer = lua_tolstring(state, index, lua_string_length)

    new_string = string_from_c(c_string_pointer, lua_string_length + 1)
    !? c_string_pointer is not done with malloc. No need to free. (tested)
  end function lua_tostring


!? END TRANSLATED MACROS. =================================================================================



!* The rest is totally custom. =================================================================================


  !! BEGIN DEBUGGING LUAJIT CLOSURE !!
  ! typedef int (*lua_CFunction) (lua_State *L);
  subroutine test_luajit_closure(state)
    implicit none

    type(c_ptr), intent(in), value :: state
    character(len = :, kind = c_char), allocatable :: testing

    !! NOTE: IF THE FUNCTION DOESN'T DO ANYTHING, IT IS OPTIMIZED OUT !!
    !! IT WILL SEGFAULT IF IT DOES NOT DO ANYTHING !!
    print*,"hello from fortran, passed into lua, called from lua, back into fortran"


    testing = lua_typename(state, 1)

    print*,testing
    print*,lua_isnumber(state, 1)

    testing = lua_typename(state, 2)

    print*,testing


  end subroutine test_luajit_closure
  !! END DEBUGGING LUAJIT CLOSURE !!


  function lual_error(state, error_string) result(status)
    use :: string, only: into_c_string
    implicit none

    type(c_ptr), intent(in), value :: state
    character(len = *, kind = c_char), intent(in) :: error_string
    integer(c_int) :: status

    status = internal_luaL_error(state, into_c_string("%s"), into_c_string(error_string))
  end function lual_error


  subroutine lua_pushstring(state, input_string)
    use :: string, only: into_c_string
    implicit none

    type(c_ptr), intent(in), value :: state
    character(len = *, kind = c_char), intent(in) :: input_string
    character(len = :, kind = c_char), allocatable :: c_string

    c_string = into_c_string(input_string)

    call internal_lua_pushstring(state, c_string)
  end subroutine lua_pushstring


  !* Get the type of a variable as a string.
  function lua_typename(state, index) result(type_name)
    use :: string, only: string_from_c
    implicit none

    type(c_ptr), intent(in), value :: state
    integer(c_int), intent(in), value :: index
    character(len = :, kind = c_char), allocatable :: type_name
    integer(c_int) :: type_index
    type(c_ptr) :: c_string_pointer

    type_index = lua_type(state, index)
    c_string_pointer = internal_lua_typename(state, type_index)
    type_name = string_from_c(c_string_pointer, 36)
  end function lua_typename


  !* Set a field. (variable)
  subroutine lua_setfield(state, index, key_string)
    use :: string, only: into_c_string
    implicit none

    type(c_ptr), intent(in), value :: state
    integer(c_int), intent(in), value :: index
    character(len = *, kind = c_char) :: key_string
    character(len = :, kind = c_char), allocatable :: c_string

    c_string = into_c_string(key_string)

    call internal_lua_setfield(state, index, c_string)
  end subroutine lua_setfield


  !* Just chuck an error into LuaJIT.
  !* This is built upon a bunch of things to make it easy as heck.
  !* This will also pull the stack pointer out of your function
  !* using a jump. So when this is hit, the function stops executing.
  subroutine luajit_error_stop(state, error_string)
    implicit none

    type(c_ptr), intent(in), value :: state
    character(len = *, kind = c_char) :: error_string

    ! If we fail to throw an error, what the heck.
    if (lual_error(state, error_string) /= LUA_OK) then
      error stop "LuaJIT: Error: Failed to throw error!"
    end if
  end subroutine luajit_error_stop


  !* Create the actual LuaJIT state that we will use.
  subroutine luajit_initialize(state)
    implicit none

    type(c_ptr), intent(inout) :: state

    !? This is written like this to protect myself, from myself.

    if (c_associated(state)) then
      error stop "[LuaJIT] Error: Tried to initialize LuaJIT when already initialized."
    end if

    state = internal_lual_newstate()

    if (.not. c_associated(state)) then
      error stop "[LuaJIT] Error: Failed to initialize."
    end if

    ! Make the entire standard library available.
    !! Is this safe for the end user when using external mods? HELL NO.
    call internal_lual_openlibs(state)
  end subroutine luajit_initialize


  !* Clean up the memory used by LuaJIT and destroy it.
  subroutine luajit_destroy(state)
    implicit none

    type(c_ptr), intent(inout) :: state

    !? This is written like this to protect myself, from myself.

    if (.not. c_associated(state)) then
      error stop "[LuaJIT] Error: Tried to destroy LuaJIT when not initialized."
    end if

    call internal_lua_close(state)

    ! Nullify. Allows re-initialization.
    state = c_null_ptr
  end subroutine luajit_destroy


  !* Run a LuaJIT string. Returns success.
  function luajit_run_string(state, string_to_run) result(success)
    use :: string
    use :: terminal
    implicit none

    type(c_ptr), intent(in), value :: state
    character(len = *), intent(in) :: string_to_run
    character(len = :, kind = c_char), allocatable :: c_string
    logical :: success

    success = .false.

    c_string = into_c_string(string_to_run)

    if (lual_loadstring(state, c_string) == LUA_OK) then
      if (lua_pcall(state, 0, 0, 0) == LUA_OK) then
        ! If code was executed successfully, we remove the code from the stack.
        call lua_pop(state, lua_gettop(state))
        success = .true.
      else
        print"(A)", colorize_rgb(achar(10)//"[LuaJIT] Error:"//achar(10)//lua_tostring(state, lua_gettop(state)), 255, 0, 0)
      end if
    else
      print"(A)", colorize_rgb(achar(10)//"[LuaJIT] Error:"//achar(10)//lua_tostring(state, lua_gettop(state)), 255, 0, 0)
    end if
  end function luajit_run_string


  !* Run a LuaJIT file. Returns success.
  function luajit_run_file(state, file_path) result(success)
    use :: string
    use :: files
    use :: terminal
    implicit none

    type(c_ptr), intent(in), value :: state
    character(len = *, kind = c_char), intent(in) :: file_path
    type(file_reader) :: reader
    character(len = :, kind = c_char), allocatable :: c_string
    logical :: success

    success = .false.

    call reader%read_file(file_path)

    if (.not. reader%exists) then
      print"(A)", "[LuaJIT] Error: Could not load file path ["//file_path//"]. Does not exist."
      return
    end if

    c_string = into_c_string(reader%file_string)

    if (lual_loadstring(state, c_string) == LUA_OK) then
      if (lua_pcall(state, 0, 0, 0) == LUA_OK) then
        ! If code was executed successfully, we remove the code from the stack.
        call lua_pop(state, lua_gettop(state))
        success = .true.
      else
        print"(A)", colorize_rgb(achar(10)//"[LuaJIT] Error: Error in file ["//file_path//"]"//achar(10)//lua_tostring(state, lua_gettop(state)), 255, 0, 0)
      end if
    else
      print"(A)", colorize_rgb(achar(10)//"[LuaJIT] Error: Error in file ["//file_path//"]"//achar(10)//lua_tostring(state, lua_gettop(state)), 255, 0, 0)
    end if
  end function luajit_run_file


  !* This function will attempt to push whatever variable type into the LuaJIT stack.
  subroutine luajit_push_generic(state, input)
    implicit none

    type(c_ptr), intent(in), value :: state
    class(*), intent(in) :: input

    select type (input)

      !* Integer.
     type is (integer(c_int))
      call lua_pushinteger(state, int(input, kind = c_int64_t))
      ! print*,"push integer cast to c_int64_t"
     type is (integer(c_int64_t))
      call lua_pushinteger(state, input)
      ! print*, "push c_int64_t"

      !* Floating point.
     type is (real(c_float))
      call lua_pushnumber(state, real(input, kind = c_double))
      ! print*, "push float cast to c_double"
     type is (real(c_double))
      call lua_pushnumber(state, input)
      ! print*, "push c_double"

      !* String.
     type is (character(len = *))
      !* It appears that LuaJIT will simply grab the length without a null terminator.
      call lua_pushlstring(state, input, int(len(input), kind = c_size_t))
      ! print*, "push string with length"

      !* Boolean.
     type is (logical)
      call lua_pushboolean(state, logical(input, kind = c_bool))
      ! print*, "push logical, convert to c_bool"
     type is (logical(c_bool))
      call lua_pushboolean(state, input)
      ! print*, "push c_bool"

      !? Now we get into the interesting part.
      !* Function pointer. Aka, "closure".
     type is (luajit_closure)
      call lua_pushcclosure(state, input%pointer, input%argument_count)
      ! print*, "push fortran lua c function"

      !* We did something very bad.
     class default
      ! print*, "uh oh"
    end select
  end subroutine luajit_push_generic


  !* Ultra generic LuaJIT function caller.
  !* Limited to 4 input variables.
  !* Limited to 1 output variables.
  !* This could be changed though.
  subroutine luajit_call_function(state, function_name, a, b, c, d, return_value)
    use :: terminal
    implicit none

    type(c_ptr), intent(in), value :: state
    character(len = *, kind = c_char), intent(in) :: function_name
    class(*), intent(in), optional :: a, b, c, d
    !? This is written like this to allow pure LuaJIT functions.
    class(*), intent(inout), optional :: return_value
    integer(c_int) :: return_value_count

    ! Load the function into the LuaJIT stack.
    call lua_getglobal(state, function_name)

    ! We must push 4 values, nil or not, into the LuaJIT stack.
    ! They are positional arguments.
    if (present(a)) then
      call luajit_push_generic(state, a)
    else
      call lua_pushnil(state)
    end if

    if (present(b)) then
      call luajit_push_generic(state, b)
    else
      call lua_pushnil(state)
    end if

    if (present(c)) then
      call luajit_push_generic(state, c)
    else
      call lua_pushnil(state)
    end if

    if (present(d)) then
      call luajit_push_generic(state, d)
    else
      call lua_pushnil(state)
    end if

    ! Now we're going to check if the return value is present.
    return_value_count = 0

    if (present(return_value)) then
      return_value_count = return_value_count + 1
    end if

    print*,return_value_count

    !* We need 4 arguments, even if they do not exist. They are nil.
    !* The LuaJIT stack will read out of it's buffer if this isn't 4.
    if (lua_pcall(state, 4, return_value_count, 0) == LUA_OK) then
      call lua_pop(state, lua_gettop(state))
    else
      print"(A)", colorize_rgb(achar(10)//"[LuaJIT] Error: Error running LuaJIT function ["//function_name//"]"//achar(10)//lua_tostring(state, lua_gettop(state)), 255, 0, 0)
    end if
  end subroutine luajit_call_function


  !* This is an extremely specific function for swapping table values
  !* in the API. It allows me to swap the declaration with a fortran
  !* function pointer.
  !* This is a very delicate function as well, must be handled with care.
  !* The table must be at stack level -1.
  subroutine luajit_swap_table_function(state, table_key, function_pointer)
    implicit none

    type(c_ptr), intent(in), value :: state
    character(len = *, kind = c_char), intent(in) :: table_key
    type(c_funptr), intent(in), value :: function_pointer

    ! Push our stack values. Everything will shift back.
    call lua_pushstring(state, table_key)
    call lua_pushcfunction(state, function_pointer)

    ! The table now resides in -3. Set it.
    call lua_settable(state, -3)

    ! Finally, remove the values from the stack.
    call lua_pop(state, -2)
  end subroutine luajit_swap_table_function


  !* This is simply a helper function for luajit_get_generic.
  subroutine internal_get_if_nil_or_wrong_type(state, index, status)
    implicit none

    type(c_ptr), intent(in), value :: state
    integer(c_int), intent(in), value :: index
    integer(c_int), intent(inout) :: status

    if (lua_isnoneornil(state, index)) then
      status = LUAJIT_GET_MISSING
    else
      status = LUAJIT_GET_WRONG_TYPE
    end if
  end subroutine internal_get_if_nil_or_wrong_type


  !* This subroutine will attempt to grab data from whatever index you give it.
  function luajit_get_generic(state, index, generic_data) result(status)
    use :: string
    implicit none

    type(c_ptr), intent(in), value :: state
    integer(c_int), intent(in), value :: index
    class(*), intent(inout) :: generic_data
    integer(c_int) :: status

    !* This is written a bit defensively to prevent problems.

    select type (generic_data)

      !* Integer.
     type is (integer(c_int))
      if (.not. lua_isnumber(state, index)) then
        call internal_get_if_nil_or_wrong_type(state, index, status)
        return
      end if
      generic_data = int(lua_tonumber(state, index), kind = c_int)
      print*,"get integer cast to c_int"
     type is (integer(c_int64_t))
      if (.not. lua_isnumber(state, index)) then
        call internal_get_if_nil_or_wrong_type(state, index, status)
        return
      end if
      generic_data = int(lua_tonumber(state, index), kind = c_int64_t)
      print*, "get c_int64_t"

      !* Floating point.
     type is (real(c_float))
      if (.not. lua_isnumber(state, index)) then
        call internal_get_if_nil_or_wrong_type(state, index, status)
        return
      end if
      generic_data = real(lua_tonumber(state, index), kind = c_int)
      print*, "get float cast to c_int"
     type is (real(c_double))
      if (.not. lua_isnumber(state, index)) then
        call internal_get_if_nil_or_wrong_type(state, index, status)
        return
      end if
      generic_data = lua_tonumber(state, index)
      print*, "get c_double"

      !* String. (Only heap string)
     type is (heap_string)
      if (.not. lua_isstring(state, index)) then
        call internal_get_if_nil_or_wrong_type(state, index, status)
        return
      end if
      generic_data = lua_tostring(state, index)
      print*, "get string into heap_string"
      !* If you try to use a regular allocatable string, it can cause
      !* horrible problems so I'm not going to allow that.
      !* Use a heap_string.
     type is (character(len = *))
      error stop "[LuaJIT] Error: Cannot process an non-heap_string."

      !* Boolean.
     type is (logical)
      if (.not. lua_isboolean(state, index)) then
        call internal_get_if_nil_or_wrong_type(state, index, status)
        return
      end if
      generic_data = lua_toboolean(state, index)
      print*, "get logical"
     type is (logical(c_bool))
      if (.not. lua_isboolean(state, index)) then
        call internal_get_if_nil_or_wrong_type(state, index, status)
        return
      end if
      generic_data = lua_toboolean(state, index)
      print*, "get c_bool, convert to c_bool"

      !? Now we get into the interesting part.
      !* Function pointer. Aka, "closure".
      !! I have no idea why I would use this but it's here in case
      !! I ever decide to use it.
      !  type is (luajit_closure)
      ! call lua_pushcclosure(state, input%pointer, input%argument_count)
      ! print*, "get fortran lua c function"

      !* We did something very bad.
     class default
      error stop "LuaJIT Error: Tried to get an unknown data type."
      ! print*, "uh oh"
    end select

    status = LUAJIT_GET_OK
  end function luajit_get_generic


  !* This function is simply a shorthand helper for a few
  !* function calls. It also clarifies what things are doing.
  !* Table must be in stack -1.
  function luajit_table_get(state, table_key, data_output) result(status)
    implicit none

    type(c_ptr), intent(in), value :: state
    character(len = *, kind = c_char), intent(in) :: table_key
    class(*), intent(inout) :: data_output
    integer(c_int) :: status

    !* Push "name" to -1.
    call lua_pushstring(state, table_key)
    !* Table is now at -2. Calling as table["name"].
    call lua_gettable(state, -2)

    !* Now the value is pushed into the stack at -1.
    status = luajit_get_generic(state, -1, data_output)

    !* And we can pop the getters and data output off the LuaJIT stack.
    call lua_pop(state, -2)
  end function luajit_table_get


  !* This will luajit_error_stop if a table is missing a required field.
  !* This is going to be repeated, quite a lot. So I'm making it a subroutine.
  subroutine luajit_require_table_field(state, module_name, table_name, field_name, output_status)
    implicit none

    type(c_ptr), intent(in), value :: state
    character(len = *, kind = c_char), intent(in) :: module_name, table_name, field_name
    integer(c_int), intent(in), value :: output_status

    ! If we enter into a none OK value, it either doesn't exist or we have the wrong type.
    if (output_status /= LUAJIT_GET_OK) then
      if (output_status == LUAJIT_GET_MISSING) then
        call luajit_error_stop(state, "["//module_name//"] Error: Table ["//table_name//"] is missing field ["//field_name//"].")
      else
        call luajit_error_stop(state, "["//module_name//"] Error: Table ["//table_name//"] field ["//field_name//"] has the wrong type.")
      end if
    end if
  end subroutine


end module luajit
