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

  public :: luajit_closure
  public :: test_luajit_closure

  public :: luajit_initialize
  public :: luajit_destroy
  public :: luajit_run_string
  public :: luajit_run_file
  public :: luajit_call_function


  integer(c_int), parameter :: LUA_OK = 0
  integer(c_int), parameter :: LUA_YIELD = 1
  integer(c_int), parameter :: LUA_ERRRUN = 2
  integer(c_int), parameter :: LUA_ERRSYNTAX = 3
  integer(c_int), parameter :: LUA_ERRMEM = 4
  integer(c_int), parameter :: LUA_ERRERR = 5

  integer(c_int), parameter :: LUA_REGISTRYINDEX = (-10000)
  integer(c_int), parameter :: LUA_ENVIRONINDEX = (-10001)
  integer(c_int), parameter :: LUA_GLOBALSINDEX = (-10002)

  !* This is a custom parameter to indicate we're getting the result from a function.
  integer(c_int), parameter :: LUA_RETURNINDEX = -1


  type(c_ptr) :: lua_state

  !* A custom wrapper type to allow X amount of arguments to be associated
  !* With a fortran function.
  type luajit_closure
    type(c_funptr) :: pointer
    integer(c_int) :: argument_count
  end type luajit_closure


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


    function lua_tolstring(state, index, length_of_returning_string) result(new_string_pointer) bind(c, name = "lua_tolstring")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: state
      integer(c_int), intent(in), value :: index
      !? This was originally c_size_t, but I don't think strings are going to go past 2 billion characters.
      integer(c_int), intent(inout) :: length_of_returning_string
      type(c_ptr) :: new_string_pointer
    end function lua_tolstring


    subroutine lua_getfield(state, index, key_string) bind(c, name = "lua_getfield")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: state
      integer(c_int), intent(in), value :: index
      character(kind = c_char), intent(in) :: key_string
    end subroutine lua_getfield


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


    !* There are no other ways to push a string into LuaJIT at the moment.
    !* I am only binding this one because it is the safest.
    subroutine lua_pushlstring(state, string, string_length) bind(c, name = "lua_pushlstring")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: state
      character(kind = c_char), intent(in) :: string
      integer(c_size_t), intent(in), value :: string_length
    end subroutine lua_pushlstring


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


  end interface


contains

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


  !* Create the actual LuaJIT state that we will use.
  subroutine luajit_initialize()
    implicit none

    if (c_associated(lua_state)) then
      error stop "[LuaJIT] Error: Tried to initialize LuaJIT when already initialized."
    end if

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

    if (.not. c_associated(lua_state)) then
      error stop "[LuaJIT] Error: Tried to destroy LuaJIT when not initialized."
    end if

    call lua_close(lua_state)

    ! Nullify. Allows re-initialization.
    lua_state = c_null_ptr
  end subroutine luajit_destroy


  !* Pop the stack. This was a macro in LuaJIT.
  subroutine lua_pop(index)
    implicit none

    integer(c_int), intent(in), value :: index

    call lua_settop(lua_State, -(index) - 1)
  end subroutine lua_pop


  !* Get a string from lua. This was a macro in LuaJIT.
  !* This has been reconfigured to work with Fortran.
  function lua_tostring(index) result(new_string)
    use :: string
    implicit none

    integer(c_int), intent(in), value :: index
    character(len = :, kind = c_char), allocatable :: new_string
    integer(c_int) :: lua_string_length
    type(c_ptr) :: c_string_pointer

    c_string_pointer = lua_tolstring(lua_state, index, lua_string_length)

    new_string = string_from_c(c_string_pointer, lua_string_length + 1)
    !? c_string_pointer is not done with malloc. No need to free. (tested)
  end function lua_tostring


  !* Load a LuaJIT global into the stack.
  subroutine lua_getglobal(function_name)
    use :: string
    implicit none

    character(len = *, kind = c_char), intent(in) :: function_name
    character(len = :, kind = c_char), allocatable :: c_function_name

    c_function_name = into_c_string(function_name)

    call lua_getfield(lua_state, LUA_GLOBALSINDEX, c_function_name)
  end subroutine lua_getglobal


  !* Run a LuaJIT string. Returns success.
  function luajit_run_string(string_to_run) result(success)
    use :: string
    use :: terminal
    implicit none

    character(len = *), intent(in) :: string_to_run
    character(len = :, kind = c_char), allocatable :: c_string
    logical :: success

    success = .false.

    c_string = into_c_string(string_to_run)

    if (lual_loadstring(lua_state, c_string) == LUA_OK) then
      if (lua_pcall(lua_state, 0, 0, 0) == LUA_OK) then
        ! If code was executed successfully, we remove the code from the stack.
        call lua_pop(lua_gettop(lua_state))
        success = .true.
      else
        print"(A)", colorize_rgb(achar(10)//"[LuaJIT] Error:"//achar(10)//lua_tostring(lua_gettop(lua_state)), 255, 0, 0)
      end if
    else
      print"(A)", colorize_rgb(achar(10)//"[LuaJIT] Error:"//achar(10)//lua_tostring(lua_gettop(lua_state)), 255, 0, 0)
    end if
  end function luajit_run_string


  !* Run a LuaJIT file. Returns success.
  function luajit_run_file(file_path) result(success)
    use :: string
    use :: files
    use :: terminal
    implicit none

    character(len = *, kind = c_char), intent(in) :: file_path
    type(file_reader) :: reader
    character(len = :, kind = c_char), allocatable :: c_string
    logical :: success

    success = .false.

    call reader%read_file(file_path)

    if (.not. reader%exists) then
      print"(A)", "[LuaJIT] Error: Could not load file path ["//file_path//"]. Does not exist."
    end if

    c_string = into_c_string(reader%file_string)

    if (lual_loadstring(lua_state, c_string) == LUA_OK) then
      if (lua_pcall(lua_state, 0, 0, 0) == LUA_OK) then
        ! If code was executed successfully, we remove the code from the stack.
        call lua_pop(lua_gettop(lua_state))
        success = .true.
      else
        print"(A)", colorize_rgb(achar(10)//"[LuaJIT] Error: Error in file ["//file_path//"]"//achar(10)//lua_tostring(lua_gettop(lua_state)), 255, 0, 0)
      end if
    else
      print"(A)", colorize_rgb(achar(10)//"[LuaJIT] Error: Error in file ["//file_path//"]"//achar(10)//lua_tostring(lua_gettop(lua_state)), 255, 0, 0)
    end if
  end function luajit_run_file


  !* This function will attempt to push whatever variable type into the LuaJIT stack.
  subroutine luajit_push_generic(input)
    use :: terminal
    use :: string, only: into_c_string
    implicit none

    class(*), intent(in) :: input

    select type (input)

      !* Integer.
     type is (integer(c_int))
      call lua_pushinteger(lua_state, int(input, kind = c_int64_t))
      print*,"push integer cast to c_int64_t"
     type is (integer(c_int64_t))
      call lua_pushinteger(lua_state, input)
      print*, "push c_int64_t"

      !* Floating point.
     type is (real(c_float))
      call lua_pushnumber(lua_state, real(input, kind = c_double))
      print*, "push float cast to c_double"
     type is (real(c_double))
      call lua_pushnumber(lua_state, input)
      print*, "push c_double"

      !* String.
     type is (character(len = *))
      call lua_pushlstring(lua_state, into_c_string(input), int(len(input) + 1, kind = c_size_t))
      print*, "push string with length"

      !* Boolean.
     type is (logical)
      call lua_pushboolean(lua_state, logical(input, kind = c_bool))
      print*, "push logical, convert to c_bool"
     type is (logical(c_bool))
      call lua_pushboolean(lua_state, input)
      print*, "push c_bool"

      !? Now we get into the interesting part.
      !* Function pointer. Aka, "closure".
     type is (luajit_closure)
      call lua_pushcclosure(lua_state, input%pointer, input%argument_count)
      print*, "push fortran lua c function"

      !* We did something very bad.
     class default
      print*, "uh oh"
    end select
  end subroutine luajit_push_generic


  !* Ultra generic LuaJIT function caller.
  !* Limited to 4 input variables.
  !* Limited to 1 output variables.
  !* This could be changed though.
  subroutine luajit_call_function(function_name, a, b, c, d, return_value)
    use :: terminal
    implicit none

    character(len = *, kind = c_char), intent(in) :: function_name
    class(*), intent(in), optional :: a, b, c, d
    !? This is written like this to allow pure LuaJIT functions.
    class(*), intent(inout), optional :: return_value
    integer(c_int) :: return_value_count

    ! Load the function into the LuaJIT stack.
    call lua_getglobal(function_name)

    ! We must push 4 values, nil or not, into the LuaJIT stack.
    ! They are positional arguments.
    if (present(a)) then
      call luajit_push_generic(a)
    else
      call lua_pushnil(lua_state)
    end if

    if (present(b)) then
      call luajit_push_generic(b)
    else
      call lua_pushnil(lua_state)
    end if

    if (present(c)) then
      call luajit_push_generic(c)
    else
      call lua_pushnil(lua_state)
    end if

    if (present(d)) then
      call luajit_push_generic(d)
    else
      call lua_pushnil(lua_state)
    end if

    ! Now we're going to check if the return value is present.
    return_value_count = 0

    if (present(return_value)) then
      return_value_count = return_value_count + 1
    end if

    print*,return_value_count

    !* We need 4 arguments, even if they do not exist. They are nil.
    !* The LuaJIT stack will read out of it's buffer if this isn't 4.
    if (lua_pcall(lua_state, 4, return_value_count, 0) == LUA_OK) then
      call lua_pop(lua_gettop(lua_state))
    else
      print"(A)", colorize_rgb(achar(10)//"[LuaJIT] Error: Error running LuaJIT function ["//function_name//"]"//achar(10)//lua_tostring(lua_gettop(lua_state)), 255, 0, 0)
    end if
  end subroutine luajit_call_function


end module luajit
