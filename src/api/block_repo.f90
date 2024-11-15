module block_repo
  use :: luajit
  use :: string_f90
  use :: hashmap_str
  use :: vector
  use, intrinsic :: iso_c_binding
  implicit none


  private


  !* Block database.
  !*
  !* Since this is attempted to utilize the CPU cache to the extreme,
  !* we will have some ground rules laid out.
  !*
  !* The idea is: We want the memory to be contiguous.
  !*
  !* It will be extremely unsafe if we do not follow these rules.
  !*
  !! Ground rules:
  !*
  !* The array will live in the heap as an allocated smart pointer.
  !*
  !* Block definitions will be created as the game starts up.
  !*
  !* Block will not be deleted during the game runtime.
  !*
  !* The string database will simply point to an index in the array via a raw pointer.
  !* This is here for when we need to access into the array.
  !*
  !* LuaJIT will never have access to the direct block_definition pointer.
  !*
  !* LuaJIT shall have it's own copy of the database which will be immutable with metatables.
  !*
  !* No block shall share an ID. The history of the block IDs will be held in the world database. (when that is created)
  !*
  !* As new blocks are added in, they will incremement the available ID.
  !*

  public :: block_definition
  public :: initialize_block_repo_module
  public :: block_repo_get_id_from_name
  public :: block_repo_get_number_of_definitions
  public :: block_repo_get_definition_pointer_by_id
  public :: block_repo_deploy_lua_api
  public :: register_block
  public :: block_repo_destroy


  !* Bake the module name into the executable.

  character(len = 12, kind = c_char), parameter :: module_name = "[Block Repo]"


  !* Block draw types.

  ! This is a simple range check that can be used to verify input draw_type.
  ! If new draw_types are added, syncronize the max.
  integer(c_int), parameter :: DRAW_TYPE_MIN = 0
  integer(c_int), parameter :: DRAW_TYPE_MAX = 1

  integer(c_int), parameter :: DRAW_TYPE_AIR = 0
  integer(c_int), parameter :: DRAW_TYPE_NORMAL = 1


  !* Block definition container.

  type :: block_definition
    character(len = :, kind = c_char), pointer :: name => null()
    character(len = :, kind = c_char), pointer :: description => null()
    type(string_pointer), dimension(6) :: textures
    integer(c_int) :: draw_type = DRAW_TYPE_AIR
    integer(c_int) :: id = 0
  end type block_definition


  integer(c_int) :: current_id = 1
  integer(c_int) :: definition_array_length = 0

  ! Random access oriented.
  !* Type: block_definition
  type(hashmap_string_key) :: definition_database

  ! Linear access oriented.
  !* Type: block_definition
  !? NOTE: the definition_database is the one responsible for cleaning up the pointers.
  type(vec) :: definition_array

  ! Data oriented.
  ! integer(c_int) :: draw_type = DRAW_TYPE_AIR


contains

  subroutine initialize_block_repo_module()
    implicit none

    type(block_definition) :: blank

    !* Type: block_definition
    definition_database = new_hashmap_string_key(sizeof(blank), gc_definition_repo)

    !* Create the base smart pointer of the block array.
    definition_array = new_vec(sizeof(blank), 0_8)
  end subroutine initialize_block_repo_module


  !* Get the ID from the name.
  !* If it does not exist, it will return false.
  function block_repo_get_id_from_name(name, id) result(exists)
    implicit none

    character(len = *, kind = c_char), intent(in) :: name
    integer(c_int), intent(inout) :: id
    logical(c_bool) :: exists
    type(c_ptr) :: raw_c_ptr
    type(block_definition), pointer :: definition

    exists = .false.

    ! If it does not exist, simply return false.
    if (.not. definition_database%get(name, raw_c_ptr)) then
      return
    end if

    call c_f_pointer(raw_c_ptr, definition)

    id = definition%id

    exists = .true.
  end function block_repo_get_id_from_name


  !* Check how many block are registered.
  function block_repo_get_number_of_definitions() result(total)
    implicit none

    integer(c_int) :: total

    total = int(definition_array%size())
  end function block_repo_get_number_of_definitions


  !* Get a definition pointer by the ID.
  function block_repo_get_definition_pointer_by_id(id) result(definition_pointer)
    implicit none

    integer(c_int), intent(in), value :: id
    type(c_ptr) :: raw_c_ptr
    type(block_definition), pointer :: definition_pointer

    raw_c_ptr = definition_array%get(int(id, c_int64_t))

    call c_f_pointer(raw_c_ptr, definition_pointer)
  end function block_repo_get_definition_pointer_by_id


  !* This hooks the required fortran functions into the LuaJIT "block" table.
  subroutine block_repo_deploy_lua_api(state)
    implicit none

    type(c_ptr), intent(in), value :: state

    ! Memory layout: (Stack grows down.)
    ! -1 - block = {}
    ! then moves to:
    ! -3 - block = {}
    ! -2 - table key string.
    ! -1 - function pointers.
    ! Then we pop -2 and -1 off the stack, shifting block back to -1.


    call lua_getglobal(state, "block")

    if (.not. lua_istable(state, -1)) then
      error stop "[Block Repo] Error: Can't initialize function pointers. [block] table is missing!"
    end if

    ! Swap the declaration with the actual fortran function.
    call luajit_swap_table_function(state, "register", register_block)


    ! Now clear the stack. We're done with the block LuaJIT table.
    call lua_pop(state, lua_gettop(state))
  end subroutine block_repo_deploy_lua_api


  !* This allows you to register a block into the engine from LuaJIT.
  !* See the LuaJIT API [./api/init.lua] for the layout of block_definition.
  recursive function register_block(state) result(status) bind(c)
    use :: string_f90
    use :: array, only: string_array
    implicit none

    type(c_ptr), intent(in), value :: state
    ! We're going to be using the status quite a lot.
    integer(c_int) :: status, i
    ! block_definition fields.
    type(heap_string) :: name, description
    type(string_array) :: textures
    integer(c_int) :: draw_type
    !* The smart pointer where we will store the block definiton.
    !* We will only allocate this after a successful data query from LuaJIT.
    type(block_definition) :: new_definition

    status = LUAJIT_GET_OK

    ! Enforce the first and only argument to be a table.
    if (.not. lua_istable(state, -1)) then
      call luajit_error_stop(state, module_name//" Error: Cannot register block. Not a table.")
    end if

    ! Name is required.
    call luajit_table_get_key_required(state, module_name, "definition", "name", name, "string")

    !! If it is "air" silent abord.
    if (name%string == "air") then
      print"(A)", module_name//" warning: Please do not try to register air."
      call lua_pop(state, lua_gettop(state))
      return
    end if

    ! Description is required.
    call luajit_table_get_key_required(state, module_name, "definition", "description", description, "string")

    ! Now we need to get the table which contains the textures.
    call luajit_put_table_in_table_on_stack_required(state, module_name, "definition", "textures", "Array<string>")

    status = luajit_copy_string_array_from_table(state, textures)

    if (status /= LUAJIT_GET_OK) then
      if (status == LUAJIT_GET_MISSING) then
        call luajit_error_stop(state, module_name//" error: Table [definition] key table [textures] is missing.")
      else
        call luajit_error_stop(state, module_name//" error: Table [definition] key table [textures] has a non-string element.")
      end if
    end if


    ! Now we get rid of the string table.
    call lua_pop(state, 1)

    ! We're back into the block_definition table.

    ! draw_type is required. This will auto push and pop the target table so
    ! we're still at the definition table being at -1.
    call luajit_table_get_key_required(state, module_name, "definition", "draw_type", draw_type, "draw_type")


    !* todo: can add in more definition components here. :)


    ! Clean up the stack. We are done with the LuaJIT stack.
    !? The definition table has now disappeared.
    call lua_pop(state, lua_gettop(state))


    ! We have completed a successful query of the definition table from LuaJIT.
    ! Put all the data into the fortran database.


    call string_copy_pointer_to_pointer(name%get_pointer(), new_definition%name)

    call string_copy_pointer_to_pointer(description%get_pointer(), new_definition%description)

    do i = 1,6
      call string_copy_pointer_to_pointer(textures%data(i)%get_pointer(), new_definition%textures(i)%string)
    end do

    new_definition%draw_type = draw_type

    new_definition%id = current_id

    ! print"(A)", module_name//": Current Block definition:"
    ! print"(A)", "Name: "//definition_pointer%name
    ! print"(A)", "Description: "//definition_pointer%description
    ! print*, "Textures: [",definition_pointer%textures,"]"
    ! print"(A)", "draw_type: "//int_to_string(definition_pointer%draw_type)

    ! Copy the definition into the string based database.
    call definition_database%set(name%string, new_definition)

    call definition_array%push_back(new_definition)

    ! print"(A)","[Block Repo]: Registered ID ["//int_to_string(current_id)//"] to block ["//new_definition%name//"]"

    definition_array_length = definition_array_length + 1
    current_id = current_id + 1
  end function register_block


  subroutine block_repo_destroy()
    implicit none

    call definition_database%destroy()

    call definition_array%destroy()
  end subroutine block_repo_destroy


  subroutine gc_definition_repo(raw_c_ptr)
    implicit none

    type(c_ptr), intent(in), value :: raw_c_ptr
    type(block_definition), pointer :: definition_pointer
    integer(c_int) :: i

    call c_f_pointer(raw_c_ptr, definition_pointer)

    deallocate(definition_pointer%description)
    deallocate(definition_pointer%name)
    do i = 1,6
      if (associated(definition_pointer%textures(i)%string)) then
        deallocate(definition_pointer%textures(i)%string)
      end if
    end do
  end subroutine gc_definition_repo


end module block_repo
