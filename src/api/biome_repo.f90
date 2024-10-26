module biome_repo
  use, intrinsic :: iso_c_binding
  use :: luajit
  use :: string
  use :: hashmap_str
  use :: hashmap_int
  use :: vector
  implicit none


  private


  public :: initialize_biome_repo_module
  public :: biome_definition
  public :: biome_repo_deploy_lua_api
  public :: register_biome
  public :: biome_repo_finalize
  public :: biome_repo_get_biome_pointer_by_id
  public :: biome_repo_copy_definition_array
  public :: biome_repo_destroy

  !* Bake the module name into the executable.

  character(len = 12, kind = c_char), parameter :: module_name = "[Biome Repo]"



  !* This is what lua will send into a queue to be processed after
  !* all biome definition have been processed into the engine.

  type :: luajit_biome_definition
    character(len = :, kind = c_char), pointer :: name => null()
    character(len = :, kind = c_char), pointer :: grass_layer => null()
    character(len = :, kind = c_char), pointer :: dirt_layer => null()
    character(len = :, kind = c_char), pointer :: stone_layer => null()
    real(c_float) :: heat_min = 0.0
    real(c_float) :: heat_max = 0.0
  end type luajit_biome_definition


  !* Biome definition container.

  type :: biome_definition
    integer(c_int) :: grass_layer = 0
    integer(c_int) :: dirt_layer = 0
    integer(c_int) :: stone_layer = 0
    real(c_float) :: heat_min = 0.0
    real(c_float) :: heat_max = 0.0
  end type biome_definition


  ! Random access oriented.
  !* Type: biome_definition.
  type(hashmap_string_key) :: definition_database


  ! Linear access oriented.
  !* Type: biome_definition
  !? NOTE: the definition_database is the one responsible for cleaning up the pointers.
  type(vec) :: definition_array

  ! Reverse lookup.
  !* Type: string pointer.
  type(hashmap_integer_key) :: biome_id_to_name_database


  !! These two will be destroyed as the game starts.

  ! Random access oriented.
  !* Type: luajit_biome_definition
  type(hashmap_string_key) :: luajit_definition_database

  ! Linear access oriented.
  !* Type: luajit_biome_definition
  !? NOTE: the definition_database is the one responsible for cleaning up the pointers.
  type(vec) :: luajit_definition_array


contains


  subroutine initialize_biome_repo_module()
    implicit none

    type(biome_definition) :: blank
    type(luajit_biome_definition) :: blank_lua
    character(len = :, kind = c_char), pointer :: blank_string_pointer

    !* Type: biome_definition
    definition_database = new_hashmap_string_key(sizeof(blank), gc_definition_repo)

    !* Create the base smart pointer of the biome array.
    definition_array = new_vec(sizeof(blank), 0_8)

    !* Create the reverse lookup pointers.
    biome_id_to_name_database = new_hashmap_integer_key(sizeof(blank_string_pointer), gc_biome_id_database)

    !* Type: luajit_biome_definition
    luajit_definition_database = new_hashmap_string_key(sizeof(blank_lua), gc_definition_repo_from_lua)

    !* Create the base smart pointer of the biome array.
    luajit_definition_array = new_vec(sizeof(blank_lua), 0_8)
  end subroutine initialize_biome_repo_module


  !* This hooks the required fortran functions into the LuaJIT "biome" table.
  subroutine biome_repo_deploy_lua_api(state)
    implicit none

    type(c_ptr), intent(in), value :: state

    ! Memory layout: (Stack grows down.)
    ! -1 - biome = {}
    ! then moves to:
    ! -3 - biome = {}
    ! -2 - table key string.
    ! -1 - function pointers.
    ! Then we pop -2 and -1 off the stack, shifting biome back to -1.


    call lua_getglobal(state, "biome")

    if (.not. lua_istable(state, -1)) then
      error stop "[Biome Repo] Error: Can't initialize function pointers. [biome] table is missing!"
    end if

    ! Swap the declaration with the actual fortran function.
    call luajit_swap_table_function(state, "register", register_biome)


    ! Now clear the stack. We're done with the biome LuaJIT table.
    call lua_pop(state, lua_gettop(state))
  end subroutine biome_repo_deploy_lua_api


  !* This allows you to register a biome into the engine from LuaJIT.
  !* See the LuaJIT API [./api/init.lua] for the layout of biome_definition.
  recursive function register_biome(state) result(status) bind(c)
    use :: string
    use :: array, only: string_array
    implicit none

    type(c_ptr), intent(in), value :: state
    ! We're going to be using the status quite a lot.
    integer(c_int) :: status
    ! biome_definition fields.
    type(heap_string) :: name, grass_layer, dirt_layer, stone_layer
    real(c_float) :: heat_min, heat_max
    !* The smart pointer where we will store the biome definiton.
    !* We will only allocate this after a successful data query from LuaJIT.
    type(luajit_biome_definition) :: new_definition

    status = LUAJIT_GET_OK

    ! Enforce the first and only argument to be a table.
    if (.not. lua_istable(state, -1)) then
      call luajit_error_stop(state, module_name//" Error: Cannot register biome. Not a table.")
    end if

    ! All components of the biome definition are required. (For now)
    call luajit_table_get_key_required(state, module_name, "Biome Definition", "name", name, "string")

    call luajit_table_get_key_required(state, module_name, "Biome Definition", "grass_layer", grass_layer, "string")

    call luajit_table_get_key_required(state, module_name, "Biome Definition", "dirt_layer", dirt_layer, "string")

    call luajit_table_get_key_required(state, module_name, "Biome Definition", "stone_layer", stone_layer, "string")

    call luajit_table_get_key_required(state, module_name, "Biome Definition", "heat_min", heat_min, "number")

    call luajit_table_get_key_required(state, module_name, "Biome Definition", "heat_max", heat_max, "number")


    !* todo: can add in more definition components here. :)


    ! Clean up the stack. We are done with the LuaJIT stack.
    !? The definition table has now disappeared.
    call lua_pop(state, lua_gettop(state))


    ! We have completed a successful query of the definition table from LuaJIT.
    ! Put all the data into the fortran database.

    call string_copy_pointer_to_pointer(name%get_pointer(), new_definition%name)

    call string_copy_pointer_to_pointer(grass_layer%get_pointer(), new_definition%grass_layer)

    call string_copy_pointer_to_pointer(dirt_layer%get_pointer(), new_definition%dirt_layer)

    call string_copy_pointer_to_pointer(stone_layer%get_pointer(), new_definition%stone_layer)

    new_definition%heat_min = heat_min

    new_definition%heat_max = heat_max

    ! print*,new_definition%name
    ! print*,new_definition%grass_layer
    ! print*,new_definition%dirt_layer
    ! print*,new_definition%stone_layer
    ! print*,new_definition%heat_min
    ! print*,new_definition%heat_max

    ! Copy the definition into the string based database.
    call luajit_definition_database%set(name%string, new_definition)

    call luajit_definition_array%push_back(new_definition)
  end function register_biome


  subroutine biome_repo_finalize()
    use :: block_repo
    implicit none

    type(luajit_biome_definition), pointer :: lua_definition
    type(biome_definition) :: definition
    type(c_ptr) :: raw_c_ptr
    integer(c_int) :: current_biome_id
    character(len = :, kind = c_char), pointer :: name_pointer

    print"(A)",module_name//": Finalizing biomes."


    current_biome_id = 0


    call luajit_definition_database%initialize_iterator()

    do while (luajit_definition_database%iterate(raw_c_ptr))

      call c_f_pointer(raw_c_ptr, lua_definition)

      if (definition_database%has_key(lua_definition%name)) then
        error stop module_name//" Error: Tried to overwrite biome: ["//lua_definition%name//"]"
      end if

      ! print*,lua_definition%name
      ! print*,lua_definition%grass_layer
      ! print*,lua_definition%dirt_layer
      ! print*,lua_definition%stone_layer
      ! print*,lua_definition%heat_min
      ! print*,lua_definition%heat_max


      if (.not. block_repo_get_id_from_name(lua_definition%grass_layer, definition%grass_layer)) then
        error stop module_name//": Biome ["//lua_definition%name//"] contains invalid block for [grass_layer]. ["//lua_definition%grass_layer//"]"
      end if

      if (.not. block_repo_get_id_from_name(lua_definition%dirt_layer, definition%dirt_layer)) then
        error stop module_name//": Biome ["//lua_definition%name//"] contains invalid block for [dirt_layer]. ["//lua_definition%dirt_layer//"]"
      end if

      if (.not. block_repo_get_id_from_name(lua_definition%stone_layer, definition%stone_layer)) then
        error stop module_name//": Biome ["//lua_definition%name//"] contains invalid block for [stone_layer]. ["//lua_definition%stone_layer//"]"
      end if

      definition%heat_min = lua_definition%heat_min
      definition%heat_max = lua_definition%heat_max

      call definition_database%set(lua_definition%name, definition)
      call definition_array%push_back(definition)

      allocate(character(len = len(lua_definition%name), kind = c_char) :: name_pointer)

      name_pointer = lua_definition%name

      call biome_id_to_name_database%set(int(current_biome_id, c_int64_t), name_pointer)

      print*,lua_definition%name, " is biome", current_biome_id

      current_biome_id = current_biome_id + 1
    end do

    ! Now destroy the LuaJIT components.
    call luajit_definition_array%destroy()

    call luajit_definition_array%destroy()
  end subroutine biome_repo_finalize


  !* Used for when walking around the world.
  !* You can get the biome definition from the X,Z you're in numeric value.
  function biome_repo_get_biome_pointer_by_id(id, biome_pointer) result(exists)
    implicit none

    integer(c_int), intent(in), value :: id
    type(biome_definition), intent(inout), pointer :: biome_pointer
    logical(c_bool) :: exists
    type(c_ptr) :: raw_c_ptr
    character(len = :, kind = c_char), pointer :: name_pointer

    exists = .false.

    if (.not. biome_id_to_name_database%get(int(id, c_int64_t), raw_c_ptr)) then
      return
    end if

    call c_f_pointer(raw_c_ptr, name_pointer)

    if (.not. definition_database%get(name_pointer, raw_c_ptr)) then
      error stop module_name//" Error: Biome exists in reverse lookup but not database. ["//name_pointer//"]"
    end if

    call c_f_pointer(raw_c_ptr, biome_pointer)

    exists = .true.
  end function biome_repo_get_biome_pointer_by_id


  !* This is named like this so it's never used out of place.
  function biome_repo_copy_definition_array() result(clone_of)
    implicit none

    type(vec) :: clone_of

    call definition_array%clone(clone_of)
  end function biome_repo_copy_definition_array


  subroutine biome_repo_destroy()
    implicit none

    call definition_database%destroy()
    call definition_array%destroy()
  end subroutine biome_repo_destroy


  subroutine gc_definition_repo(raw_c_ptr)
    implicit none

    type(c_ptr), intent(in), value :: raw_c_ptr
    type(biome_definition), pointer :: definition_pointer

    call c_f_pointer(raw_c_ptr, definition_pointer)

    ! deallocate(definition_pointer%name)
  end subroutine gc_definition_repo


  subroutine gc_definition_repo_from_lua(raw_c_ptr)
    implicit none

    type(c_ptr), intent(in), value :: raw_c_ptr
    type(luajit_biome_definition), pointer :: definition_pointer

    call c_f_pointer(raw_c_ptr, definition_pointer)

    deallocate(definition_pointer%name)
    deallocate(definition_pointer%grass_layer)
    deallocate(definition_pointer%dirt_layer)
    deallocate(definition_pointer%stone_layer)
  end subroutine gc_definition_repo_from_lua


  subroutine gc_biome_id_database(raw_c_ptr)
    implicit none

    type(c_ptr), intent(in), value :: raw_c_ptr
    character(len = :, kind = c_char), pointer :: str_pointer

    call c_f_pointer(raw_c_ptr, str_pointer)

    deallocate(str_pointer)
  end subroutine gc_biome_id_database


end module biome_repo
