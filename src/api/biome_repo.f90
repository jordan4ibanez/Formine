module biome_repo
  use, intrinsic :: iso_c_binding
  use :: luajit
  use :: string
  use :: hashmap_str
  use :: vector
  implicit none


  private


  public :: initialize_biome_repo_module
  public :: biome_definition
  public :: biome_repo_destroy


  !* This is what lua will send into a queue to be processed after
  !* all block definition have been processed into the engine.

  type :: biome_definition_from_lua
    character(len = :, kind = c_char), pointer :: name => null()
    character(len = :, kind = c_char), pointer :: grass_layer => null()
    character(len = :, kind = c_char), pointer :: dirt_layer => null()
    character(len = :, kind = c_char), pointer :: stone_layer => null()
  end type biome_definition_from_lua


  !* Biome definition container.

  type :: biome_definition
    character(len = :, kind = c_char), pointer :: name => null()
    integer(c_int) :: grass_layer = 0
    integer(c_int) :: dirt_layer = 0
    integer(c_int) :: stone_layer = 0
  end type biome_definition


  ! Random access oriented.
  !* Type: biome_definition_from_lua
  type(hashmap_string_key) :: definition_database_from_lua


  ! Random access oriented.
  !* Type: biome_definition.
  type(hashmap_string_key) :: definition_database


  ! Linear access oriented.
  !* Type: biome_definition
  !? NOTE: the definition_database is the one responsible for cleaning up the pointers.
  type(vec) :: definition_array


contains


  subroutine initialize_biome_repo_module()
    implicit none

    type(biome_definition) :: blank
    type(biome_definition_from_lua) :: blank_lua

    !* Type: biome_definition
    definition_database = new_hashmap_string_key(sizeof(blank), gc_definition_repo)

    !* Type: biome_definition_from_lua
    definition_database_from_lua = new_hashmap_string_key(sizeof(blank_lua), gc_definition_repo_from_lua)
  end subroutine initialize_biome_repo_module


  !* This hooks the required fortran functions into the LuaJIT "biome" table.
  subroutine biome_repo_deploy_lua_api(state)
    implicit none

    type(c_ptr), intent(in), value :: state
    type(biome_definition) :: blank

    ! Memory layout: (Stack grows down.)
    ! -1 - biome = {}
    ! then moves to:
    ! -3 - biome = {}
    ! -2 - table key string.
    ! -1 - function pointers.
    ! Then we pop -2 and -1 off the stack, shifting biome back to -1.


    call lua_getglobal(state, "world")

    if (.not. lua_istable(state, -1)) then
      error stop "[Biome Repo] Error: Can't initialize function pointers. [biome] table is missing!"
    end if

    ! Swap the declaration with the actual fortran function.
    call luajit_swap_table_function(state, "register", register_biome)


    ! Now clear the stack. We're done with the biome LuaJIT table.
    call lua_pop(state, lua_gettop(state))
  end subroutine biome_repo_deploy_lua_api




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

    deallocate(definition_pointer%name)
  end subroutine gc_definition_repo


  subroutine gc_definition_repo_from_lua(raw_c_ptr)
    implicit none

    type(c_ptr), intent(in), value :: raw_c_ptr
    type(biome_definition_from_lua), pointer :: definition_pointer

    call c_f_pointer(raw_c_ptr, definition_pointer)

    deallocate(definition_pointer%name)
    deallocate(definition_pointer%grass_layer)
    deallocate(definition_pointer%dirt_layer)
    deallocate(definition_pointer%stone_layer)
  end subroutine gc_definition_repo_from_lua


end module biome_repo
