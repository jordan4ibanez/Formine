module api
  use :: luajit
  use :: string
  use :: fhash, only: fhash_tbl_t, key => fhash_key
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


  type :: mod_config
    type(heap_string) :: name
    type(heap_string) :: description
    type(heap_string) :: path
  end type mod_config


  type(fhash_tbl_t) :: mod_database
  type(c_ptr) :: lua_state

contains


  !* Initialize the API.
  subroutine api_initialize()
    implicit none

    call luajit_initialize(lua_state)

    associate (status => luajit_run_file(lua_state, "./api/init.lua"))
      if (status /= LUAJIT_RUN_FILE_OK) then
        select case(status)
         case (LUAJIT_RUN_FILE_FAILURE)
          error stop "[API] Error: Failed to load the API init file. Execution error."
         case (LUAJIT_RUN_FILE_MISSING)
          ! Someone removed the api init file, eh?
          error stop "[API] Error: Failed to load the API init file. It's missing."
         case default
          error stop "[API] Error: Failed to load the API init file. UNIMPLEMENTED ERROR!"
        end select
      end if
    end associate

    ! Initialize LuaJIT compatible modules.
    call block_repo_deploy_lua_api(lua_state)

    ! Load up all mods.
    call load_all_mods()
  end subroutine api_initialize


  !* Clean up the API data.
  subroutine api_destroy()
    implicit none

    call luajit_destroy(lua_state)
  end subroutine api_destroy


  !* Run a LuaJIT file.
  function api_run_file(file_path) result(status)
    implicit none

    character(len = *, kind = c_char), intent(in) :: file_path
    integer(c_int) :: status

    status = luajit_run_file(lua_state, file_path)
  end function api_run_file


  !* This will attempt to load up all init.lua files in the mod folders.
  subroutine load_all_mods()
    use :: directory
    implicit none

    type(directory_reader) :: dir_reader
    integer :: i
    logical :: found_mods_folder
    character(len = :, kind = c_char), allocatable :: folder_name, mod_path_string, conf_path_string, init_path_string
    type(mod_config) :: mod_config_struct


    found_mods_folder = .false.

    ! We can reuse the directory reader.
    call dir_reader%read_directory("./")

    ! So let's get the mods folder.

    if (dir_reader%folder_count == 0) then
      error stop "[API] error: No folders in the directory [./]"
    end if

    do i = 1,dir_reader%folder_count
      if (dir_reader%folders(i) == "mods") then
        found_mods_folder = .true.
        exit
      end if
    end do

    if (.not. found_mods_folder) then
      error stop "[API] error: Could not find the [mods] folder in the directory [./]"
    end if

    ! Now, we can attempt to load up all the mods.

    call dir_reader%deallocate_memory()

    call dir_reader%read_directory("./mods/")

    if (dir_reader%folder_count == 0) then
      error stop "[API] error: There are no mods installed."
    end if

    do i = 1,dir_reader%folder_count
      folder_name = dir_reader%folders(i)%get()
      mod_path_string = "./mods/"//folder_name//"/"
      init_path_string = mod_path_string//"init.lua"
      conf_path_string = mod_path_string//"mod.conf"

      mod_config_struct = construct_mod_config_from_file(conf_path_string, mod_path_string)

      associate (status => api_run_file(init_path_string))
        if (status /= LUAJIT_RUN_FILE_OK) then
          select case (status)
           case (LUAJIT_RUN_FILE_FAILURE)
            error stop "[API] error: Failed to run the mod ["//mod_config_struct%name%get()//"]. Execution error."
           case (LUAJIT_RUN_FILE_MISSING)
            error stop "[API] error: Failed to run the mod ["//mod_config_struct%name%get()//"]. Missing init.lua"
           case default
            error stop "[API] error: Failed to run the mod ["//mod_config_struct%name%get()//"]. UNIMPLEMENTED ERROR!"
          end select
        end if
      end associate

      ! Now, if the mod loaded up properly, we can store the configuration.
      call mod_database%set(key(mod_config_struct%name%get()), mod_config_struct)
    end do
  end subroutine load_all_mods


  !* This will take the mod folder's conf file (if it exists) and parse it.
  function construct_mod_config_from_file(path, mod_path) result(new_mod_config)
    use :: files
    implicit none

    character(len = *, kind = c_char), intent(in) :: path, mod_path
    type(mod_config) :: new_mod_config
    type(file_reader) :: reader
    character(len = :, kind = c_char), allocatable :: temp_string, value_string
    integer :: i

    call reader%read_lines(path)

    if (.not. reader%exists) then
      error stop "[API] error: Mod folder ["//path//"] is missing a [mod.conf] file."
    end if

    if (reader%line_count == 0) then
      error stop "[API] error: Mod folder ["//path//"] has a blank [mod.conf] file."
    end if

    ! Parse each line to try to accumulate the required elements.
    do i = 1,reader%line_count
      temp_string = reader%lines(i)%get()

      if (string_starts_with(temp_string, "name = ")) then
        value_string = string_get_right_of_character(temp_string, "=")
        if (value_string == "") then
          error stop "[API] error: Missing value for [mod.conf] key [name]."
        end if
        new_mod_config%name = value_string
      else if (string_starts_with(temp_string, "description = ")) then
        value_string = string_get_right_of_character(temp_string, "=")
        if (value_string == "") then
          error stop "[API] error: Missing value for [mod.conf] key [description]."
        end if
        new_mod_config%description = value_string
      end if
    end do

    new_mod_config%path = mod_path
  end function construct_mod_config_from_file


end module api
