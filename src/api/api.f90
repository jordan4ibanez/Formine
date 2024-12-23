module api
  use :: luajit
  use :: string_f90
  use :: hashmap_str
  use, intrinsic :: iso_c_binding
  !* LuaJIT API compatible modules.
  use :: block_repo
  use :: biome_repo
  implicit none


  !* This LuaJIT API has been written with love and care. :)
  !*
  !* Everything you see get loaded in, you can follow.
  !* I will try to document it the best I can.
  !*
  !* I am using the tables as a "LuaJIT preprocessor".
  !* Therefore, they will use replace instead of insert
  !* in the api functions.

  ! Ideas:
  !
  ! From: the4spaceconstants2181
  ! Idea: Recipe mod origin.
  ! Addition: This could be implemented for blocks and biomes as well.


  private


  public :: api_initialize
  public :: api_destroy


  type :: mod_config
    character(len = :, kind = c_char), pointer :: name => null()
    character(len = :, kind = c_char), pointer :: description => null()
    character(len = :, kind = c_char), pointer :: path => null()
  end type mod_config


  !* Type: mod_config
  type(hashmap_string_key) :: mod_database

  type(c_ptr) :: lua_state

contains


  !* Initialize the API.
  subroutine api_initialize()
    use :: texture_atlas
    implicit none

    integer(c_int) :: status

    call initialize_block_repo_module()

    call initialize_biome_repo_module()

    mod_database = new_hashmap_string_key(sizeof(mod_config()), gc_mod_database)

    call luajit_initialize(lua_state)

    status = luajit_run_file(lua_state, "./api/init.lua")

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

    ! Initialize LuaJIT compatible modules.
    call api_deploy_lua_modules()

    ! Load up all mods.
    call load_all_mods()

    ! Finalize (post-process) LuaJIT compatible modules.
    call api_finalize_lua_modules()

    ! Pack the loaded texture data.
    call texture_atlas_pack()
  end subroutine api_initialize


  subroutine api_deploy_lua_modules()
    implicit none

    call block_repo_deploy_lua_api(lua_state)

    call biome_repo_deploy_lua_api(lua_state)
  end subroutine api_deploy_lua_modules


  subroutine api_finalize_lua_modules()
    implicit none

    call biome_repo_finalize()
  end subroutine api_finalize_lua_modules


  !* Clean up the API data.
  subroutine api_destroy()
    implicit none

    call luajit_destroy(lua_state)
    call mod_database%destroy()
    call block_repo_destroy()
    print"(A)","[API]: Destroyed API."
  end subroutine api_destroy


  !* This will attempt to load up all init.lua files in the mod folders.
  subroutine load_all_mods()
    use :: directory
    implicit none

    type(directory_reader) :: dir_reader
    integer(c_int) :: i, status
    logical :: found_mods_folder
    character(len = :, kind = c_char), allocatable :: folder_name, mod_path_string, conf_path_string, init_path_string
    type(mod_config) :: mod_config_struct


    found_mods_folder = .false.

    ! We can reuse the directory reader.
    if (.not. dir_reader%read_directory("./")) then
      error stop "[API] Error: Something has gone wrong with the current working directory."
    end if

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

    call dir_reader%destroy()


    if (.not. dir_reader%read_directory("./mods/")) then
      error stop "[API] error: The mods folder doesn't exist."
    end if

    if (dir_reader%folder_count == 0) then
      error stop "[API] error: There are no mods installed."
    end if

    do i = 1,dir_reader%folder_count
      folder_name = dir_reader%folders(i)%string
      mod_path_string = "./mods/"//folder_name//"/"
      init_path_string = mod_path_string//"init.lua"
      conf_path_string = mod_path_string//"mod.conf"

      mod_config_struct = construct_mod_config_from_file(conf_path_string, mod_path_string)

      ! Check if there's already a mod with this name in the database.
      if (mod_database%has_key(mod_config_struct%name)) then
        error stop "[API] error: There is already a mod named ["//mod_config_struct%name//"]. Culprit: ["//mod_path_string//"]"
      end if

      status = luajit_run_file(lua_state, init_path_string)
      if (status /= LUAJIT_RUN_FILE_OK) then
        select case (status)
         case (LUAJIT_RUN_FILE_FAILURE)
          error stop "[API] error: Failed to run the mod ["//mod_config_struct%name//"]. Execution error."
         case (LUAJIT_RUN_FILE_MISSING)
          error stop "[API] error: Failed to run the mod ["//mod_config_struct%name//"]. Missing init.lua"
         case default
          error stop "[API] error: Failed to run the mod ["//mod_config_struct%name//"]. UNIMPLEMENTED ERROR!"
        end select
      end if

      ! If the mod loaded up properly, we can store the configuration.
      call mod_database%set(mod_config_struct%name, mod_config_struct)

      ! Finally, we want to get all the textures stored in the [./mod/mod_name/textures/] folder.
      call load_up_all_textures(mod_path_string)

      print"(A)","[API]: Loaded mod ["//mod_config_struct%name//"]"
    end do

    call dir_reader%destroy()
  end subroutine load_all_mods


  !* This will take the mod folder's conf file (if it exists) and parse it.
  function construct_mod_config_from_file(path, mod_path) result(new_mod_config)
    use :: files_f90
    implicit none

    character(len = *, kind = c_char), intent(in) :: path, mod_path
    type(mod_config) :: new_mod_config
    type(file_reader) :: reader
    character(len = :, kind = c_char), allocatable :: temp_string, value_string
    integer(c_int) :: i

    if (.not. reader%read_lines(path)) then
      error stop "[API] error: Mod folder ["//string_remove_file_name_from_path(path)//"] is missing a [mod.conf] file."
    end if

    if (reader%line_count == 0) then
      error stop "[API] error: Mod folder ["//string_remove_file_name_from_path(path)//"] has a blank [mod.conf] file."
    end if

    ! Parse each line to try to accumulate the required elements.
    do i = 1,reader%line_count
      temp_string = reader%lines(i)%string

      if (string_starts_with(temp_string, "name = ")) then
        value_string = string_get_right_of_character(temp_string, "=")
        if (value_string == "") then
          error stop "[API] error: Missing value for [mod.conf] key [name]."
        end if

        allocate(character(len = len(value_string), kind = c_char) :: new_mod_config%name)
        new_mod_config%name = value_string
      else if (string_starts_with(temp_string, "description = ")) then
        value_string = string_get_right_of_character(temp_string, "=")
        if (value_string == "") then
          error stop "[API] error: Missing value for [mod.conf] key [description]."
        end if
        allocate(character(len = len(value_string), kind = c_char) :: new_mod_config%description)
        new_mod_config%description = value_string
      end if
    end do

    allocate(character(len = len(mod_path), kind = c_char) :: new_mod_config%path)
    new_mod_config%path = mod_path
  end function construct_mod_config_from_file


  !* Load up all PNG textures in the mod folder's texture folder. (if it exists)
  !* This will only load PNG images.
  !* If there is no textures folder this is a no-op.
  !* If there are no textures in the textures folder this is a no-op.
  !* This will only attempt to go 4 folders deep.
  subroutine load_up_all_textures(mod_path)
    use :: directory
    implicit none

    character(len = *, kind = c_char), intent(in) :: mod_path
    character(len = :, kind = c_char), allocatable :: textures_path
    type(directory_reader), dimension(5) :: dir_readers
    integer(c_int) :: i, a, b, c, d, e
    logical :: found_textures_folder

    found_textures_folder = .false.

    if (.not. dir_readers(1)%read_directory(mod_path)) then
      error stop "[API] Error: Failed to read directory ["//mod_path//"]"
    end if

    ! No folders.
    if (dir_readers(1)%folder_count == 0) then
      return
    end if

    do i = 1,dir_readers(1)%folder_count
      if (dir_readers(1)%folders(i)%string == "textures") then
        found_textures_folder = .true.
      end if
    end do

    ! No textures folders.
    if (.not. found_textures_folder) then
      return
    end if

    ! We're going to reallocate the base directory reader.
    call dir_readers(1)%destroy()

    textures_path = mod_path//"textures/"

    if (.not. dir_readers(1)%read_directory(textures_path)) then
      error stop "[API] Error: Failed to read ["//textures_path//"]"
    end if

    ! This allows for 4 folders deep.
    ! This is a bit complicated, I could have made this a recursive function
    ! but I wanted to keep the implementation flat, dumb, and simple.
    ! Needless to say, the textures_path variable will be getting a workout.

    ! Root level.
    do a = 1,dir_readers(1)%file_count
      call attempt_texture_upload(dir_readers(1)%files(a)%string, textures_path)
    end do

    do a = 1,dir_readers(1)%folder_count

      !* + 1 depth.
      textures_path = mod_path//"textures/"//dir_readers(1)%folders(a)%string//"/"

      call dir_readers(2)%destroy()
      if (.not. dir_readers(2)%read_directory(textures_path)) then
        error stop "[API] Error: Failed to read ["//textures_path//"]"
      end if

      do b = 1,dir_readers(2)%file_count
        call attempt_texture_upload(dir_readers(2)%files(b)%string, textures_path)
      end do

      do b = 1,dir_readers(2)%folder_count

        !* + 2 depth.
        textures_path = mod_path//"textures/"// &
          dir_readers(1)%folders(a)%string//"/"// &
          dir_readers(2)%folders(b)%string//"/"

        call dir_readers(3)%destroy()
        if (.not. dir_readers(3)%read_directory(textures_path)) then
          error stop "[API] Error: Failed to read ["//textures_path//"]"
        end if

        do c = 1,dir_readers(3)%file_count
          call attempt_texture_upload(dir_readers(3)%files(c)%string, textures_path)
        end do

        do c = 1,dir_readers(3)%folder_count

          !* + 3 depth.
          textures_path = mod_path//"textures/"// &
            dir_readers(1)%folders(a)%string//"/"// &
            dir_readers(2)%folders(b)%string//"/"// &
            dir_readers(3)%folders(c)%string//"/"

          call dir_readers(4)%destroy()
          if (.not. dir_readers(4)%read_directory(textures_path)) then
            error stop "[API] Error: Failed to read ["//textures_path//"]"
          end if

          do d = 1,dir_readers(4)%file_count
            call attempt_texture_upload(dir_readers(4)%files(d)%string, textures_path)
          end do

          do d = 1,dir_readers(4)%folder_count

            !* + 4 depth.
            textures_path = mod_path//"textures/"// &
              dir_readers(1)%folders(a)%string//"/"// &
              dir_readers(2)%folders(b)%string//"/"// &
              dir_readers(3)%folders(c)%string//"/"// &
              dir_readers(4)%folders(d)%string//"/"

            call dir_readers(5)%destroy()
            if (.not. dir_readers(5)%read_directory(textures_path)) then
              error stop "[API] Error: Failed to read ["//textures_path//"]"
            end if

            do e = 1,dir_readers(5)%file_count
              call attempt_texture_upload(dir_readers(5)%files(e)%string, textures_path)
            end do
          end do
        end do
      end do
    end do

    do i = 1,size(dir_readers)
      call dir_readers(i)%destroy()
    end do
  end subroutine load_up_all_textures


  !* We will not only upload the texture into the texture module,
  !* We will also put it into the texture atlas so we can stitch it together.
  subroutine attempt_texture_upload(file_name, file_path)
    use :: texture
    use :: texture_atlas
    implicit none

    character(len = *, kind = c_char) :: file_name, file_path
    character(len = :, kind = c_char), allocatable :: full_file_path

    if (string_get_file_extension(file_name) == "png") then
      full_file_path = file_path//file_name
      call texture_create(full_file_path)
      call texture_atlas_add_texture_to_pack(full_file_path, file_name)
    end if
  end subroutine attempt_texture_upload


  subroutine gc_mod_database(raw_c_ptr)
    implicit none

    type(c_ptr), intent(in), value :: raw_c_ptr
    type(mod_config), pointer :: mod_config_pointer

    call c_f_pointer(raw_c_ptr, mod_config_pointer)

    deallocate(mod_config_pointer%name)
    deallocate(mod_config_pointer%description)
    deallocate(mod_config_pointer%path)
  end subroutine gc_mod_database


end module api
