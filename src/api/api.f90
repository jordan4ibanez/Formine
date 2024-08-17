module api
  use :: luajit
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
          error stop "[API] Error: Failed to load the init file. Execution error."
         case (LUAJIT_RUN_FILE_MISSING)
          ! Someone removed the api init file, eh?
          error stop "[API] Error: Failed to load the init file. It's missing."
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


  !* This will attempt to load up all init.lua files in the mods folder.
  subroutine load_all_mods()
    use :: directory
    implicit none

    type(directory_reader) :: dir_reader
    integer :: i
    logical :: found_mods_folder
    character(len = :, kind = c_char), allocatable :: path_string

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
      path_string = "./mods/"//dir_reader%folders(i)%get()
      print*,path_string
    end do
  end subroutine load_all_mods

end module api
