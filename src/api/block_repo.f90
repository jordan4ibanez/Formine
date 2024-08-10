module block_repo
  use :: luajit
  use, intrinsic :: iso_c_binding
  implicit none


  private


  public :: register_block


contains

  subroutine block_repo_deploy_lua_api(state)
    implicit none

    type(c_ptr) :: state



  end subroutine block_repo_deploy_lua_api


  subroutine register_block(state)
    implicit none

    type(c_ptr) :: state

    print*,"hi"
  end subroutine register_block


end module block_repo
