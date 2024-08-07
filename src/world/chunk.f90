module chunk
  use, intrinsic :: iso_c_binding
  implicit none


  private


  ! Width stands for X and Z. There is no sense in defining depth as they're equal sized.
  integer(c_int), parameter :: CHUNK_WIDTH = 16
  integer(c_int), parameter :: CHUNK_HEIGHT = 128



  type block_data
    ! Starts off as air.
    integer(c_int) :: id = 0
    ! Starts off as pitch black. Range: 0-15
    integer(1) :: light = 0
    
  end type block_data


  type chunk_data


  end type chunk_data


contains




end module chunk
