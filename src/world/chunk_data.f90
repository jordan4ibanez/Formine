module chunk_data
  use :: string
  use :: vector_2i
  use :: texture_atlas
  use, intrinsic :: iso_c_binding
  implicit none

  ! public :: new_memory_chunk
  public :: new_memory_chunk_pointer

  !* Width stands for X and Z. There is no sense in defining depth as they're equal sized.

  integer(c_int), parameter :: CHUNK_WIDTH = 16
  integer(c_int), parameter :: CHUNK_HEIGHT = 128

  !* The chunk is divided up into 8 meshes with width, height, and depth of 16.
  !* 1 is the bottom, 8 is the top.

  integer(c_int), parameter :: MESH_STACK_ARRAY_SIZE = 8
  integer(c_int), parameter :: MESH_STACK_HEIGHT = 16

  !* The stride before we reach into actual data.
  integer(c_int), parameter :: XY_STRIDE = CHUNK_WIDTH * CHUNK_HEIGHT


  !* Block data is one element in a chunk.

  type :: block_data
    ! Starts off as air.
    integer(c_int) :: id = 0
    ! Starts off as pitch black. Range: 0-15
    integer(1) :: light = 0
    ! There is no use for state yet. So we're going to leave this disabled.
    ! integer(c_int) :: state = 0
  end type block_data


  !* Memory chunk is the data for the entire chunk.

  type :: memory_chunk
    ! Layout: [ Y, Z, X ]
    type(vec2i) :: world_position
    type(block_data), dimension(CHUNK_HEIGHT, CHUNK_WIDTH, CHUNK_WIDTH) :: data
    integer(c_int), dimension(MESH_STACK_ARRAY_SIZE) :: mesh = (/ 0, 0, 0, 0, 0, 0, 0, 0 /)
  end type memory_chunk


contains


  ! function new_memory_chunk(x, y) result(nmc)
  !   implicit none

  !   integer(c_int) :: x, y
  !   type(memory_chunk), allocatable :: nmc

  !   allocate(nmc)
  !   nmc%world_position = [x,y]
  ! end function new_memory_chunk


  function new_memory_chunk_pointer(x, y) result(nmcp)
    implicit none

    integer(c_int) :: x, y
    type(memory_chunk), pointer :: nmcp

    allocate(nmcp)
    nmcp%world_position = [x,y]
  end function new_memory_chunk_pointer


end module chunk_data
