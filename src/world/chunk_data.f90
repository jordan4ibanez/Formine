module chunk_data
  use :: string
  use :: vector_2i
  use, intrinsic :: iso_c_binding
  implicit none

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
    type(vec2i), allocatable :: world_position
    type(block_data), dimension(CHUNK_HEIGHT, CHUNK_WIDTH, CHUNK_WIDTH), allocatable :: data(:, :, :)
    type(heap_string), dimension(MESH_STACK_ARRAY_SIZE), allocatable :: mesh(:)
  end type memory_chunk

  interface memory_chunk
    module procedure :: memory_chunk_constructor
  end interface memory_chunk


contains


  function memory_chunk_constructor(x, y) result(memory_chunk_new)
    implicit none

    integer(c_int) :: x, y
    type(memory_chunk), pointer :: memory_chunk_new

    allocate(memory_chunk_new)
    allocate(memory_chunk_new%world_position)
    memory_chunk_new%world_position = [x,y]
    allocate(memory_chunk_new%data(CHUNK_HEIGHT, CHUNK_WIDTH, CHUNK_WIDTH))
    allocate(memory_chunk_new%mesh(MESH_STACK_ARRAY_SIZE))
  end function memory_chunk_constructor


end module chunk_data
