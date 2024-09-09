module chunk_data
  use :: string
  use :: vector_2i
  use :: texture_atlas
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


  !* The data that will get sent to the chunk mesh generator.
  type :: chunk_mesh_generator_message
    !* The position in the world in which the chunk resides.
    type(vec2i), pointer :: world_position => null()
    !* Current chunk.
    type(memory_chunk), pointer :: current => null()
    !* Neighbor: -X
    type(memory_chunk), pointer :: left => null()
    !* Neighbor: +X
    type(memory_chunk), pointer :: right => null()
    !* Neighbor: -Z
    type(memory_chunk), pointer :: back => null()
    !* Neighbor: +Z
    type(memory_chunk), pointer :: front => null()
    !* Pointer of texture indices into the positions array.
    integer(c_int), dimension(6,0), pointer :: texture_indices(:, :)
    !* Texture positions array.
    type(texture_rectangle), dimension(:), pointer :: texture_positions_array
    !* Total number of textures.
    integer(c_int) :: texture_count = 0
    !* Which stack portion to generate.
    integer(c_int) :: mesh_stack = -1
  end type chunk_mesh_generator_message


contains


  function memory_chunk_constructor(x, y) result(new_memory_chunk)
    implicit none

    integer(c_int) :: x, y
    type(memory_chunk), pointer :: new_memory_chunk

    allocate(new_memory_chunk)
    allocate(new_memory_chunk%world_position)
    new_memory_chunk%world_position = [x,y]
    allocate(new_memory_chunk%data(CHUNK_HEIGHT, CHUNK_WIDTH, CHUNK_WIDTH))
    allocate(new_memory_chunk%mesh(MESH_STACK_ARRAY_SIZE))
  end function memory_chunk_constructor


end module chunk_data
