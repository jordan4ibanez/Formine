module texture_atlas
  use :: string
  use :: vector_2i
  use, intrinsic :: iso_c_binding
  implicit none


  !* We need a pool of things to build upon.
  !* The game only has one texture atlas.


  private


  public :: texture_atlas_initialize
  public :: texture_atlas_add_texture_to_pack


  type :: texture_pack_element
    type(heap_string) :: full_path
    type(heap_string) :: file_name
  end type texture_pack_element


  type(texture_pack_element), dimension(:), allocatable :: textures_to_pack


contains


  !* Set up the texture atlas for use.
  subroutine texture_atlas_initialize()
    implicit none

    allocate(textures_to_pack(0))
  end subroutine texture_atlas_initialize


  !* Add a texture for the texture atlas to stitch together.
  subroutine texture_atlas_add_texture_to_pack(full_path, file_name)
    implicit none

    character(len = *, kind = c_char), intent(in) :: full_path, file_name
    type(texture_pack_element), allocatable :: new_element
    type(texture_pack_element), dimension(:), allocatable :: temp_string_array

    allocate(new_element)

    new_element%full_path = full_path
    new_element%file_name = file_name

    temp_string_array = array_texture_pack_element_insert(textures_to_pack, new_element)
    call move_alloc(temp_string_array, textures_to_pack)
  end subroutine texture_atlas_add_texture_to_pack


  !* Insert a value at the end of a memory texture array.
  function array_texture_pack_element_insert(input, new_value) result(output)
    use :: memory_texture_module
    implicit none

    type(texture_pack_element), dimension(:), intent(in) :: input
    type(texture_pack_element), intent(in), value :: new_value
    type(texture_pack_element), dimension(:), allocatable :: output
    integer(c_int) :: old_size, i

    old_size = size(input)

    allocate(output(old_size + 1))

    do i = 1,old_size
      output(i) = input(i)
    end do

    output(old_size + 1) = new_value
  end function array_texture_pack_element_insert


end module texture_atlas
