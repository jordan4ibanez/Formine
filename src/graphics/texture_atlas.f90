module texture_atlas
  use :: string
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
    integer :: i

    allocate(new_element)

    new_element%full_path = full_path
    new_element%file_name = file_name

    textures_to_pack = [textures_to_pack, new_element]

    i = size(textures_to_pack)

    print*,i

    print*,sizeof(textures_to_pack)

    ! print*,allocated(textures_to_pack(i))

  end subroutine texture_atlas_add_texture_to_pack


end module texture_atlas
