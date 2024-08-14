module texture_atlas
  use, intrinsic :: iso_c_binding
  implicit none

  !* We need a pool of things to build upon.
  !* The game only has one texture atlas.

  private


contains

  subroutine texture_atlas_add_texture_name(texture_name)
    implicit none

    character(len = *, kind = c_char), intent(in) :: texture_name

    !! And then here we'd add it into an array using the heap_string type.

  end subroutine texture_atlas_add_texture_name


end module texture_atlas
