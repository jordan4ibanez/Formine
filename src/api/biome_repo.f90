module biome_repo
  use, intrinsic :: iso_c_binding
  use :: luajit
  use :: string
  use :: hashmap_str
  use :: vector
  implicit none


  private


  public :: initialize_biome_repo_module
  public :: biome_definition
  public :: biome_repo_destroy


  !* Biome definition container.

  type :: biome_definition
    character(len = :, kind = c_char), pointer :: name => null()
    integer(c_int) :: grass_layer = 0
    integer(c_int) :: dirt_layer = 0
    integer(c_int) :: stone_layer = 0
  end type biome_definition


  ! Random access oriented.
  !* Type: biome_definition.
  type(hashmap_string_key) :: definition_database


  ! Linear access oriented.
  !* Type: biome_definition
  !? NOTE: the definition_database is the one responsible for cleaning up the pointers.
  type(vec) :: definition_array


contains


  subroutine initialize_biome_repo_module()
    implicit none

    type(biome_definition) :: blank

    !* Type: biome_definition
    definition_database = new_hashmap_string_key(sizeof(blank), gc_definition_repo)
  end subroutine initialize_biome_repo_module



  subroutine biome_repo_destroy()
    implicit none

    call definition_database%destroy()

    call definition_array%destroy()
  end subroutine biome_repo_destroy


  subroutine gc_definition_repo(raw_c_ptr)
    implicit none

    type(c_ptr), intent(in), value :: raw_c_ptr
    type(biome_definition), pointer :: definition_pointer

    call c_f_pointer(raw_c_ptr, definition_pointer)

    deallocate(definition_pointer%name)
  end subroutine gc_definition_repo


end module biome_repo
