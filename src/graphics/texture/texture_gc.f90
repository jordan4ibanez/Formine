module texture_gc
  use, intrinsic :: iso_c_binding
  use :: hashmap_types
  implicit none


contains


  subroutine texture_database_gc(element)
    use :: hashmap_bindings
    implicit none

    class(*), pointer :: element

    !? Element pointer is of type integer(c_int).
    ! All we do is free it.
    deallocate(element)

  end subroutine texture_database_gc


end module texture_gc
