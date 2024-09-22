module texture_gc
  use, intrinsic :: iso_c_binding
  use :: hashmap_types
  use :: opengl
  implicit none


contains


  subroutine texture_database_gc(element)
    use :: hashmap_bindings
    use :: string, only: int_to_string
    implicit none

    !? Element is an OpenGL ID. [integer(c_int)]
    class(*), pointer :: element
    integer(c_int) :: texture_id

    select type (element)
     type is (integer(c_int))
      texture_id = element
     class default
      error stop "[Texture GC]: Wrong type given."
    end select

    ! Make sure we don't accidentally cause a segmentation fault in the C code.
    call gl_bind_texture(GL_TEXTURE_2D, 0)

    ! Now delete it.
    call gl_delete_textures(texture_id)

    ! And if we have made a severe mistake, stop everything.
    ! This is a massive memory leak waiting to happen.
    if (gl_is_texture(texture_id)) then
      error stop "[Texture] Error: Attempt to delete texture ID ["//int_to_string(texture_id)//"] has failed. Halting."
    end if

    !? Element pointer is of type integer(c_int).
    ! All we do is free it.
    deallocate(element)
  end subroutine texture_database_gc


end module texture_gc
