module texture_gc
  use, intrinsic :: iso_c_binding
  use :: opengl
  implicit none


contains


  subroutine texture_database_gc(raw_c_ptr)
    use :: hashmap_bindings
    use :: string_f90, only: int_to_string
    implicit none

    type(c_ptr), intent(in), value :: raw_c_ptr
    integer(c_int), pointer :: texture_id

    call c_f_pointer(raw_c_ptr, texture_id)

    ! Make sure we don't accidentally cause a segmentation fault in the C code.
    call gl_bind_texture(GL_TEXTURE_2D, 0)

    ! Now delete it.
    call gl_delete_textures(texture_id)

    ! And if we have made a severe mistake, stop everything.
    ! This is a massive memory leak waiting to happen.
    if (gl_is_texture(texture_id)) then
      error stop "[Texture] Error: Attempt to delete texture ID ["//int_to_string(texture_id)//"] has failed. Halting."
    end if
  end subroutine texture_database_gc


end module texture_gc
