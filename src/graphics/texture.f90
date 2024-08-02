module texture
  use :: fhash, only: fhash_tbl_t, key => fhash_key
  implicit none


  private


  public :: texture_create
  public :: texture_use
  public :: texture_delete
  public :: texture_exists
  public :: texture_clear_database


  type(fhash_tbl_t) :: texture_database
  logical :: debug_mode = .true.


contains

  subroutine texture_create(texture_location)
    use :: stb_image
    use :: string
    use :: opengl
    use :: terminal
    use, intrinsic :: iso_c_binding
    implicit none

    character(len = *, kind = c_char), intent(in) :: texture_location
    integer :: x, y, channels, desired_channels
    character(len = :, kind = c_char), allocatable :: c_file_location
    integer(1), dimension(:), allocatable :: image_data
    integer(c_int) :: texture_id
    character(len = :, kind = c_char), allocatable :: file_name

    c_file_location = into_c_string(texture_location)

    ! We always want 4 channels.
    desired_channels = 4

    image_data = stbi_load(c_file_location, x, y, channels, desired_channels)

    if (x + y + channels == 0) then
      error stop "[Texture] Error: Could not load texture. It does not exist."
    end if

    ! First we must generate the texture ID.
    texture_id = gl_gen_textures()

    if (texture_id == 0) then
      error stop "[Texture] Error: Failed to generate OpenGL texture."
    end if

    ! Bind to it.
    call gl_bind_texture(GL_TEXTURE_2D, texture_id)

    ! Enable texture clamping to edge.
    call gl_tex_parameter_i(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_BORDER)
    call gl_tex_parameter_i(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_BORDER)

    ! Create a completely transparent border color.
    call gl_tex_parameter_fv(GL_TEXTURE_2D, GL_TEXTURE_BORDER_COLOR, [0.0, 0.0, 0.0, 0.0]);

    ! Nearest neighbor texture filtering.
    call gl_tex_parameter_i(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST)
    call gl_tex_parameter_i(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST)

    ! We're telling OpenGL that the width of each data component is 1 byte.
    call gl_pixel_store_i(GL_UNPACK_ALIGNMENT, 1)

    ! And now, we upload it.
    call gl_tex_image_2d(GL_TEXTURE_2D, 0, GL_RGBA, x, y, 0, GL_RGBA, GL_UNSIGNED_BYTE, image_data)

    ! This is done by the file name, we don't care about the path.
    file_name = get_file_name_from_string(texture_location)

    ! We ensure that this thing exists.
    if (.not. gl_is_texture(texture_id)) then
      error stop "[Texture] Error: Failed to create texture ["//texture_location//"]. Does not exist."
    else
      print"(A)", "[Texture]: Created ["//file_name//"] at ID ["//int_to_string(texture_id)//"]"
    end if

    ! Generate the mipmaps.
    call gl_generate_mipmap(GL_TEXTURE_2D)

    ! Finally, unbind. Done.
    call gl_bind_texture(GL_TEXTURE_2D, 0)

    ! Now we can assign it into the database by the file name.
    call set_texture(file_name, texture_id)
  end subroutine texture_create


  subroutine texture_use(texture_name)
    use :: opengl
    use :: terminal
    use, intrinsic :: iso_c_binding
    implicit none

    character(len = *), intent(in) :: texture_name
    integer(c_int) :: texture_id, status

    call texture_database%get(key(texture_name), texture_id, stat = status)

    if (status /= 0) then
      print"(A)", colorize_rgb("[Texture] Error: Texture ["//texture_name//"] does not exist. Cannot use.", 255, 0, 0)
      return
    end if

    call gl_bind_texture(GL_TEXTURE_2D, texture_id)
  end subroutine texture_use


  subroutine set_texture(texture_name, new_texture)
    use, intrinsic :: iso_c_binding
    implicit none

    character(len = *), intent(in) :: texture_name
    integer(c_int), intent(in) :: new_texture

    if (debug_mode) then
      print"(A)", "[Texture]: set texture ["//texture_name//"]"
    end if

    call texture_database%set(key(texture_name), new_texture)
  end subroutine set_texture


  function get_texture(texture_name) result(texture_id)
    use, intrinsic :: iso_c_binding
    use :: terminal
    implicit none

    character(len = *), intent(in) :: texture_name
    integer(c_int) :: texture_id, status

    call texture_database%get(key(texture_name), texture_id, stat = status)

    if (status /= 0) then
      print"(A)",colorize_rgb("[Texture] Error: ["//texture_name//"] does not exist.", 255, 0, 0)
    end if
  end function get_texture


  subroutine texture_delete(texture_name)
    use, intrinsic :: iso_c_binding
    use :: opengl
    use :: terminal
    implicit none

    character(len = *), intent(in) :: texture_name
    integer(c_int) :: texture_id, status

    call texture_database%get(key(texture_name), texture_id, stat = status)

    if (status /= 0) then
      print"(A)",colorize_rgb("[Texture] Error: Texture ["//texture_name//"] does not exist. Cannot delete.", 255, 0, 0)
      return
    end if

    ! Make sure we don't accidentally cause a segmentation fault in the C code.
    call gl_bind_texture(GL_TEXTURE_2D, 0)

    ! Now delete it.
    call gl_delete_textures(texture_id)

    ! And if we have made a severe mistake, stop everything.
    ! This is a massive memory leak waiting to happen.
    if (gl_is_texture(texture_id)) then
      error stop "[Texture] Error: Attempt to delete texture ["//texture_name//"] has failed. Halting."
    end if

    ! Finally, remove it from the database.
    call texture_database%unset(key(texture_name))
  end subroutine texture_delete


  logical function texture_exists(texture_name) result(existence)
    use, intrinsic :: iso_c_binding
    implicit none

    character(len = *), intent(in) :: texture_name
    integer(c_int) :: status

    call texture_database%check_key(key(texture_name), stat = status)

    existence = status == 0
  end function texture_exists


  !* Completely wipe out all existing textures. This might be slow.
  subroutine texture_clear_database()
    use :: fhash, only: fhash_iter_t, fhash_key_t
    use :: string
    use :: terminal
    implicit none

    type(heap_string), dimension(:), allocatable :: key_array
    type(fhash_iter_t) :: iterator
    class(fhash_key_t), allocatable :: generic_key
    class(*), allocatable :: generic_placeholder
    integer :: i
    integer :: remaining_size

    ! Start with a size of 0.
    allocate(key_array(0))

    ! Create the iterator.
    iterator = fhash_iter_t(texture_database)

    ! Now we will collect the keys from the iterator.
    do while(iterator%next(generic_key, generic_placeholder))
      ! Appending. Allocatable will clean up the old data.
      key_array = [key_array, heap_string_array(generic_key%to_string())]
    end do

    do i = 1,size(key_array)
      call texture_delete(key_array(i)%get())
    end do

    !* We will always check that the remaining size is 0. This will protect us from random issues.
    call texture_database%stats(num_items = remaining_size)

    if (remaining_size /= 0) then
      print"(A)", colorize_rgb("[Texture] Error: Did not delete all textures! Expected size: [0] | Actual: ["//int_to_string(remaining_size)//"]", 255, 0, 0)
    else
      print"(A)", "[Texture]: Successfully cleared the texture database."
    end if
  end subroutine texture_clear_database


end module texture
