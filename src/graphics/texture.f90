module texture
  use :: fhash, only: fhash_tbl_t, key => fhash_key
  use :: vector_2i
  use, intrinsic :: iso_c_binding
  implicit none


  private


  public :: texture_create
  public :: texture_create_from_memory
  public :: texture_use
  public :: texture_get_size
  public :: texture_delete
  public :: texture_exists
  public :: texture_clear_database


  type(fhash_tbl_t) :: texture_database
  type(fhash_tbl_t) :: texture_size_database
  logical :: debug_mode = .false.


contains


  !* Create a texture from a file path.
  subroutine texture_create(texture_file_path)
    use :: stb_image
    use :: string
    use :: opengl
    use :: terminal
    implicit none

    character(len = *, kind = c_char), intent(in) :: texture_file_path
    integer(c_int) :: x, y, channels, desired_channels
    integer(1), dimension(:), allocatable :: image_data
    integer(c_int) :: texture_id
    character(len = :, kind = c_char), allocatable :: file_name

    ! We always want 4 channels.
    desired_channels = 4

    image_data = stbi_load(texture_file_path, x, y, channels, desired_channels)

    if (x + y + channels == 0) then
      error stop "[Texture] Error: Could not load texture. File specified from file path ["//texture_file_path//"] does not exist."
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
    file_name = string_get_file_name(texture_file_path)

    ! We ensure that this thing exists.
    if (.not. gl_is_texture(texture_id)) then
      error stop "[Texture] Error: Failed to create texture ["//texture_file_path//"]. Does not exist."
    else
      if (debug_mode) then
        print"(A)", "[Texture]: Created ["//file_name//"] at ID ["//int_to_string(texture_id)//"]"
      end if
    end if

    ! Generate the mipmaps.
    call gl_generate_mipmap(GL_TEXTURE_2D)

    ! Finally, unbind. Done.
    call gl_bind_texture(GL_TEXTURE_2D, 0)

    ! Now we can assign it into the database by the file name and dimensions.
    call set_texture(file_name, texture_id, x, y)
  end subroutine texture_create


  !* Upload an array of unsigned bytes. 4 channels per element. RGBA.
  subroutine texture_create_from_memory(texture_name, raw_data, width, height)
    use :: stb_image
    use :: string
    use :: opengl
    use :: terminal
    implicit none

    character(len = *, kind = c_char), intent(in) :: texture_name
    integer(1), dimension(:), intent(in) :: raw_data
    integer, intent(in), value :: width, height
    integer(c_int) :: texture_id

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
    call gl_tex_image_2d(GL_TEXTURE_2D, 0, GL_RGBA, width, height, 0, GL_RGBA, GL_UNSIGNED_BYTE, raw_data)

    ! We ensure that this thing exists.
    if (.not. gl_is_texture(texture_id)) then
      error stop "[Texture] Error: Failed to create texture ["//texture_name//"]. Does not exist."
    else
      print"(A)", "[Texture]: Created ["//texture_name//"] at ID ["//int_to_string(texture_id)//"]"
    end if

    ! Generate the mipmaps.
    call gl_generate_mipmap(GL_TEXTURE_2D)

    ! Finally, unbind. Done.
    call gl_bind_texture(GL_TEXTURE_2D, 0)

    ! Now we can assign it into the database by the custom name and the dimensions.
    call set_texture(texture_name, texture_id, width, height)
  end subroutine texture_create_from_memory


  !* Tell OpenGL to use a texture
  subroutine texture_use(texture_name)
    use :: opengl
    use :: terminal
    implicit none

    character(len = *, kind = c_char), intent(in) :: texture_name
    integer(c_int) :: texture_id, status

    call texture_database%get(key(texture_name), texture_id, stat = status)

    if (status /= 0) then
      print"(A)", colorize_rgb("[Texture] Error: Texture ["//texture_name//"] does not exist. Cannot use.", 255, 0, 0)
      return
    end if

    call gl_bind_texture(GL_TEXTURE_2D, texture_id)
  end subroutine texture_use


  !* Internal only. Set a texture in the database.
  subroutine set_texture(texture_name, new_texture, x, y)
    implicit none

    character(len = *, kind = c_char), intent(in) :: texture_name
    integer(c_int), intent(in) :: new_texture, x, y

    ! This creates an enforcement where the texture must be deleted before it can be re-assigned.
    ! This prevents a severe memory leak.
    if (texture_exists(texture_name)) then
      error stop "[Texture] Error: Tried to overwrite texture ["//texture_name//"]. Please delete it before setting it."
    end if

    if (debug_mode) then
      print"(A)", "[Texture]: set texture ["//texture_name//"]"
    end if

    call texture_database%set(key(texture_name), new_texture)
    call texture_size_database%set(key(texture_name), vec2i(x, y))
  end subroutine set_texture


  !* Internal only. Get a texture from the database.
  function get_texture(texture_name) result(texture_id)
    use :: terminal
    implicit none

    character(len = *, kind = c_char), intent(in) :: texture_name
    integer(c_int) :: texture_id, status

    call texture_database%get(key(texture_name), texture_id, stat = status)

    if (status /= 0) then
      error stop colorize_rgb("[Texture] Error: ["//texture_name//"] does not exist.", 255, 0, 0)
    end if
  end function get_texture


  !* This is mainly used by the texture packer to get the dimensions of the texture.
  function texture_get_size(texture_name) result(texture_size)
    use :: terminal
    implicit none

    character(len = *, kind = c_char), intent(in) :: texture_name
    class(*), allocatable :: generic_data
    type(vec2i) :: texture_size
    integer(c_int) :: status

    call texture_size_database%get_raw(key(texture_name), generic_data, stat = status)

    if (status /= 0) then
      error stop colorize_rgb("[Texture] Error: ["//texture_name//"] does not exist.", 255, 0, 0)
    end if

    select type (generic_data)
     type is (vec2i)
      texture_size = generic_data
     class default
      error stop colorize_rgb("[Texture] Error: ["//texture_name//"] is the wrong type.", 255, 0, 0)
    end select
  end function texture_get_size


  !* Delete a texture. From OpenGL and the database.
  subroutine texture_delete(texture_name)
    use :: opengl
    use :: terminal
    implicit none

    character(len = *, kind = c_char), intent(in) :: texture_name
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
    implicit none

    character(len = *, kind = c_char), intent(in) :: texture_name
    integer(c_int) :: status

    call texture_database%check_key(key(texture_name), stat = status)

    existence = status == 0
  end function texture_exists


  !* Completely wipe out all existing textures. This might be slow.
  subroutine texture_clear_database()
    use :: fhash, only: fhash_iter_t, fhash_key_t
    use :: string
    use :: array, only: string_array
    use :: terminal
    implicit none

    type(string_array) :: key_array
    type(fhash_iter_t) :: iterator
    class(fhash_key_t), allocatable :: generic_key
    class(*), allocatable :: generic_placeholder
    integer(c_int) :: i
    integer(c_int) :: remaining_size

    !* We must check that there is anything in the database before we iterate.
    call texture_database%stats(num_items = remaining_size)
    if (remaining_size == 0) then
      print"(A)", "[Texture]: Database was empty. Nothing to do. Success!"
      return
    end if

    ! Start with a size of 0.
    allocate(key_array%data(0))

    ! Create the iterator.
    iterator = fhash_iter_t(texture_database)

    ! Now we will collect the keys from the iterator.
    do while(iterator%next(generic_key, generic_placeholder))
      ! Appending. Allocatable will clean up the old data.
      key_array%data = [key_array%data, heap_string(generic_key%to_string())]
    end do

    do i = 1,size(key_array%data)
      call texture_delete(key_array%data(i)%get())
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
