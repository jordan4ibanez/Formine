module texture
  use :: hashmap_str
  use :: vector_2i
  use :: texture_gc
  use, intrinsic :: iso_c_binding
  implicit none


  private

  public :: texture_module_initialize
  public :: texture_create
  public :: texture_create_from_memory
  public :: texture_use
  public :: texture_get_size
  public :: texture_delete
  public :: texture_exists
  public :: texture_clear_database


  !! fixme: these two can use GC.
  type(hashmap_string_key) :: texture_database
  type(hashmap_string_key) :: texture_size_database

  logical :: debug_mode = .false.


contains

  !* Initialize the module.
  subroutine texture_module_initialize()
    implicit none

    texture_database = new_hashmap_string_key(texture_database_gc)

    texture_size_database = new_hashmap_string_key()
  end subroutine texture_module_initialize


  !* Create a texture from a file path.
  subroutine texture_create(texture_file_path)
    use :: stb_image
    use :: string
    use :: opengl
    use :: terminal
    implicit none

    character(len = *, kind = c_char), intent(in) :: texture_file_path
    integer(c_int) :: width, height, channels
    integer(1), dimension(:), allocatable :: image_data
    character(len = :, kind = c_char), allocatable :: file_name

    ! We always want 4 channels.
    image_data = stbi_load(texture_file_path, width, height, channels, 4)

    if (width + height == 0) then
      error stop "[Texture] Error: Could not load texture. File specified from file path ["//texture_file_path//"] does not exist."
    end if

    ! This is done by the file name, we don't care about the path.
    file_name = string_get_file_name(texture_file_path)

    call texture_create_from_memory(file_name, image_data, width, height)
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
    integer(c_int), intent(in), value :: width, height
    integer(c_int), pointer :: texture_id

    ! First we must generate the texture ID.
    allocate(texture_id)
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
    integer(c_int) :: texture_id

    if (.not. get_texture(texture_name, texture_id)) then
      print"(A)", color_term("[Texture] Warning: Texture ["//texture_name//"] does not exist. Cannot use.", WARNING)
      return
    end if

    call gl_bind_texture(GL_TEXTURE_2D, texture_id)
  end subroutine texture_use


  !* Internal only. Set a texture in the database.
  subroutine set_texture(texture_name, new_texture, x, y)
    implicit none

    character(len = *, kind = c_char), intent(in) :: texture_name
    integer(c_int), intent(in) :: new_texture, x, y
    type(vec2i), pointer :: size

    ! This creates an enforcement where the texture must be deleted before it can be re-assigned.
    ! This prevents a severe memory leak.
    if (texture_database%has_key(texture_name)) then
      error stop "[Texture] Error: Tried to overwrite texture ["//texture_name//"]. Please delete it before setting it."
    end if

    if (debug_mode) then
      print"(A)", "[Texture]: set texture ["//texture_name//"]"
    end if

    call texture_database%set(texture_name, new_texture)

    allocate(size)
    size%x = x
    size%y = y
    call texture_size_database%set(texture_name, size)
  end subroutine set_texture


  !* Internal only. Get a texture from the database. Returns if it exists.
  function get_texture(texture_name, texture_id) result(exists)
    use :: terminal
    implicit none

    character(len = *, kind = c_char), intent(in) :: texture_name
    logical(c_bool) :: exists
    class(*), pointer :: generic_pointer
    integer(c_int) :: texture_id

    exists = .false.

    if (.not. texture_database%get(texture_name, generic_pointer)) then
      print"(A)",color_term("[Texture] Warning: ["//texture_name//"] does not exist.", WARNING)
      return
    end if

    select type (generic_pointer)
     type is (integer(c_int))
      texture_id = generic_pointer
     class default
      error stop color_term("[Texture] Error: ["//texture_name//"] is the wrong type.", ERROR)
    end select

    exists = .true.
  end function get_texture


  !* This is mainly used by the texture packer to get the dimensions of the texture.
  function texture_get_size(texture_name) result(texture_size)
    use :: terminal
    implicit none

    character(len = *, kind = c_char), intent(in) :: texture_name
    class(*), pointer :: generic_data
    type(vec2i) :: texture_size

    if (.not. texture_size_database%get(texture_name, generic_data)) then
      error stop color_term("[Texture] Error: ["//texture_name//"] does not exist.", ERROR)
    end if

    select type (generic_data)
     type is (vec2i)
      texture_size = generic_data
     class default
      error stop color_term("[Texture] Error: ["//texture_name//"] is the wrong type.", ERROR)
    end select
  end function texture_get_size


  !* Delete a texture. From OpenGL and the database.
  subroutine texture_delete(texture_name)
    implicit none

    character(len = *, kind = c_char), intent(in) :: texture_name

    ! Finally, remove it from the database.
    !? GC will take care of the OpenGL memory.
    call texture_database%delete(texture_name)
  end subroutine texture_delete


  !* Check if a texture exists.
  logical function texture_exists(texture_name) result(existence)
    implicit none

    character(len = *, kind = c_char), intent(in) :: texture_name

    existence = texture_database%has_key(texture_name)
  end function texture_exists


  !* Completely wipe out all existing textures. This might be slow.
  subroutine texture_clear_database()
    use :: string
    use :: array, only: string_array, array_string_insert
    use :: terminal
    implicit none

    integer(c_int64_t) :: remaining_size

    !* We must check that there is anything in the database before we iterate.
    remaining_size = texture_database%count()

    if (remaining_size == 0) then
      print"(A)", "[Texture]: Database was empty. Freeing C memory."
      call texture_database%free()
      return
    end if

    !* Delete the entire thing in one call. 8)
    call texture_database%clear()

    !* We will always check that the remaining size is 0. This will protect us from random issues.
    remaining_size = texture_database%count()

    if (remaining_size /= 0) then
      print"(A)", color_term("[Texture] Warning: Did not clear all textures! Expected size: [0] | Actual: ["//int64_to_string(remaining_size)//"]", WARNING)
    else
      print"(A)", "[Texture]: Successfully cleared the texture database."
    end if

    call texture_database%free()
    print"(A)", "[Texture]: Successfully freed the texture database C memory."
  end subroutine texture_clear_database


end module texture
