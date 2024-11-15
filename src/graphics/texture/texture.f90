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
  public :: texture_destroy_database


  !* Type: integer(c_int)
  type(hashmap_string_key) :: texture_database
  !* Type: vec2i
  type(hashmap_string_key) :: texture_size_database


  logical, parameter :: debug_mode = .false.


contains


  !* Initialize the module.
  subroutine texture_module_initialize()
    implicit none

    texture_database = new_hashmap_string_key(sizeof(1), texture_database_gc)

    texture_size_database = new_hashmap_string_key(sizeof(vec2i(0,0)))
  end subroutine texture_module_initialize


  !* Create a texture from a file path.
  subroutine texture_create(texture_file_path)
    use :: stb_image
    use :: string_f90
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
    use :: string_f90
    use :: opengl
    use :: terminal
    implicit none

    character(len = *, kind = c_char), intent(in) :: texture_name
    integer(1), dimension(:), intent(in) :: raw_data
    integer(c_int), intent(in), value :: width, height
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
    end if

    if (debug_mode) then
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
      call print_color(WARNING, "[Texture] Warning: Texture ["//texture_name//"] does not exist. Cannot use.")
      return
    end if

    call gl_bind_texture(GL_TEXTURE_2D, texture_id)
  end subroutine texture_use


  !* Internal only. Set a texture in the database.
  subroutine set_texture(texture_name, vao_id, x, y)
    implicit none

    character(len = *, kind = c_char), intent(in) :: texture_name
    integer(c_int), intent(in), value :: vao_id
    integer(c_int), intent(in), value :: x, y
    type(vec2i) :: size

    ! This creates an enforcement where the texture must be deleted before it can be re-assigned.
    ! This prevents a severe memory leak.
    if (texture_database%has_key(texture_name)) then
      error stop "[Texture] Error: Tried to overwrite texture ["//texture_name//"]. Please delete it before setting it."
    end if

    if (debug_mode) then
      print"(A)", "[Texture]: set texture ["//texture_name//"]"
    end if

    call texture_database%set(texture_name, vao_id)

    size%x = x
    size%y = y

    call texture_size_database%set(texture_name, size)
  end subroutine set_texture


  !* Internal only. Get a texture from the database. Returns if it exists.
  function get_texture(texture_name, texture_id) result(exists)
    use :: terminal
    implicit none

    character(len = *, kind = c_char), intent(in) :: texture_name
    integer(c_int), intent(inout) :: texture_id
    logical(c_bool) :: exists
    type(c_ptr) :: raw_c_ptr
    integer(c_int), pointer :: texture_id_pointer

    exists = .false.

    if (.not. texture_database%get(texture_name, raw_c_ptr)) then
      call print_color(WARNING, "[Texture] Warning: ["//texture_name//"] does not exist.")
      return
    end if

    call c_f_pointer(raw_c_ptr, texture_id_pointer)
    texture_id = texture_id_pointer

    exists = .true.
  end function get_texture


  !* This is mainly used by the texture packer to get the dimensions of the texture.
  function texture_get_size(texture_name, texture_size) result(exists)
    use :: terminal
    implicit none

    character(len = *, kind = c_char), intent(in) :: texture_name
    type(vec2i), intent(inout) :: texture_size
    logical(c_bool) :: exists
    type(c_ptr) :: raw_c_ptr
    type(vec2i), pointer :: texture_size_pointer

    exists = .false.

    if (.not. texture_size_database%get(texture_name, raw_c_ptr)) then
      error stop color_term("[Texture] Error: ["//texture_name//"] size does not exist.", ERROR)
    end if

    call c_f_pointer(raw_c_ptr, texture_size_pointer)
    texture_size = texture_size_pointer

    exists = .true.
  end function texture_get_size


  !* Delete a texture. From OpenGL and the database.
  subroutine texture_delete(texture_name)
    implicit none

    character(len = *, kind = c_char), intent(in) :: texture_name

    call texture_database%remove(texture_name)
  end subroutine texture_delete


  !* Check if a texture exists.
  logical function texture_exists(texture_name) result(existence)
    implicit none

    character(len = *, kind = c_char), intent(in) :: texture_name

    existence = texture_database%has_key(texture_name)
  end function texture_exists


  !* Completely wipe out all existing textures. This might be slow.
  subroutine texture_destroy_database()
    use :: string_f90
    use :: terminal
    implicit none

    call texture_database%destroy()
    print"(A)", "[Texture]: Successfully freed the texture database C memory."

    call texture_size_database%destroy()
    print"(A)", "[Texture]: Successfully freed the texture size database C memory."
  end subroutine texture_destroy_database


end module texture
