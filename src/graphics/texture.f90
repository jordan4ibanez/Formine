module texture
  implicit none


  private


  public :: texture_create


contains

  subroutine texture_create(texture_location)
    use :: stb_image
    use :: string
    use :: opengl
    use :: iso_c_binding
    use :: terminal
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

    ! Now we can assign it into the database.
    ! todo: the thing
  end subroutine texture_create


end module texture
