module font
  use, intrinsic :: iso_c_binding
  use :: fhash, only: fhash_tbl_t, key => fhash_key
  implicit none

  !* I am a solo developer. I only use 1 font.
  !* In the future, if there is a desire for more than one font,
  !* we will build upon this.

  private


  public :: font_create


  ! The size of each character in pixels.
  integer :: character_width = 0
  integer :: character_height = 0

  ! Spacing between each character in pixels.
  integer :: spacing = 0

  ! Slots are the total size of a character, including the border, in pixels.
  integer :: slot_width = 0
  integer :: slot_height = 0

  ! How many characters X and Y in total.
  integer :: slots_horizontal = 0
  integer :: slots_vertical = 0

  integer :: font_texture_width = 0
  integer :: font_texture_height = 0

  type(fhash_tbl_t) :: character_database_integral


contains


  subroutine font_create(font_texture_location)
    use :: stb_image
    use :: string
    use :: files
    implicit none

    character(len = *, kind = c_char), intent(in) :: font_texture_location
    integer :: x, y, channels, desired_channels
    character(len = :, kind = c_char), allocatable :: c_file_location
    character(len = :), allocatable :: font_data_file_name
    character(len = :), allocatable :: font_config_location
    integer(1), dimension(:), allocatable :: raw_image_data

    ! print*,"    REMEMBER TO USE A SPARE SLOT FOR UNDEFINED CHARACTERS"

    !* We will assume that the only difference in png and the cfg is the file extension.

    font_data_file_name = get_file_name_from_string(font_texture_location)
    font_config_location = string_remove_file_extension(font_texture_location)//".cfg"

    ! This is quite a large and complex subroutine. It's getting all the data from the .cfg file.
    call process_font_configuration(font_config_location)

    ! Now load up the font.
    c_file_location = into_c_string(font_texture_location)

    ! We always want 4 channels.
    desired_channels = 4

    raw_image_data = stbi_load(font_texture_location, x, y, channels, desired_channels)

    ! Let's check if the configuration file is correct.
    if (x /= font_texture_width) then
      error stop "[Font] Error: Font sizing in configuration file is wrong on X axis. Expected: ["//int_to_string(font_texture_width)//"] | received: ["//int_to_string(x)//"]"
    else if (y /= font_texture_height) then
      error stop "[Font] Error: Font sizing in configuration file is wrong on Y axis. Expected: ["//int_to_string(font_texture_height)//"] | received: ["//int_to_string(y)//"]"
    end if

    ! Now, we will bake in the OpenGL texture coordinates into the double floating point database.



  end subroutine font_create


  subroutine process_font_configuration(font_config_location)
    use :: string
    use :: files
    use :: vector_2i
    implicit none

    character(len = *, kind = c_char), intent(in) :: font_config_location
    type(file_reader) :: reader
    integer :: i, temp_buffer_len, comma_location, x_location, y_location
    character(len = :), allocatable :: temp_buffer
    character :: current_character
    character(len = :), allocatable :: x_str, y_str

    call reader%read_lines(font_config_location)

    ! If it doesn't exist, we need a font to render text so stop.
    if (.not. reader%exists) then
      error stop "[Font] Error: Cannot read the font config in location ["//font_config_location//"]"
    end if

    do i = 1,reader%line_count
      temp_buffer = reader%lines(i)%get()

      ! This is a real half assed way to do this but who cares?
      temp_buffer_len = len(temp_buffer)

      if (temp_buffer_len <= 0) then
        continue
      end if

      ! We want to avoid a buffer overflow.
      if (temp_buffer_len >= 19 .and. temp_buffer(1:19) == "SLOTS_HORIZONTAL = ") then
        ! Cut the buffer and read it into the integer.
        temp_buffer = temp_buffer(19:len(temp_buffer))
        read(temp_buffer, '(i4)') slots_horizontal
        if (slots_horizontal <= 0) then
          error stop "[Font] Error: Impossible SLOTS_HORIZONTAL value on line ["//int_to_string(i)//"] of font config ["//font_config_location//"]"
        end if

      else if (temp_buffer_len >= 17 .and. temp_buffer(1:17) == "SLOTS_VERTICAL = ") then
        ! Cut the buffer and read it into the integer.
        temp_buffer = temp_buffer(17:len(temp_buffer))
        read(temp_buffer, '(i4)') slots_vertical
        if (slots_vertical <= 0) then
          error stop "[Font] Error: Impossible SLOTS_VERTICAL value on line ["//int_to_string(i)//"] of font config ["//font_config_location//"]"
        end if

      else if (temp_buffer_len >= 10 .and. temp_buffer(1:10) == "SPACING = ") then
        ! Cut the buffer and read it into the integer.
        temp_buffer = temp_buffer(10:len(temp_buffer))
        read(temp_buffer, '(i4)') spacing
        if (spacing <= 0) then
          error stop "[Font] Error: Impossible SPACING value on line ["//int_to_string(i)//"] of font config ["//font_config_location//"]"
        end if

      else if (temp_buffer_len >= 13 .and. temp_buffer(1:13) == "CHAR_WIDTH = ") then
        ! Cut the buffer and read it into the integer.
        temp_buffer = temp_buffer(13:len(temp_buffer))
        read(temp_buffer, '(i4)') character_width
        if (character_width <= 0) then
          error stop "[Font] Error: Impossible CHAR_WIDTH value on line ["//int_to_string(i)//"] of font config ["//font_config_location//"]"
        end if

      else if (temp_buffer_len >= 14 .and. temp_buffer(1:14) == "CHAR_HEIGHT = ") then
        ! Cut the buffer and read it into the integer.
        temp_buffer = temp_buffer(14:len(temp_buffer))
        read(temp_buffer, '(i4)') character_height
        if (character_height <= 0) then
          error stop "[Font] Error: Impossible CHAR_HEIGHT value on line ["//int_to_string(i)//"] of font config ["//font_config_location//"]"
        end if

      else if (temp_buffer_len >= 7) then
        ! This is a real rigid way to do this.
        ! This is basically, it has to be formatted like:
        ! [A = ]
        ! Minus the brackets.
        if (temp_buffer(1:1) /= " " .and. temp_buffer(2:4) == " = ") then
          ! print*,temp_buffer
          current_character = temp_buffer(1:1)
          ! print*,current_character

          ! Now we're going to cut the temp buffer.
          temp_buffer = temp_buffer(5:len(temp_buffer))

          ! Now we're going to chop up the X and Y out of the line.
          comma_location = index(temp_buffer, ",")

          ! There is a formatting error.
          if (comma_location <= 0) then
            error stop "[Font] Error: There is a missing comma on line ["//int_to_string(i)//"] of font config ["//font_config_location//"]"
          end if

          ! Get the X into an integer.
          x_str = temp_buffer(1:comma_location - 1)
          read(x_str, '(i4)') x_location
          if (x_location <= 0) then
            error stop "[Font] Error: Impossible X value on line ["//int_to_string(i)//"] of font config ["//font_config_location//"]"
          end if

          ! Get the Y into an integer.
          y_str = temp_buffer(comma_location + 1:len(temp_buffer))
          read(y_str, '(i4)') y_location
          if (y_location <= 0) then
            error stop "[Font] Error: Impossible Y value on line ["//int_to_string(i)//"] of font config ["//font_config_location//"]"
          end if

          ! Now finally, dump the integral position into the database.
          call character_database_integral%set(key(current_character), vec2i(x_location, y_location))
        end if
      end if
    end do

    ! Check everything to make sure nothing blew up.
    if (slots_horizontal <= 0) then
      error stop "[Font] Error: SLOTS_HORIZONTAL was not set."
    else if (slots_vertical <= 0) then
      error stop "[Font] Error: SLOTS_VERTICAL was not set."
    else if (character_width <= 0) then
      error stop "[Font] Error: CHAR_WIDTH was not set."
    else if (character_height <= 0) then
      error stop "[Font] Error: CHAR_HEIGHT was not set."
    else if (spacing <= 0) then
      error stop "[Font] Error: SPACING was not set."
    end if

    ! Finally, set the slot sizes.
    slot_width = character_width + spacing
    slot_height = character_height + spacing

    ! Check that as well because this subroutine is already huge.
    if (slot_width <= 0) then
      error stop "[Font] Error: slot_width calculation error."
    else if (slot_height <= 0) then
      error stop "[Font] Error: slot_height calculation error."
    end if

    ! Now we need a base to ensure that our texture size is the same as our calculation
    font_texture_width = slot_width * slots_horizontal
    font_texture_height = slot_height * slots_vertical
  end subroutine process_font_configuration


  subroutine calculate_opengl_texture_coordinates()
    implicit none

  end subroutine calculate_opengl_texture_coordinates




end module font
