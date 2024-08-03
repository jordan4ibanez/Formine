module font
  use, intrinsic :: iso_c_binding
  implicit none

  !* I am a solo developer. I only use 1 font.
  !* In the future, if there is a desire for more than one font,
  !* we will build upon this.

  private


  public :: font_create

  integer :: character_width != 5
  integer :: character_height != 7
  integer :: spacing != 1

  ! Slots are the total size of a character, including the border.
  integer :: slot_width ! = character_width + spacing ! 6
  integer :: slot_height != character_height + spacing ! 8

  integer :: slots_horizontal != 9
  integer :: slots_vertical != 9


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

    !* We will assume that the only difference in png and the cfg is the file extension.

    font_data_file_name = get_file_name_from_string(font_texture_location)

    ! print*,font_texture_location

    font_config_location = string_remove_file_extension(font_texture_location)//".cfg"

    ! print*,texture_cfg_location

    ! print*, "reading font config"

    call process_font_configuration(font_config_location)

    ! print*,reader%file_string








    c_file_location = into_c_string(font_texture_location)

    ! We always want 4 channels.
    desired_channels = 4

    ! print*,"    REMEMBER TO USE A SPARE SLOT FOR UNDEFINED CHARACTERS"
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
    type(vec2i) :: testing

    call reader%read_lines(font_config_location)

    ! If it doesn't exist, we need a font to render text so stop.
    if (.not. reader%exists) then
      error stop "[Font] Error: Cannot read the font config in location ["//font_config_location//"]"
    end if

    do i = 1,5!reader%line_count
      temp_buffer = reader%lines(i)%get()

      ! This is a real half assed way to do this but who cares?
      temp_buffer_len = len(temp_buffer)

      if (temp_buffer_len == 0) then
        continue
      end if

      ! We want to avoid a buffer overflow.
      if (temp_buffer_len >= 19 .and. temp_buffer(1:19) == "SLOTS_HORIZONTAL = ") then
        ! Cut the buffer and read it into the integer.
        temp_buffer = temp_buffer(19:len(temp_buffer))
        read(temp_buffer, '(i4)') slots_horizontal
        if (slots_horizontal == 0) then
          error stop "[Font] Error: Impossible SLOTS_HORIZONTAL value on line ["//int_to_string(i)//"] of font config ["//font_config_location//"]"
        end if

      else if (temp_buffer_len >= 17 .and. temp_buffer(1:17) == "SLOTS_VERTICAL = ") then
        ! Cut the buffer and read it into the integer.
        temp_buffer = temp_buffer(17:len(temp_buffer))
        read(temp_buffer, '(i4)') slots_vertical
        if (slots_vertical == 0) then
          error stop "[Font] Error: Impossible SLOTS_VERTICAL value on line ["//int_to_string(i)//"] of font config ["//font_config_location//"]"
        end if

      else if (temp_buffer_len >= 10 .and. temp_buffer(1:10) == "SPACING = ") then
        ! Cut the buffer and read it into the integer.
        temp_buffer = temp_buffer(10:len(temp_buffer))
        read(temp_buffer, '(i4)') spacing
        if (spacing == 0) then
          error stop "[Font] Error: Impossible SPACING value on line ["//int_to_string(i)//"] of font config ["//font_config_location//"]"
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

          print"(A)","["//temp_buffer//"]"

          ! Now we're going to chop up the X and Y out of the line.
          comma_location = index(temp_buffer, ",")

          ! There is a formatting error.
          if (comma_location == 0) then
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

          testing = vec2i(x_location, y_location)

        end if
      end if

    end do


  end subroutine process_font_configuration





end module font
