module font
  use, intrinsic :: iso_c_binding
  use :: fhash, only: fhash_tbl_t, key => fhash_key
  use :: vector_2d
  implicit none

  !* I am a solo developer. I only use 1 font.
  !* In the future, if there is a desire for more than one font,
  !* we will build upon this.

  private


  public :: font_create


  ! This is a container which holds the points on the texture that make the character appear correctly.
  type opengl_character
    ! We use this for constructing the quad to match the size of the texture mapping.
    real(c_double) :: width
    ! Counter clock-wise.
    type(vec2d) :: top_left
    type(vec2d) :: bottom_left
    type(vec2d) :: bottom_right
    type(vec2d) :: top_right
  end type opengl_character

  ! This is an abstraction over the raw array components to allow me to actually
  ! read what I'm doing during the subroutine that checks how wide characters are.
  type rgba
    integer(c_int) :: r, g, b, a = 0
  end type rgba


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

  ! Todo: Move this into the subroutine, and pass it around.
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
    call calculate_opengl_texture_coordinates(raw_image_data)

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


  subroutine calculate_opengl_texture_coordinates(raw_image_data)
    use :: math_helpers
    use, intrinsic :: iso_c_binding
    use :: fhash, only: fhash_iter_t, fhash_key_t
    use :: vector_2i
    implicit none

    integer(1), dimension(:), intent(in) :: raw_image_data
    integer(c_int), dimension(:), allocatable :: integral_image_data
    type(fhash_iter_t) :: iterator
    class(fhash_key_t), allocatable :: generic_key
    class(*), allocatable :: generic_data
    type(vec2i) :: position
    integer :: pixel_x, pixel_y
    type(rgba) :: pixel_color
    type(opengl_character) :: gpu_character
    type(vec2d) :: debugging

    ! Shift this into a format we can use.
    integral_image_data = c_uchar_to_int_array(raw_image_data)

    ! Iterate integral character position.
    iterator = fhash_iter_t(character_database_integral)
    do while(iterator%next(generic_key, generic_data))
      ! print*,generic_key%to_string()

      ! Enforce that we are running with a vec2i.
      select type(generic_data)
       type is (vec2i)
        position = generic_data
       class default
        error stop "[Font] Error: The wrong type got inserted for character ["//generic_key%to_string()//"]"
      end select

      ! So now that we have the position of the character in the texture, let's calculate some basic elements.
      ! I could have made this an array for super speed, but I want to be able to understand it in the future.
      print*,generic_key%to_string()
      ! print*,position

      ! We put our initial brush position at the top left of the character.
      pixel_x = ((position%x - 1) * slot_width) + 1
      pixel_y = ((position%y - 1) * slot_height) + 1

      ! print*,pixel_x, pixel_y

      ! print*,integral_image_data(position_to_index(pixel_x, pixel_y))
      ! print*,position_to_index(pixel_x, pixel_y)

      !! Remember: to get the right side and bottom, you need to overshoot by 1 pixel !!

      pixel_color = get_color(1, 1)

      print*,pixel_x, pixel_y

      debugging = pixel_position_to_opengl_position(pixel_x, pixel_y)

      ! gpu_character = calculate_opengl_texture_coordinates()
      print*,debugging



      exit

    end do
    ! print*, integral_image_data

  contains


    !* We need to do such complex work we need this subroutine to have functions.

    function pixel_position_to_opengl_position(x,y) result(pos)
      implicit none

      integer(c_int), intent(in), value :: x, y
      type(vec2d) :: pos
      real(c_double) :: x_f64, y_f64, canvas_width_f64, canvas_height_f64

      ! We want the top left of the pixel. Shift into 0 indexed.
      x_f64 = real(x - 1, kind = c_double)
      y_f64 = real(y - 1, kind = c_double)

      canvas_width_f64 = real(font_texture_width, kind = c_double)
      canvas_height_f64 = real(font_texture_height, kind = c_double)

      ! A 0.0 - 1.0 range is the goal.
      pos = vec2d(x_f64 / canvas_width_f64, y_f64 / canvas_height_f64)
    end function pixel_position_to_opengl_position


    !* This wraps a chain of functions to just get the data we need, which is RGBA of a pixel.
    function get_color(x,y) result(color)
      implicit none

      integer(c_int), intent(in), value :: x, y
      type(rgba) :: color

      color = index_get_color(position_to_index(x,y))
    end function get_color


    !* Position (in pixels) to the index in the texture array.
    function position_to_index(x, y) result(i)
      implicit none

      integer(c_int), intent(in), value :: x, y
      integer(c_int) :: a, b
      integer(c_int) :: i

      ! Shift into 0 indexed, because math.
      a = x - 1
      b = y - 1

      ! Times 4 because we have 4 individual channels we're hopping over.
      ! Plus 1 because we're shifting back into 1 indexed.
      i = (((b * font_texture_width) + a) * 4) + 1
    end function position_to_index


    !* Get the RGBA of an index.
    function index_get_color(index) result(color)
      implicit none

      integer(c_int), intent(in), value :: index
      type(rgba) :: color

      color%r = integral_image_data(index)
      color%g = integral_image_data(index + 1)
      color%b = integral_image_data(index + 2)
      color%a = integral_image_data(index + 3)
    end function index_get_color


  end subroutine calculate_opengl_texture_coordinates


end module font
