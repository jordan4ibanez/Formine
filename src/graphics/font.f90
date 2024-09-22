module font
  use :: vector_2d
  use :: memory_texture_module
  use :: hashmap_str
  use, intrinsic :: iso_c_binding
  implicit none

  !* I am a solo developer. I only use 1 font.
  !* In the future, if there is a desire for more than one font,
  !* we will build upon this.

  private


  public :: font_create
  public :: font_generate_text
  public :: font_clear_database


  ! This is a container which holds the points on the texture that make the character appear correctly.
  type :: opengl_character
    ! We use this for constructing the quad to match the size of the texture mapping.
    integer(c_int) :: width_pixels = 0
    real(c_double) :: width_real = 0.0d0
    ! Counter clock-wise.
    type(vec2d) :: top_left
    type(vec2d) :: bottom_left
    type(vec2d) :: bottom_right
    type(vec2d) :: top_right
  end type opengl_character


  ! The size of each character in pixels.
  integer(c_int) :: character_width = 0
  integer(c_int) :: character_height = 0

  ! Spacing between each character in pixels.
  integer(c_int) :: spacing = 0

  ! Slots are the total size of a character, including the border, in pixels.
  integer(c_int) :: slot_width = 0
  integer(c_int) :: slot_height = 0

  ! How many characters X and Y in total.
  integer(c_int) :: slots_horizontal = 0
  integer(c_int) :: slots_vertical = 0

  integer(c_int) :: font_texture_width = 0
  integer(c_int) :: font_texture_height = 0

  !! fixme:? this maybe could use a gc, not sure why though.
  !! todo: look into this
  type(hashmap_string_key) :: character_database

contains


  !* Generate a text mesh.
  subroutine font_generate_text(mesh_name, font_size, text, r,g,b, center, size)
    use :: mesh
    use :: string, only: string_get_non_space_characters
    use :: vector_2f
    implicit none

    character(len = *, kind = c_char), intent(in) :: mesh_name, text
    real(c_float), intent(in), value :: font_size
    real(c_float), intent(in), optional :: r,g,b
    logical, intent(in), optional :: center
    type(vec2f), intent(inout), optional :: size
    logical :: should_center
    real(c_float) :: red, green, blue
    real(c_float), dimension(:), allocatable :: positions, texture_coordinates, colors
    integer(c_int), dimension(:), allocatable :: indices
    integer(c_int) :: text_length, allocation_length, i, buffer_index, current_positions_offset, current_texture_coordinates_offset, current_colors_offset, current_indices_offset, current_indices_index
    character :: current_character
    type(opengl_character), pointer :: character_data_pointer
    logical :: exists
    real(c_float) :: current_scroll_right, actual_character_width, centering_offset
    integer, parameter :: points = 4
    real(c_float), parameter :: space_width = 0.4
    integer, parameter :: dimensions = 2
    integer, parameter :: stride = dimensions * points

    if (.not. present(r)) then
      red = 0.0
    else
      red = r
    end if

    if (.not. present(g)) then
      green = 0.0
    else
      green = g
    end if

    if (.not. present(b)) then
      blue = 0.0
    else
      blue = b
    end if

    if (.not. present(center)) then
      should_center = .false.
    else
      should_center = center
    end if

    current_scroll_right = 0.0

    text_length = len(text)
    allocation_length = string_get_non_space_characters(text)

    ! Per character: 1 quad. 4 positions. 3 positions per tri. 2 tris. This can probably be optimized, somehow.
    allocate(positions(allocation_length * stride))
    allocate(texture_coordinates(allocation_length * 8))
    allocate(colors(allocation_length * 12))
    allocate(indices(allocation_length * 6))

    ! With spaces, this desynchronizes. We must stop it from trying to reach outside the buffer.
    buffer_index = 0

    do i = 1,text_length

      current_character = text(i:i)

      if (current_character == " ") then
        current_scroll_right = current_scroll_right + (font_size * space_width)
        cycle
      end if

      buffer_index = buffer_index + 1

      character_data_pointer => get_character(current_character, exists)

      ! For now, we're just going to skip characters that don't exist.
      ! todo: use a special character that is a square box or something as a replacement.
      if (exists) then

        ! Positions.
        current_positions_offset = ((buffer_index - 1) * stride) + 1
        actual_character_width = real(character_data_pointer%width_real, kind = c_float) * font_size

        positions(current_positions_offset    ) = current_scroll_right
        positions(current_positions_offset + 1) = font_size

        positions(current_positions_offset + 2) = current_scroll_right
        positions(current_positions_offset + 3) = 0.0

        positions(current_positions_offset + 4) = current_scroll_right + actual_character_width
        positions(current_positions_offset + 5) = 0.0

        positions(current_positions_offset + 6) = current_scroll_right + actual_character_width
        positions(current_positions_offset + 7) = font_size

        current_scroll_right = current_scroll_right + actual_character_width + (font_size * 0.1)

        ! Texture coordinates.

        current_texture_coordinates_offset = ((buffer_index - 1) * 8) + 1
        texture_coordinates(current_texture_coordinates_offset    ) = character_data_pointer%top_left%x_f32()
        texture_coordinates(current_texture_coordinates_offset + 1) = character_data_pointer%top_left%y_f32()
        texture_coordinates(current_texture_coordinates_offset + 2) = character_data_pointer%bottom_left%x_f32()
        texture_coordinates(current_texture_coordinates_offset + 3) = character_data_pointer%bottom_left%y_f32()
        texture_coordinates(current_texture_coordinates_offset + 4) = character_data_pointer%bottom_right%x_f32()
        texture_coordinates(current_texture_coordinates_offset + 5) = character_data_pointer%bottom_right%y_f32()
        texture_coordinates(current_texture_coordinates_offset + 6) = character_data_pointer%top_right%x_f32()
        texture_coordinates(current_texture_coordinates_offset + 7) = character_data_pointer%top_right%y_f32()

        ! Colors.
        current_colors_offset = ((buffer_index - 1) * 12) + 1
        colors(current_colors_offset    ) = red
        colors(current_colors_offset + 1) = green
        colors(current_colors_offset + 2) = blue

        colors(current_colors_offset + 3) = red
        colors(current_colors_offset + 4) = green
        colors(current_colors_offset + 5) = blue

        colors(current_colors_offset + 6) = red
        colors(current_colors_offset + 7) = green
        colors(current_colors_offset + 8) = blue

        colors(current_colors_offset + 9) = red
        colors(current_colors_offset + 10) = green
        colors(current_colors_offset + 11) = blue

        ! Indices.
        current_indices_offset = ((buffer_index - 1) * 6) + 1
        current_indices_index = (buffer_index - 1) * 4

        indices(current_indices_offset    ) = 0 + current_indices_index
        indices(current_indices_offset + 1) = 1 + current_indices_index
        indices(current_indices_offset + 2) = 2 + current_indices_index
        indices(current_indices_offset + 3) = 2 + current_indices_index
        indices(current_indices_offset + 4) = 3 + current_indices_index
        indices(current_indices_offset + 5) = 0 + current_indices_index
      end if
    end do

    if (present(size)) then
      size%x = (current_scroll_right - (font_size * 0.1))
      size%y = font_size
    end if

    if (should_center) then
      do i = 1,allocation_length
        current_positions_offset = ((i - 1) * stride) + 1

        centering_offset = (current_scroll_right - (font_size * 0.1)) * 0.5

        positions(current_positions_offset    ) = positions(current_positions_offset    ) - centering_offset
        positions(current_positions_offset + 2) = positions(current_positions_offset + 2) - centering_offset
        positions(current_positions_offset + 4) = positions(current_positions_offset + 4) - centering_offset
        positions(current_positions_offset + 6) = positions(current_positions_offset + 6) - centering_offset
      end do
    end if

    call mesh_create_2d(mesh_name, positions, texture_coordinates, colors, indices)
  end subroutine font_generate_text


  !* Get a character's OpenGL data.
  function get_character(char, exists) result(gl_char_information_pointer)
    use :: terminal
    implicit none

    character, intent(in) :: char
    logical, intent(inout) :: exists
    type(opengl_character), pointer :: gl_char_information_pointer
    class(*), pointer :: generic_pointer
    integer(c_int) :: status

    exists = .false.

    ! We will have a special handler to use a generic character for characters that aren't registered.
    if (.not. character_database%get(char, generic_pointer)) then
      return
    end if

    select type(generic_pointer)
     type is (opengl_character)
      exists = .true.
      gl_char_information_pointer => generic_pointer
     class default
      error stop colorize_rgb("[Font] Error: Character ["//char//"] has the wrong type.", 255, 0, 0)
    end select
  end function get_character


  !* Create a font from a png and a config using one file path.
  !* It will be assumed that the only difference between the texture and the
  !* config file will be the file extension.
  subroutine font_create(font_texture_file_path)
    use :: stb_image
    use :: string
    use :: files
    use :: texture
    implicit none

    character(len = *, kind = c_char), intent(in) :: font_texture_file_path
    integer(c_int) :: image_width, image_height, channels, desired_channels
    character(len = :), allocatable :: font_data_file_name
    character(len = :), allocatable :: font_config_file_path
    integer(1), dimension(:), allocatable :: raw_image_data
    type(hashmap_string_key) :: character_database_integral

    ! print*,"    REMEMBER TO USE A SPARE SLOT FOR UNDEFINED CHARACTERS"

    !* We will assume that the only difference in png and the cfg is the file extension.

    font_data_file_name = string_get_file_name(font_texture_file_path)
    font_config_file_path = string_remove_file_extension(font_texture_file_path)//".cfg"

    ! This is quite a large and complex subroutine. It's getting all the data from the .cfg file.
    call process_font_configuration(font_config_file_path, character_database_integral)

    ! Now load up the font.

    ! We always want 4 channels.
    desired_channels = 4

    raw_image_data = stbi_load(font_texture_file_path, image_width, image_height, channels, desired_channels)

    ! Let's check if the configuration file is correct.
    if (image_width /= font_texture_width) then
      error stop "[Font] Error: Font sizing in configuration file is wrong on X axis. Expected: ["//int_to_string(font_texture_width)//"] | received: ["//int_to_string(image_width)//"]"
    else if (image_height /= font_texture_height) then
      error stop "[Font] Error: Font sizing in configuration file is wrong on Y axis. Expected: ["//int_to_string(font_texture_height)//"] | received: ["//int_to_string(image_height)//"]"
    end if

    ! Now, we will bake in the OpenGL texture coordinates into the double floating point database.
    call calculate_opengl_texture_coordinates(raw_image_data, image_width, image_height, character_database_integral)

    ! Then we can finally upload it into the texture database.
    call texture_create_from_memory("font", raw_image_data, image_width, image_height)
  end subroutine font_create


  !* Very simple configuration file processing.
  subroutine process_font_configuration(font_config_file_path, character_database_integral)
    use :: string
    use :: files
    use :: vector_2i
    implicit none

    character(len = *, kind = c_char), intent(in) :: font_config_file_path
    type(hashmap_string_key), intent(inout) :: character_database_integral
    type(file_reader) :: reader
    integer(c_int) :: i, temp_buffer_length, x_index, y_index
    character(len = :), allocatable :: current_character, temp_buffer
    character(len = :), allocatable :: x_str, y_str


    call reader%read_lines(font_config_file_path)

    ! If it doesn't exist, we need a font to render text so stop.
    if (.not. reader%exists) then
      error stop "[Font] Error: Cannot read the font config in file path ["//font_config_file_path//"]"
    end if

    do i = 1,reader%line_count
      temp_buffer = reader%lines(i)%get()

      ! This is a real sloppy way to do this but who cares?
      temp_buffer_length = len(temp_buffer)

      if (temp_buffer_length <= 0) then
        cycle
      end if

      if (string_starts_with(temp_buffer, "SLOTS_HORIZONTAL = ")) then
        temp_buffer = string_get_right_of_character(temp_buffer, "=")
        slots_horizontal = string_to_int(temp_buffer)
        if (slots_horizontal <= 0) then
          error stop "[Font] Error: Impossible SLOTS_HORIZONTAL value on line ["//int_to_string(i)//"] of font config ["//font_config_file_path//"]"
        end if

      else if (string_starts_with(temp_buffer, "SLOTS_VERTICAL = ")) then
        temp_buffer = string_get_right_of_character(temp_buffer, "=")
        slots_vertical = string_to_int(temp_buffer)
        if (slots_vertical <= 0) then
          error stop "[Font] Error: Impossible SLOTS_VERTICAL value on line ["//int_to_string(i)//"] of font config ["//font_config_file_path//"]"
        end if

      else if (string_starts_with(temp_buffer, "SPACING = ")) then
        temp_buffer = string_get_right_of_character(temp_buffer, "=")
        spacing = string_to_int(temp_buffer)
        if (spacing <= 0) then
          error stop "[Font] Error: Impossible SPACING value on line ["//int_to_string(i)//"] of font config ["//font_config_file_path//"]"
        end if

      else if (string_starts_with(temp_buffer, "CHAR_WIDTH = ")) then
        temp_buffer = string_get_right_of_character(temp_buffer, "=")
        character_width = string_to_int(temp_buffer)
        if (character_width <= 0) then
          error stop "[Font] Error: Impossible CHAR_WIDTH value on line ["//int_to_string(i)//"] of font config ["//font_config_file_path//"]"
        end if

      else if (string_starts_with(temp_buffer, "CHAR_HEIGHT = ")) then
        temp_buffer = string_get_right_of_character(temp_buffer, "=")
        character_height = string_to_int(temp_buffer)
        if (character_height <= 0) then
          error stop "[Font] Error: Impossible CHAR_HEIGHT value on line ["//int_to_string(i)//"] of font config ["//font_config_file_path//"]"
        end if

      else
        ! This is a real rigid way to do this.
        ! This is basically, it has to be formatted like:
        ! [A = ]
        ! Minus the brackets.

        ! This is a comment.
        if (string_starts_with(temp_buffer, "#")) then
          cycle
        end if

        ! Extract the data between the brackets.
        if (.not. string_starts_with(temp_buffer, "[") .or. .not. string_ends_with(temp_buffer, "]")) then
          error stop "[Font] Error: Line ["//int_to_string(i)//"] is missing brackets []."
        end if

        temp_buffer = string_get_right_of_character(temp_buffer, "[")
        temp_buffer = string_get_left_of_character(temp_buffer, "]")

        ! Get the character key.
        current_character = string_get_left_of_character(temp_buffer, "=")

        if (len(current_character) /= 1) then
          error stop "[Font] Error: The key on line ["//int_to_string(i)//"] must be a character."
        end if

        ! Get the tuple value.
        temp_buffer = string_get_right_of_character(temp_buffer, "=")

        ! Make sure it has a comma.
        if (.not. string_contains_character(temp_buffer, ",")) then
          error stop "[Font] Error: There is a missing comma on line ["//int_to_string(i)//"] of font config ["//font_config_file_path//"]"
        end if

        ! First value.
        x_str = string_get_left_of_character(temp_buffer, ",")
        x_index = string_to_int(x_str)
        if (x_index <= 0) then
          error stop "[Font] Error: Impossible X value on line ["//int_to_string(i)//"] of font config ["//font_config_file_path//"]"
        end if

        ! Second value.
        y_str = string_get_right_of_character(temp_buffer, ",")
        y_index = string_to_int(y_str)
        if (y_index <= 0) then
          error stop "[Font] Error: Impossible Y value on line ["//int_to_string(i)//"] of font config ["//font_config_file_path//"]"
        end if

        ! Now finally, dump the integral position into the database.
        call character_database_integral%set(current_character, vec2i(x_index, y_index))
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


  !* Automates converting the pixel positions into OpenGL texture mapping positions.
  subroutine calculate_opengl_texture_coordinates(raw_image_data, image_width, image_height, character_database_integral)
    use :: math_helpers
    use, intrinsic :: iso_c_binding
    use :: vector_2i
    implicit none

    integer(1), dimension(:), intent(in) :: raw_image_data
    integer(c_int), intent(in), value :: image_width, image_height
    type(hashmap_string_key), intent(in) :: character_database_integral
    type(memory_texture) :: rgba8_texture_data
    character(len = :, kind = c_char), pointer :: string_key
    class(*), pointer :: generic_pointer
    type(vec2i) :: position
    integer(c_int) :: pixel_x, pixel_y
    type(opengl_character), pointer :: gpu_character
    integer(c_int64_t) :: i

    ! Shift this into a format we can use.
    rgba8_texture_data = memory_texture(raw_image_data, image_width, image_height)

    i = 0

    do while(character_database_integral%iterate_kv(i, string_key, generic_pointer))

      ! Enforce that we are running with a vec2i.
      select type(generic_pointer)
       type is (vec2i)
        position = generic_pointer
       class default
        error stop "[Font] Error: The wrong type got inserted for character ["//string_key//"]"
      end select

      ! So now that we have the position of the character in the texture, let's calculate some basic elements.
      ! I could have made this an array for super speed, but I want to be able to understand it in the future.
      allocate(gpu_character)

      ! We put our initial brush position at the top left of the character.
      pixel_x = ((position%x - 1) * slot_width) + 1
      pixel_y = ((position%y - 1) * slot_height) + 1

      ! Assign the widths.
      gpu_character%width_pixels = find_pixel_width(pixel_x, pixel_y)
      gpu_character%width_real = convert_pixel_width_to_real_width(gpu_character%width_pixels)

      ! Now assign the quad texture positions.
      gpu_character%top_left = pixel_position_to_opengl_position(pixel_x, pixel_y)
      gpu_character%bottom_left = pixel_position_to_opengl_position(pixel_x, pixel_y + character_height)
      gpu_character%bottom_right = pixel_position_to_opengl_position(pixel_x + gpu_character%width_pixels, pixel_y + character_height)
      gpu_character%top_right = pixel_position_to_opengl_position(pixel_x + gpu_character%width_pixels, pixel_y)

      ! Finally, we can assign this character into the database.
      call character_database%set(string_key, gpu_character)
    end do


  contains


    !* We need to do such complex work we need this subroutine to have functions.


    !* Convert the integral pixel width into double floating precision with range 0.0 - 1.0.
    function convert_pixel_width_to_real_width(input_width) result(real_width)
      implicit none

      integer(c_int), intent(in), value :: input_width
      real(c_double) :: real_width

      !! +1 and slot width cause I have no idea. It just looks more correct with it set like this.
      real_width = real(input_width, kind = c_double) / real(slot_width + 1, kind = c_double)
    end function convert_pixel_width_to_real_width


    !* Find how wide a character is by scanning it.
    function find_pixel_width(starting_x, starting_y) result(pixel_width)
      implicit none

      integer(c_int), intent(in), value :: starting_x, starting_y
      integer(c_int) :: scan_x, scan_y
      integer(c_int) :: pixel_width
      type(pixel) :: pixel_color

      ! Vertical scan, by row, right to left, until we hit something.
      !? -1 because: character width includes the border. We must remove this.
      scanner: do scan_x = starting_x + character_width - 1, starting_x, - 1
        do scan_y = starting_y + character_height - 1, starting_y, - 1
          pixel_color = rgba8_texture_data%get_pixel(scan_x, scan_y)
          if (pixel_color%a > 0) then
            !? +1 because: We need the "bottom left" of the current pixel.
            pixel_width = (scan_x - starting_x) + 1
            exit scanner
          end if
        end do
      end do scanner
    end function find_pixel_width


    !* Convert the pixel position into the OpenGL floating texture map position.
    !* Pixel position is the top left of a pixel viewed in GIMP.
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


  end subroutine calculate_opengl_texture_coordinates


  !* Completely wipe out all existing font characters. This might be slow.
  subroutine font_clear_database()
    use :: string
    use :: array, only: string_array, array_string_insert
    use :: terminal
    implicit none

    type(string_array) :: key_array
    class(*), allocatable :: generic_data
    class(*), pointer :: generic_pointer
    integer(c_int) :: i, remaining_size
    type(heap_string), dimension(:), allocatable :: temp_string_array


    !* We must check that there is anything in the database before we iterate.
    remaining_size =  character_database%count()

    if (remaining_size == 0) then
      print"(A)", "[Font]: Database was empty. Nothing to do. Success!"
      return
    end if

    ! Start with a size of 0.
    !! fixme: this is a great use for vectors!
    allocate(key_array%data(0))


    ! Now we will collect the keys from the iterator.
    !! fixme: use the new iterator style!
    do while(iterator%next(generic_key, generic_data))
      ! Appending.
      temp_string_array = array_string_insert(key_array%data, heap_string(generic_key%to_string()))
      call move_alloc(temp_string_array, key_array%data)
    end do

    ! Now clear the database out.
    do i = 1,size(key_array%data)
      !* We are manually managing memory, we must free as we go.
      call character_database%get_raw_ptr(key(key_array%data(i)%get()), generic_pointer)
      deallocate(generic_pointer)

      call character_database%unset(key(key_array%data(i)%get()))
    end do

    !* We will always check that the remaining size is 0. This will protect us from random issues.
    remaining_size = character_database%count()

    if (remaining_size /= 0) then
      print"(A)", colorize_rgb("[Font] Error: Did not delete all characters! Expected size: [0] | Actual: ["//int_to_string(remaining_size)//"]", 255, 0, 0)
    else
      print"(A)", "[Font]: Successfully cleared the character database."
    end if
  end subroutine font_clear_database


end module font
