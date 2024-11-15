module fast_pack
  use :: memory_texture_module
  use :: stb_image
  use :: string_f90
  use :: fast_pack_types
  use :: hashmap_str
  use, intrinsic :: iso_c_binding
  implicit none


  !* Despite it's name, it's probably not very fast. :P


  private


  public :: texture_rectangle
  public :: fast_packer_config
  public :: fast_packer


  !* Configure the fast packer.
  type :: fast_packer_config
    logical(c_bool) :: fast_canvas_export = .true.
    logical(c_bool) :: enable_trimming = .false.
    integer(c_int) :: padding = 1
    type(pixel) :: edge_color
    type(pixel) :: blank_color
    integer(c_int) :: canvas_expansion_amount = 100
    logical(c_bool) :: debug_edge = .false.
    integer(c_int) :: width = 400
    integer(c_int) :: height = 400
  end type fast_packer_config


  !* The fast packer.
  type :: fast_packer
    private

    integer(c_int) :: current_id = 1
    logical(c_bool) :: fast_canvas_export = .true.
    logical(c_bool) :: enable_trimming = .false.
    integer(c_int) :: padding = 1
    type(pixel) :: edge_color
    type(pixel) :: blank_color
    integer(c_int) :: canvas_expansion_amount = 100
    logical(c_bool) :: debug_edge = .false.
    integer(c_int) :: canvas_width = 0
    integer(c_int) :: canvas_height = 0
    logical(c_bool) :: allocated = .false.
    integer(c_int) :: max_x = 0
    integer(c_int) :: max_y = 0
    logical(c_bool) :: locked_out = .false.
    ! Everything below this is allocated in the constructor.
    !* Type: texture_rectangle
    type(hashmap_string_key) :: texture_coordinates
    !! THIS ALL NEEDS TO BE A VECTOR OMG.
    type(heap_string), dimension(:), allocatable :: keys_array
    integer(c_int), dimension(:), allocatable :: position_x
    integer(c_int), dimension(:), allocatable :: position_y
    integer(c_int), dimension(:), allocatable :: box_width
    integer(c_int), dimension(:), allocatable :: box_height
    type(memory_texture), dimension(:), allocatable :: textures
    integer(c_int), dimension(:), allocatable :: available_x
    integer(c_int), dimension(:), allocatable :: available_y

  contains
    procedure :: get_canvas_size => fast_packer_get_canvas_size
    procedure :: get_keys => fast_packer_get_keys
    procedure :: pack => fast_packer_pack_from_file_path, fast_packer_pack_from_memory
    procedure, private :: internal_pack => fast_packer_internal_pack
    procedure, private :: tetris_pack => fast_packer_tetris_pack
    procedure :: get_texture_coordinates_database => fast_packer_get_texture_coordinates_database
    procedure :: save_to_png => fast_packer_save_to_png
    procedure :: save_to_memory_texture => fast_packer_save_to_memory_texture
    procedure, private :: create_texture_rectangles => fast_packer_create_texture_rectangles
    procedure, private :: update_max_size => fast_packer_update_max_size
    procedure, private :: upload_texture_path => fast_packer_upload_texture_from_file_path
    procedure, private :: upload_texture_memory => fast_packer_upload_texture_from_memory
    procedure, private :: trim_and_sort_available_slots => fast_packer_trim_and_sort_available_slots
  end type fast_packer


  interface fast_packer
    module procedure :: constructor_fast_packer, constructor_fast_packer_blank
  end interface fast_packer


contains


  function constructor_fast_packer_blank() result(new_fast_packer)
    implicit none

    type(fast_packer_config) :: config
    type(fast_packer) :: new_fast_packer

    error stop "USE THE VECTOR MODULE!"

    ! Config gets generated with it's defaults, hooray!

    ! Now chain to the constructor with a config.
    new_fast_packer = constructor_fast_packer(config)
  end function constructor_fast_packer_blank


  function constructor_fast_packer(config) result(new_fast_packer)
    implicit none

    type(fast_packer_config), intent(in) :: config
    type(fast_packer) :: new_fast_packer

    ! Assign from config.
    new_fast_packer%fast_canvas_export = config%fast_canvas_export
    new_fast_packer%enable_trimming = config%enable_trimming
    new_fast_packer%padding = config%padding
    new_fast_packer%edge_color = config%edge_color
    new_fast_packer%blank_color = config%blank_color
    new_fast_packer%canvas_expansion_amount = config%canvas_expansion_amount
    new_fast_packer%debug_edge = config%debug_edge
    new_fast_packer%canvas_width = config%width
    new_fast_packer%canvas_height = config%height

    ! Allocate
    new_fast_packer%texture_coordinates = new_hashmap_string_key(sizeof(texture_rectangle()))
    allocate(new_fast_packer%keys_array(0))
    allocate(new_fast_packer%position_x(0))
    allocate(new_fast_packer%position_y(0))
    allocate(new_fast_packer%box_width(0))
    allocate(new_fast_packer%box_height(0))
    allocate(new_fast_packer%textures(0))
    allocate(new_fast_packer%available_x(1))
    allocate(new_fast_packer%available_y(1))

    new_fast_packer%available_x(1) = config%padding
    new_fast_packer%available_y(1) = config%padding

    new_fast_packer%allocated = .true.
  end function constructor_fast_packer


  !* Get the canvas size of a fast packer.
  function fast_packer_get_canvas_size(this) result(size)
    use :: vector_2i
    implicit none

    class(fast_packer), intent(in) :: this
    type(vec2i) :: size

    size%x = this%canvas_width
    size%y = this%canvas_height
  end function fast_packer_get_canvas_size


  !* Get the array of keys.
  function fast_packer_get_keys(this) result(key_array)
    use :: vector_2i
    implicit none

    class(fast_packer), intent(in) :: this
    type(heap_string), dimension(:), allocatable :: key_array

    key_array = this%keys_array
  end function fast_packer_get_keys


  !* Pack a texture located on disk.
  subroutine fast_packer_pack_from_file_path(this, texture_key, file_path)
    implicit none

    class(fast_packer), intent(inout) :: this
    character(len = *, kind = c_char), intent(in) :: texture_key, file_path
    integer(c_int) :: current_index

    if (this%locked_out) then
      error stop "[Fast Pack] Error: Fast Packer has already processed it's data. It is locked."
    end if

    if (.not. this%allocated) then
      error stop "[Fast Pack] Error: Fast Packer not allocated! Please use the constructor."
    end if

    current_index = this%upload_texture_path(texture_key, file_path)

    call this%internal_pack(current_index)
  end subroutine fast_packer_pack_from_file_path


  !* Pack a texture located in memory.
  subroutine fast_packer_pack_from_memory(this, texture_key, mem_texture)
    implicit none

    class(fast_packer), intent(inout) :: this
    character(len = *, kind = c_char), intent(in) :: texture_key
    type(memory_texture), intent(in) :: mem_texture
    integer(c_int) :: current_index

    if (this%locked_out) then
      error stop "[Fast Pack] Error: Fast Packer has already processed it's data. It is locked."
    end if

    if (.not. this%allocated) then
      error stop "[Fast Pack] Error: Fast Packer not allocated! Please use the constructor."
    end if

    current_index = this%upload_texture_memory(texture_key, mem_texture)

    call this%internal_pack(current_index)
  end subroutine fast_packer_pack_from_memory


  !* Pack the image data.
  subroutine fast_packer_internal_pack(this, current_index)
    implicit none

    class(fast_packer), intent(inout) :: this
    integer(c_int), intent(in) :: current_index

    do while(.not. this%tetris_pack(current_index))
      ! Reallocate space if cannot fit.
      this%canvas_width = this%canvas_width + this%canvas_expansion_amount
      this%canvas_height = this%canvas_height + this%canvas_expansion_amount
    end do

    ! Finally, update the canvas's size in memory.
    call this%update_max_size(current_index)
  end subroutine fast_packer_internal_pack


  !* Tetris packing algorithm.
  !* This algorithm is HORRIBLE.
  function fast_packer_tetris_pack(this, current_index) result(pack_success)
    use :: constants, only: C_INT_MAX
    use :: array, only: array_i32_insert
    implicit none

    class(fast_packer), intent(inout) :: this
    integer(c_int), intent(in) :: current_index
    logical(c_bool) :: pack_success, found, failed
    integer(c_int) :: padding, score, max_x, max_y, best_x, best_y, this_width, this_height
    integer(c_int) :: y_index, x_index, y, x, new_score, i, other_x, other_y, other_width, other_height
    integer(c_int), dimension(:), allocatable :: temp_x_array, temp_y_array

    found = .false.
    padding = this%padding
    score = C_INT_MAX
    max_x = this%canvas_width
    max_y = this%canvas_height
    best_x = padding
    best_y = padding
    this_width = this%box_width(current_index)
    this_height = this%box_height(current_index)

    ! Iterate all available positions.
    y_iter: do y_index = 1,size(this%available_y)

      y = this%available_y(y_index)

      x_iter: do x_index = 1, size(this%available_x)

        x = this%available_x(x_index)

        new_score = x + y

        if (new_score >= score) then
          exit x_iter
        end if

        ! Bounds check.
        if (x + this_width + padding < max_x .and. y + this_height + padding < max_y) then

          failed = .false.

          ! Collided with other box failure.
          ! Index each collision box to check if within.

          inner_iter: do i = 1,current_index - 1

            other_x = this%position_x(i)
            other_y = this%position_y(i)
            other_width = this%box_width(i)
            other_height = this%box_height(i)

            ! If it hit something, we'll try the next position.
            if (other_x + other_width + padding > x .and. &
              other_x < x + this_width + padding .and. &
              other_y + other_height + padding > y .and. &
              other_y < y + this_height + padding) then

              failed = .true.
              exit inner_iter
            end if
          end do inner_iter

          if (.not. failed) then
            found = .true.
            best_x = x
            best_y = y
            score = new_score
            exit x_iter
          end if
        end if

      end do x_iter
    end do y_iter

    if (.not. found) then
      pack_success = .false.
      return
    end if

    this%position_x(current_index) = best_x
    this%position_y(current_index) = best_y

    temp_x_array = array_i32_insert(this%available_x, best_x + this_width + padding)
    call move_alloc(temp_x_array, this%available_x)

    temp_y_array = array_i32_insert(this%available_y, best_y + this_height + padding)
    call move_alloc(temp_y_array, this%available_y)

    pack_success = .true.
  end function fast_packer_tetris_pack


  !* Pull the texture coordinates database out of the fast packer for use without the fast packer.
  !* This allows the fast packer to go out of scope and be cleaned up.
  !* This is returning a c_ptr under the hood on the stack.
  function fast_packer_get_texture_coordinates_database(this) result(database_pointer)
    implicit none

    class(fast_packer), intent(inout) :: this
    type(hashmap_string_key) :: database_pointer

    database_pointer = this%texture_coordinates
  end function fast_packer_get_texture_coordinates_database


  !* Write the texture packer's data to a png file.
  subroutine fast_packer_save_to_png(this, file_path)
    use :: stb_image, only: stbi_write_png
    use :: string_f90, only: int_to_string
    implicit none

    class(fast_packer), intent(inout) :: this
    character(len = *, kind = c_char), intent(in) :: file_path
    type(memory_texture) :: new_memory_texture
    integer(1), dimension(:), allocatable :: raw_texture_data
    integer(c_int) :: status

    new_memory_texture = this%save_to_memory_texture()

    raw_texture_data = new_memory_texture%get_raw_data()

    status = stbi_write_png(file_path, new_memory_texture%width, new_memory_texture%height, raw_texture_data)
    if (status == 0) then
      error stop "[Fast Pack] Error: Failed to write png image to ["//file_path//"] status: ["//int_to_string(status)//"]"
    end if
  end subroutine


  !* Write the texture packer's data to a memory_texture. This locks out the fast packer.
  function fast_packer_save_to_memory_texture(this) result(new_memory_texture)
    implicit none

    class(fast_packer), intent(inout) :: this
    type(memory_texture) :: new_memory_texture
    integer(c_int) :: i, this_x, this_y, this_width, this_height, x, y, canvas_pixel_x, canvas_pixel_y, texture_pixel_x, texture_pixel_y
    type(pixel) :: current_pixel

    if (this%locked_out) then
      error stop "[Fast Pack] Error: Fast Packer has already processed it's data. It is locked."
    end if

    this%canvas_width = this%max_x
    this%canvas_height = this%max_y

    ! Create a new memory texture the size of the canvas.
    new_memory_texture = memory_texture(this%canvas_width, this%canvas_height)

    ! Iterate through each texture and copy the data into the new memory texture.
    do i = 1,this%current_id - 1

      this_x = this%position_x(i)
      this_y = this%position_y(i)
      this_width = this%box_width(i)
      this_height = this%box_height(i)

      ! Iterate the pixel. We're doing it this way so it's linear in memory.
      do y = 1, this_height
        do x = 1, this_width

          canvas_pixel_x = x + this_x
          canvas_pixel_y = y + this_y

          texture_pixel_x = x
          texture_pixel_y = y

          current_pixel = this%textures(i)%get_pixel(texture_pixel_x, texture_pixel_y)

          call new_memory_texture%set_pixel(canvas_pixel_x, canvas_pixel_y, current_pixel)
        end do
      end do
    end do

    this%locked_out = .true.

    ! Now that we have everything iterated, the canvas is locked.
    ! We can create the locations of each texture in OpenGL/Vulkan space.
    call this%create_texture_rectangles()
  end function fast_packer_save_to_memory_texture


  !* The final step in the fast packer.
  !* Creates OpenGL/Vulkan space coordinates to be queried.
  subroutine fast_packer_create_texture_rectangles(this)
    use :: terminal
    implicit none

    class(fast_packer), intent(inout) :: this
    integer(c_int) :: keys_array_size, i
    real(c_double) :: d_min_x, d_min_y, d_max_x, d_max_y, d_canvas_width, d_canvas_height
    type(texture_rectangle) :: new_texture_rectangle

    ! We use a hash table to store the texture_rectangles.
    ! Ideally, access time will be n(1). Hopefully.

    keys_array_size = size(this%keys_array)

    !? There is nothing to do, which can be very bad.
    if (keys_array_size <= 0) then
      call print_color(WARNING, "[Fast Pack] Warning: Canvas is blank. This might cause serious issues.")
      return
    end if

    !? We are doing the calculation in double precision then cutting it into float.

    d_canvas_width = real(this%canvas_width, kind = c_double)
    d_canvas_height = real(this%canvas_height, kind = c_double)

    do i = 1,keys_array_size

      ! First, put the raw data into the stack double floating point variables.
      d_min_x = real(this%position_x(i), kind = c_double)
      d_min_y = real(this%position_y(i), kind = c_double)
      d_max_x = real(this%position_x(i) + this%box_width(i), kind = c_double)
      d_max_y = real(this%position_y(i) + this%box_height(i), kind = c_double)

      ! if (this%keys_array(i)%get() == "debug_texture.png") then
      !   print*,"-----------------------"
      !   print*,this%position_x(i)
      !   print*,this%position_y(i)
      !   print*,this%box_width(i)
      !   print*,this%box_height(i)
      !   print*,d_min_x
      !   print*,d_max_x
      !   print*,d_canvas_width
      ! end if

      ! Next, create the floating point position in OpenGL/Vulkan memory.
      ! We are chopping the precision to single floating point.
      new_texture_rectangle = texture_rectangle()

      new_texture_rectangle%min_x = real(d_min_x / d_canvas_width, kind = c_float)
      new_texture_rectangle%min_y = real(d_min_y / d_canvas_height, kind = c_float)
      new_texture_rectangle%max_x = real(d_max_x / d_canvas_width, kind = c_float)
      new_texture_rectangle%max_y = real(d_max_y / d_canvas_height, kind = c_float)

      ! Now put it into the database.
      call this%texture_coordinates%set(this%keys_array(i)%string, new_texture_rectangle)
    end do
  end subroutine fast_packer_create_texture_rectangles


  !* Update the size of the texture packer's canvas.
  subroutine fast_packer_update_max_size(this, current_index)
    implicit none

    class(fast_packer), intent(inout) :: this
    integer(c_int), intent(in) :: current_index
    integer(c_int) :: new_right, new_top

    new_right = this%position_x(current_index) + this%box_width(current_index)
    new_top = this%position_y(current_index) + this%box_height(current_index)

    if (new_right > this%max_x) then
      this%max_x = new_right + this%padding
    end if

    if (new_top > this%max_y) then
      this%max_y = new_top + this%padding
    end if
  end subroutine


  !* Upload a texture from a file path into the fast_packer.
  function fast_packer_upload_texture_from_file_path(this, texture_key, file_path) result(new_index)
    implicit none

    class(fast_packer), intent(inout) :: this
    character(len = *, kind = c_char), intent(in) :: texture_key, file_path
    integer(c_int) :: new_index
    integer(c_int) :: width, height, channels
    integer(1), dimension(:), allocatable :: temporary_raw_texture
    type(memory_texture) :: new_texture

    temporary_raw_texture = stbi_load(file_path, width, height, channels, 4)

    new_texture = memory_texture(temporary_raw_texture, width, height)

    ! This is chained.
    new_index = this%upload_texture_memory(texture_key, new_texture)
  end function fast_packer_upload_texture_from_file_path


  !* Upload a memory_texture into the fast_packer.
  function fast_packer_upload_texture_from_memory(this, texture_key, mem_texture) result(new_index)
    use :: array, only: array_i32_insert, array_string_insert, array_memory_texture_insert
    implicit none

    class(fast_packer), intent(inout) :: this
    character(len = *, kind = c_char), intent(in) :: texture_key
    type(memory_texture), intent(in) :: mem_texture
    type(memory_texture) :: trimmed_texture
    integer(c_int) :: new_index
    integer(c_int), dimension(:), allocatable :: temp_x, temp_y, temp_width, temp_height
    type(heap_string), dimension(:), allocatable :: temp_keys
    type(memory_texture), dimension(:), allocatable :: temp_textures

    ! Slap on trimming in the least efficient manor.
    if (this%enable_trimming) then
      trimmed_texture = fast_pack_trim_texture(mem_texture)
    else
      trimmed_texture = mem_texture
    end if

    new_index = this%current_id

    this%current_id = this%current_id + 1

    ! Add data.
    temp_keys = array_string_insert(this%keys_array, heap_string(texture_key))
    call move_alloc(temp_keys, this%keys_array)

    temp_x = array_i32_insert(this%position_x, 0)
    call move_alloc(temp_x, this%position_x)

    temp_y = array_i32_insert(this%position_y, 0)
    call move_alloc(temp_y, this%position_y)

    temp_width = array_i32_insert(this%box_width, trimmed_texture%width)
    call move_alloc(temp_width, this%box_width)

    temp_height = array_i32_insert(this%box_height, trimmed_texture%height)
    call move_alloc(temp_height, this%box_height)

    temp_textures = array_memory_texture_insert(this%textures, trimmed_texture)
    call move_alloc(temp_textures, this%textures)

    call this%trim_and_sort_available_slots()
  end function fast_packer_upload_texture_from_memory


  !* Removes duplicates, automatically sorts small to large.
  subroutine fast_packer_trim_and_sort_available_slots(this)
    use :: array, only: array_i32_small_to_large_unique
    implicit none

    class(fast_packer), intent(inout) :: this
    integer(c_int), dimension(:), allocatable :: temp_x, temp_y

    temp_x = array_i32_small_to_large_unique(this%available_x)
    temp_y = array_i32_small_to_large_unique(this%available_y)

    call move_alloc(temp_x, this%available_x)
    call move_alloc(temp_y, this%available_y)
  end subroutine fast_packer_trim_and_sort_available_slots


  !* Trims blank space off a texture.
  function fast_pack_trim_texture(input) result(output)
    implicit none

    type(memory_texture), intent(in) :: input
    type(memory_texture) :: output
    integer(c_int) :: texture_width, texture_height, min_x, min_y, x, y, max_x, max_y, new_size_x, new_size_y
    type(pixel) :: current_pixel

    min_x = 0
    max_x = 0
    min_y = 0
    max_y = 0

    texture_width = input%width
    texture_height = input%height

    ! MIN_X: Scan rows for alpha.
    iterator_min_x: do x = 1,texture_width
      do y = 1, texture_height
        current_pixel = input%get_pixel(x,y)
        if (current_pixel%a > 0) then
          min_x = x - 1;
          exit iterator_min_x
        end if
      end do
    end do iterator_min_x


    ! MAX_X: Scan rows for alpha in reverse.
    iterator_max_x: do x = texture_width,1,-1
      do y = 1, texture_height
        current_pixel = input%get_pixel(x,y)
        if (current_pixel%a > 0) then
          max_x = x;
          exit iterator_max_x
        end if
      end do
    end do iterator_max_x


    ! MIN_Y: Scan columns for alpha.
    iterator_min_y: do y = 1, texture_height
      do x = 1,texture_width
        current_pixel = input%get_pixel(x,y)
        if (current_pixel%a > 0) then
          min_y = y - 1;
          exit iterator_min_y
        end if
      end do
    end do iterator_min_y


    ! MAX_Y: Scan columns for alpha in reverse.
    iterator_max_y: do y = texture_height,1,-1
      do x = 1,texture_width
        current_pixel = input%get_pixel(x,y)
        if (current_pixel%a > 0) then
          max_y = y;
          exit iterator_max_y
        end if
      end do
    end do iterator_max_y

    new_size_x = max_x - min_x
    new_size_y = max_y - min_y

    ! It's the same size, just return the same memory.
    if (new_size_x == input%width .and. new_size_y == input%height) then
      output = input
      return
    end if

    output = memory_texture(new_size_x, new_size_y)

    do x = 1, new_size_x
      do y = 1, new_size_y
        call output%set_pixel(x, y, input%get_pixel(x + min_x, y + min_y))
      end do
    end do

  end function fast_pack_trim_texture


end module fast_pack
