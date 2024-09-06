module texture_atlas
  use :: string
  use :: vector_2i
  use :: fast_pack
  use :: memory_texture_module
  use :: texture
  use :: fhash, only: fhash_tbl_t, key => fhash_key
  use, intrinsic :: iso_c_binding
  implicit none


  !* We need a pool of things to build upon.
  !* The game only has one texture atlas.


  private


  public :: texture_rectangle
  public :: texture_atlas_initialize
  public :: texture_atlas_add_texture_to_pack
  public :: texture_atlas_pack
  public :: texture_atlas_get_texture_rectangle_pointer
  public :: texture_atlas_debug
  public :: texture_atlas_destroy


  type :: texture_pack_element
    type(heap_string) :: full_path
    type(heap_string) :: file_name
  end type texture_pack_element


  type(texture_pack_element), dimension(:), allocatable :: textures_to_pack
  type(fhash_tbl_t), pointer :: texture_coordinates_pointer
  type(heap_string), dimension(:), allocatable :: showcase_array
  integer(c_int) :: current_index, showcase_length


contains


  !* Set up the texture atlas for use.
  subroutine texture_atlas_initialize()
    implicit none

    allocate(textures_to_pack(0))
  end subroutine texture_atlas_initialize


  !* Add a texture for the texture atlas to stitch together.
  subroutine texture_atlas_add_texture_to_pack(full_path, file_name)
    implicit none

    character(len = *, kind = c_char), intent(in) :: full_path, file_name
    type(texture_pack_element), allocatable :: new_element
    type(texture_pack_element), dimension(:), allocatable :: temp_string_array

    allocate(new_element)

    new_element%full_path = full_path
    new_element%file_name = file_name

    temp_string_array = array_texture_pack_element_insert(textures_to_pack, new_element)
    call move_alloc(temp_string_array, textures_to_pack)
  end subroutine texture_atlas_add_texture_to_pack


  !* The final step of the texture atlas.
  subroutine texture_atlas_pack()
    implicit none

    type(fast_packer) :: packer
    type(fast_packer_config) :: config
    integer(c_int) :: i
    type(texture_pack_element) :: element
    type(memory_texture) :: texture_data
    integer(1), dimension(:), allocatable :: raw_texture_atlas_data
    type(vec2i) :: canvas_size

    print"(A)","[Texture Atlas]: Stitching together the texture atlas."

    config%canvas_expansion_amount = 1000
    ! config%enable_trimming = .false.

    packer = fast_packer(config)

    do i = 1,size(textures_to_pack)
      element = textures_to_pack(i)

      call packer%pack(element%file_name%get(), element%full_path%get())
    end do

    deallocate(textures_to_pack)

    ! call packer%save_to_png("./testing2.png")
    texture_data = packer%save_to_memory_texture()
    raw_texture_atlas_data = texture_data%get_raw_data()

    canvas_size = packer%get_canvas_size()

    call texture_create_from_memory("TEXTURE_ATLAS", raw_texture_atlas_data, canvas_size%x, canvas_size%y)

    ! Now we attach the coordinates pointer to be used for the lifetime of the game.
    texture_coordinates_pointer => packer%get_texture_coordinates_database()

    showcase_array = packer%get_keys()

    current_index = 1
    showcase_length = size(showcase_array)

    print"(A)","[Texture Atlas]: Successfully stitched together the texture atlas."
  end subroutine texture_atlas_pack


  !* Get a texture rectangle for OpenGL/Vulkan.
  function texture_atlas_get_texture_rectangle_pointer(texture_name) result(texture_rectangle_pointer)
    implicit none

    character(len = *, kind = c_char), intent(in) :: texture_name
    type(texture_rectangle), pointer :: texture_rectangle_pointer
    class(*), pointer :: generic_pointer
    integer(c_int) :: status

    call texture_coordinates_pointer%get_raw_ptr(key(texture_name), generic_pointer, stat = status)

    if (status /= 0) then
      error stop "[Texture Atlas] Error: Null pointer."
    end if

    select type(generic_pointer)
     type is (texture_rectangle)
      texture_rectangle_pointer => generic_pointer
     class default
      error stop "[Texture Atlas] Error: Wrong pointer type."
    end select
  end function texture_atlas_get_texture_rectangle_pointer


  !* This is debug for selecting an atlas element.
  function texture_atlas_debug() result(texture_location)
    implicit none

    class(*), allocatable :: generic_data
    integer(c_int) :: status
    type(texture_rectangle) :: texture_location

    call texture_coordinates_pointer%get_raw(key(showcase_array(current_index)%get()), generic_data, stat = status)

    if (status /= 0) then
      error stop "Debug failed, it doesn't exist"
    end if

    select type (generic_data)
     type is (texture_rectangle)
      texture_location = generic_data
     class default
      error stop "How, did this even get in here?!"
    end select

    current_index = current_index + 1
    if (current_index > showcase_length) then
      current_index = 1
    end if

    ! print*,output

    ! Make this actually readable
    ! print*,"BEGIN OUTPUT"

    ! write(*,"(A f0.10)") "min_x = 0", output%min_x

    ! write(*,"(A f0.10)") "min_y = 0", output%min_y

    ! write(*,"(A f0.10)") "max_x = 0", output%max_x

    ! write(*,"(A f0.10)") "max_y = 0", output%max_y

  end function texture_atlas_debug


  !* Insert a value at the end of a memory texture array.
  function array_texture_pack_element_insert(input, new_value) result(output)
    use :: memory_texture_module
    implicit none

    type(texture_pack_element), dimension(:), intent(in) :: input
    type(texture_pack_element), intent(in), value :: new_value
    type(texture_pack_element), dimension(:), allocatable :: output
    integer(c_int) :: old_size, i

    old_size = size(input)

    allocate(output(old_size + 1))

    do i = 1,old_size
      output(i) = input(i)
    end do

    output(old_size + 1) = new_value
  end function array_texture_pack_element_insert


  !* This frees any pointers used by the texture atlas module.
  subroutine texture_atlas_destroy()
    use :: terminal
    implicit none

    ! Free the pointer.
    if (associated(texture_coordinates_pointer)) then
      deallocate(texture_coordinates_pointer)
      ! Double check.
      if (associated(texture_coordinates_pointer)) then
        print"(A)",colorize_rgb("[Texture Atlas] Error: Failed to free the texture coordinates pointer.", 255, 0, 0)
        return
      end if
    else
      ! If this happens, something went very wrong.
      print"(A)",colorize_rgb("[Texture Atlas] Error: Texture coordinates pointer is not associated.", 255, 0, 0)
      return
    end if
    ! Everything is freed, hooray.
    print"(A)", "[Texture Atlas]: Successfully destroyed texture atlas."
  end subroutine


end module texture_atlas
