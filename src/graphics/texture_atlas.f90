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


  public :: texture_atlas_initialize
  public :: texture_atlas_add_texture_to_pack
  public :: texture_atlas_pack
  public :: texture_atlas_debug


  type :: texture_pack_element
    type(heap_string) :: full_path
    type(heap_string) :: file_name
  end type texture_pack_element


  type(texture_pack_element), dimension(:), allocatable :: textures_to_pack
  type(fhash_tbl_t), pointer :: texture_coordinates_pointer


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


    config%canvas_expansion_amount = 1000
    ! config%enable_trimming = .false.

    packer = fast_packer(config)

    print"(A)","[Texture Atlas]: Texture atlas construction START."

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
  end subroutine texture_atlas_pack


  subroutine texture_atlas_debug()
    implicit none

    class(*), allocatable :: generic_data
    integer(c_int) :: status
    type(texture_rectangle) :: output

    call texture_coordinates_pointer%get_raw(key("default_furnace_front_active.png"), generic_data, stat = status)

    if (status /= 0) then
      error stop "Debug failed, it doesn't exist"
    end if

    select type (generic_data)
     type is (texture_rectangle)
      output = generic_data
     class default
      error stop "How, did this even get in here?!"
    end select

    print*,output

    ! Make this actually readable
    print*,"BEGIN OUTPUT"

    write(*,"(A f0.10)") "min_x = 0", output%min_x

    write(*,"(A f0.10)") "min_y = 0", output%min_y

    write(*,"(A f0.10)") "max_x = 0", output%max_x

    write(*,"(A f0.10)") "max_y = 0", output%max_y

  end subroutine texture_atlas_debug


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


end module texture_atlas
