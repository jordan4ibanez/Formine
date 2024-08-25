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


    texture_coordinates_pointer => packer%get_texture_coordinates_database()


    call testing()
  end subroutine texture_atlas_pack

  subroutine testing()
    use :: fhash, only: fhash_iter_t, fhash_key_t
    implicit none

    type(fhash_iter_t) :: iterator
    class(fhash_key_t), allocatable :: generic_key
    class(*), allocatable :: generic_data
    integer(c_int) :: i, remaining_size
    type(heap_string), dimension(:), allocatable :: temp_string_array


    iterator = fhash_iter_t(texture_coordinates_pointer)

    do while(iterator%next(generic_key, generic_data))
      print*,generic_key%to_string()
    end do
  end subroutine testing


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
