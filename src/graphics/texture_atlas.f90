module texture_atlas
  use :: string_f90
  use :: vector_2i
  use :: fast_pack
  use :: memory_texture_module
  use :: texture
  use :: hashmap_str
  use, intrinsic :: iso_c_binding
  implicit none


  !* We need a pool of things to build upon.
  !* The game only has one texture atlas.


  private


  public :: texture_rectangle
  public :: texture_atlas_initialize
  public :: texture_atlas_add_texture_to_pack
  public :: texture_atlas_pack
  public :: texture_atlas_get_texture_rectangle
  public :: texture_atlas_get_texture_indices_clone_pointer
  public :: texture_atlas_get_texture_positions_array_clone_pointer
  public :: texture_atlas_get_texture_count
  public :: texture_atlas_destroy


  type :: texture_pack_element
    type(heap_string) :: full_path
    type(heap_string) :: file_name
  end type texture_pack_element


  !* Type: texture_pack_element
  !! This needs to be a vector.
  !! this needs a GC.
  type(texture_pack_element), dimension(:), allocatable :: textures_to_pack


  !* Type: Texture rectangle
  type(hashmap_string_key) :: texture_coordinates

  !* Type: An array of heap strings
  type(heap_string), dimension(:), allocatable :: texture_key_array

  !? Each index is the Block ID. [arr, id]
  !? Each array at the ID points to a gpu position in the texture atlas.
  integer(c_int), dimension(6,0), allocatable :: texture_indices(:, :)

  !! This can be a vector.
  type(texture_rectangle), dimension(:), allocatable :: texture_positions_array
  integer(c_int) :: key_array_size = 0


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

      call packer%pack(element%file_name%string, element%full_path%string)
    end do

    deallocate(textures_to_pack)

    ! call packer%save_to_png("./testing2.png")
    texture_data = packer%save_to_memory_texture()
    raw_texture_atlas_data = texture_data%get_raw_data()

    canvas_size = packer%get_canvas_size()

    call texture_create_from_memory("TEXTURE_ATLAS", raw_texture_atlas_data, canvas_size%x, canvas_size%y)

    ! Now we attach the coordinates pointer to be used for the lifetime of the game.
    !* Implementation note: this is getting passed a hashmap which simply contains a c_str.
    texture_coordinates = packer%get_texture_coordinates_database()

    texture_key_array = packer%get_keys()

    print"(A)","[Texture Atlas]: Successfully stitched together the texture atlas."

    ! This will optimize the understandable data into pure numbers.
    call optimize_data_array()
  end subroutine texture_atlas_pack


  !* Get a texture rectangle for OpenGL/Vulkan.
  function texture_atlas_get_texture_rectangle(texture_name) result(tr)
    implicit none

    character(len = *, kind = c_char), intent(in) :: texture_name
    type(texture_rectangle) :: tr
    type(texture_rectangle), pointer :: texture_rectangle_pointer
    type(c_ptr) :: raw_c_ptr

    if (.not. texture_coordinates%get(texture_name, raw_c_ptr)) then
      error stop "[Texture Atlas] Error: Null pointer."
    end if

    call c_f_pointer(raw_c_ptr, texture_rectangle_pointer)
    tr = texture_rectangle_pointer
  end function texture_atlas_get_texture_rectangle


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


  !* This will match up texture IDs to raw texture coordinate data so
  !* it can be accessed extremely fast.
  subroutine optimize_data_array()
    use :: block_repo, only: block_definition, block_repo_get_number_of_definitions, block_repo_get_definition_pointer_by_id
    implicit none

    character(len = :, kind = c_char), allocatable :: temp
    type(hashmap_string_key) :: string_to_index_array
    integer(c_int) :: i, y
    integer(c_int), pointer :: current_index
    type(c_ptr) :: raw_c_ptr
    type(texture_rectangle), pointer :: rect_pointer
    type(block_definition), pointer :: definition_pointer

    print"(A)","[Texture Atlas]: Begin cachiness optimization."

    !* Type: integer(c_int)
    string_to_index_array = new_hashmap_string_key(sizeof(10))

    key_array_size = size(texture_key_array)

    allocate(texture_positions_array(key_array_size))

    allocate(texture_indices(6, key_array_size))

    ! String name is first come first serve.
    ! As long as it never changes, this will work perfectly.
    do i = 1,size(texture_key_array)

      temp = texture_key_array(i)%string

      call string_to_index_array%set(temp, i)

      if (.not. texture_coordinates%get(temp, raw_c_ptr)) then
        error stop "[Texture Atlas] Error: wat"
      end if

      call c_f_pointer(raw_c_ptr, rect_pointer)

      texture_positions_array(i)%min_x = rect_pointer%min_x
      texture_positions_array(i)%min_y = rect_pointer%min_y
      texture_positions_array(i)%max_x = rect_pointer%max_x
      texture_positions_array(i)%max_y = rect_pointer%max_y
    end do

    ! Now iterate them, and insert the indices of the textures in the array.
    do i = 1,block_repo_get_number_of_definitions()
      definition_pointer => block_repo_get_definition_pointer_by_id(i)

      do y = 1,6

        if  (.not. string_to_index_array%get(definition_pointer%textures(y)%string, raw_c_ptr)) then
          error stop "[Texture Atlas] Error: Received an invalid texture. ["//definition_pointer%textures(y)%string//"]"
        end if

        call c_f_pointer(raw_c_ptr, current_index)

        texture_indices(y, i) = current_index
      end do
    end do

    call string_to_index_array%destroy()

    print"(A)","[Texture Atlas]: Cachiness optimization complete. Optimized: ["//int_to_string(key_array_size)//"] textures."
  end subroutine optimize_data_array


  !* Clone the texture indices and return a pointer to the data.
  function texture_atlas_get_texture_indices_clone_pointer() result(indices_clone_pointer)
    implicit none

    integer(c_int), dimension(6,0), pointer :: indices_clone_pointer(:, :)

    allocate(indices_clone_pointer(6, key_array_size))

    indices_clone_pointer = texture_indices
  end function texture_atlas_get_texture_indices_clone_pointer


  !* Clone the texture positions array and return a pointer to the data.
  function texture_atlas_get_texture_positions_array_clone_pointer() result(positions_array_clone_pointer)
    implicit none

    type(texture_rectangle), dimension(:), pointer :: positions_array_clone_pointer(:)

    allocate(positions_array_clone_pointer(key_array_size))

    positions_array_clone_pointer = texture_positions_array
  end function texture_atlas_get_texture_positions_array_clone_pointer


  !* Get the number of textures in the atlas.
  function texture_atlas_get_texture_count() result(count)
    implicit none

    integer(c_int) :: count

    count = key_array_size
  end function texture_atlas_get_texture_count


  !* This frees any pointers used by the texture atlas module.
  subroutine texture_atlas_destroy()
    implicit none

    call texture_coordinates%destroy()

    print"(A)", "[Texture Atlas]: Successfully destroyed texture atlas."
  end subroutine


end module texture_atlas
