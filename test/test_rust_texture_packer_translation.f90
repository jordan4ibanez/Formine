module test_texture_packer_suite
  use :: texture_packer_mod
  implicit none


contains


  subroutine begin_test()
    use :: iso_c_binding
    use :: string
    use :: stb_image
    use :: rgba8_texture_module
    implicit none

    type(texture_packer_conf) :: config
    type(texture_packer) :: packer
    integer :: i
    character(len = :, kind = c_char), allocatable :: root_path, temp_path
    integer(1), dimension(:), allocatable :: raw_image_data
    integer(c_int) :: width, height, channels, result
    type(rgba8_texture) :: rgba_image_data


    config%max_width = 400
    config%max_height = 400
    config%trim = .false.
    config%allow_rotation = .false.
    config%texture_outlines = .true.
    config%border_padding = 2
    config%force_max_dimensions = .false.

    packer = texture_packer(config)

    root_path = "./test/textures/"

    do i = 1,10

      temp_path = root_path//int_to_string(i)//".png"
      print*,temp_path

      raw_image_data = stbi_load(temp_path, width, height, channels, 4)

      ! print*,"len",size(raw_image_data) / 4
      ! print*,width, height, channels

      rgba_image_data = rgba8_texture(raw_image_data, width, height)


      ! print*,packer%packer%skylines
      result = packer%pack(temp_path, rgba_image_data)

      if(result == TEXTURE_PACKER_OK) then
        ! print*,"PACKED"
        ! print*,packer%packer%skylines
        ! print*,"skyline size:",size(packer%packer%skylines)
      else
        print*,result, "failed to pack"
      end if
    end do

  end subroutine


end module test_texture_packer_suite


program test_texture_packer
  use test_texture_packer_suite
  implicit none


  call begin_test()


end program test_texture_packer
