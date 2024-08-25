module test_fast_pack_suite
  use :: fast_pack
  implicit none


contains


  subroutine begin_test()
    use :: iso_c_binding
    use :: string
    use :: stb_image
    use :: memory_texture_module
    implicit none

    ! type(texture_packer_conf) :: config
    type(fast_packer) :: packer
    integer :: i
    character(len = :, kind = c_char), allocatable :: root_path, temp_path, temp_key
    type(fast_packer_config) :: config

    config%padding = 1
    config%width = 400
    config%height = 400
    config%enable_trimming = .true.
    packer = fast_packer(config)
    root_path = "./test/textures/"
    print*,"begin fast packer test"
    do i = 1,10
      temp_path = root_path//int_to_string(i)//".png"
      print*,temp_path
      temp_key = string_get_file_name(temp_path)
      call packer%pack(temp_key, temp_path)
    end do
    ! testing = packer%save_to_memory_texture()
    call packer%save_to_png("./test/textures/packer_test_result.png")
  end subroutine begin_test


  subroutine test_memory_leak()
    use :: iso_c_binding
    use :: string
    use :: stb_image
    use :: memory_texture_module
    implicit none

    ! type(texture_packer_conf) :: config
    type(fast_packer) :: packer
    integer :: i
    character(len = :, kind = c_char), allocatable :: root_path, temp_path, temp_key
    type(fast_packer_config) :: config
    type(memory_texture) :: testing

    print*,"BEGIN TESTING MEMORY LEAK FOR FAST PACKER"

    do
      config%padding = 1
      config%width = 400
      config%height = 400
      config%enable_trimming = .true.
      packer = fast_packer(config)
      root_path = "./test/textures/"
      do i = 1,10
        temp_path = root_path//int_to_string(i)//".png"
        temp_key = string_get_file_name(temp_path)
        call packer%pack(temp_key, temp_path)
      end do
      testing = packer%save_to_memory_texture()
      ! call packer%save_to_png("./test/textures/packer_test_result.png")
    end do
  end subroutine test_memory_leak



end module test_fast_pack_suite


program test_fast_pack
  use test_fast_pack_suite
  implicit none


  call begin_test()

  call test_memory_leak()
end program test_fast_pack
