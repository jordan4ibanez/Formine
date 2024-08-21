module test_texture_packer_suite
  use :: texture_packer_mod
  implicit none


contains


  subroutine begin_test()
    use :: iso_c_binding
    use :: string
    implicit none

    type(texture_packer_conf) :: config
    type(texture_packer) :: packer
    integer :: i
    character(len = :, kind = c_char), allocatable :: root_path, temp_path

    config%trim = .false.

    packer = texture_packer(config)


    root_path = "./test/textures/"


    do i = 1,11

      temp_path = root_path//int_to_string(i)//".png"
      print*,temp_path

    end do

  end subroutine


end module test_texture_packer_suite


program test_texture_packer
  use test_texture_packer_suite
  implicit none


  call begin_test()


end program test_texture_packer
