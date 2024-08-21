module test_texture_packer_suite
  use :: texture_packer_mod
  implicit none


contains


  subroutine begin_test()
    implicit none

    type(texture_packer_conf) :: config
    type(texture_packer) :: packer

    config%trim = .false.

    packer = texture_packer(config)

  end subroutine


end module test_texture_packer_suite


program test_texture_packer
  use test_texture_packer_suite
  implicit none


  call begin_test()


end program test_texture_packer
