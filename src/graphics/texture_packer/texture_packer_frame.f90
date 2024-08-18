module texture_packer_frame
  use, intrinsic :: iso_c_binding
  implicit none

  ! Boundaries and properties of a packed texture.
  type :: frame
    character(len = :, kind = c_char), allocatable :: key
    
  end type frame
end module texture_packer_frame
