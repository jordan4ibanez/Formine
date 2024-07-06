module f_string
  use, intrinsic :: iso_c_binding
  implicit none

  private

  type string
    character(:), allocatable :: data
  end type string

  

contains

end module f_string
