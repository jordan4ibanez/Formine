module f_string
  use, intrinsic :: iso_c_binding
  implicit none

  type string
    character(:), allocatable :: data
  end type string

  private
contains

end module f_string
