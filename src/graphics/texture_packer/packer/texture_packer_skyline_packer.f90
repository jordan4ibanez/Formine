module texture_packer_skyline_packer
  use :: texture_packer_frame
  use :: texture_packer_rectangle
  use :: texture_packer_config
  use, intrinsic :: iso_c_binding
  implicit none

  !* This is not wise to make this one module, but I will mimic the Rust impl.
  !* Nothing is documented, so I will mirror that.

  type :: skyline
    integer(c_int) :: x = 0
    integer(c_int) :: y = 0
    integer(c_int) :: w = 0
  contains
    procedure :: left => skyline_left
    procedure :: right => skyline_right
  end type skyline


  type :: skyline_packer
    type(texture_packer_conf) :: config
    type(rect) :: border
    type(skyline), dimension(:), allocatable :: skylines
  end type skyline_packer


contains


!* SKYLINE. =================================================================================


  function skyline_left(this) result(left)
    implicit none

    class(skyline), intent(in) :: this
    integer(c_int) :: left

    left = this%x
  end function skyline_left


  function skyline_right(this) result(right)
    implicit none

    class(skyline), intent(in) :: this
    integer(c_int) :: right

    right = this%x + this%w - 1
  end function skyline_right

end module texture_packer_skyline_packer
