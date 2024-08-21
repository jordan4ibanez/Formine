module sub_texture_module
  use :: texture_packer_rectangle
  use :: rgba8_texture_module
  use, intrinsic :: iso_c_binding
  implicit none


  private


  public :: sub_texture
  public :: sub_texture_from_ref


  !* Texture that is a subset of another texture%
  type :: sub_texture
    type(rgba8_texture), pointer :: texture
    type(rect) :: source
  contains
    procedure :: width => sub_texture_width
    procedure :: height => sub_texture_height
    procedure :: get => sub_texture_get
  end type sub_texture


  interface sub_texture
    module procedure :: constructor_sub_texture
  end interface sub_texture


contains


  !* Get a `SubTexture` owning the parent texture%
  function constructor_sub_texture(texture, source) result(new_sub_texture)
    implicit none

    type(rgba8_texture), intent(in), target :: texture
    type(rect), intent(in) :: source
    type(sub_texture) :: new_sub_texture

    new_sub_texture%texture => texture
    new_sub_texture%source = source
  end function constructor_sub_texture


  !* Get a `SubTexture` referencing the parent texture%
  function sub_texture_from_ref(texture, source) result(new_sub_texture)
    implicit none

    type(rgba8_texture), intent(in), target :: texture
    type(rect), intent(in) :: source
    type(sub_texture) :: new_sub_texture

    new_sub_texture%texture => texture
    new_sub_texture%source = source
  end function sub_texture_from_ref


  function sub_texture_width(this) result(the_width)
    implicit none

    class(sub_texture), intent(in) :: this
    integer(c_int) :: the_width

    the_width = this%source%w
  end function sub_texture_width


  function sub_texture_height(this) result(the_height)
    implicit none

    class(sub_texture), intent(in) :: this
    integer(c_int) :: the_height

    the_height = this%source%h
  end function sub_texture_height


  function sub_texture_get(this, x, y, optional_pixel) result(get_success)
    implicit none

    class(sub_texture), intent(in) :: this
    integer(c_int), intent(in), value :: x, y
    type(rgba8_pixel), intent(inout) :: optional_pixel
    logical :: get_success
    integer(c_int) :: a, b

    a = this%source%x + x;
    b = this%source%y + y;

    get_success = this%texture%get_color_optional(a, b, optional_pixel)
  end function sub_texture_get


  subroutine sub_texture_set(this, x, y, value)
    implicit none

    class(sub_texture), intent(in) :: this
    integer(c_int), intent(in), value :: x, y
    type(rgba8_pixel), intent(in) :: value
    integer(c_int) :: a, b

    if (associated(this%texture)) then
      a = this%source%x + x;
      b = this%source%y + y;
      call this%texture%set_pixel(a, b, value);
    else
      error stop "[Sub Texture] Error: Cannot set pixel, no texture pointed to."
    end if
  end subroutine sub_texture_set


end module sub_texture_module
