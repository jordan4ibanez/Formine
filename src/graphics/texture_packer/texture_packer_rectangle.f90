module texture_packer_rectangle
  use, intrinsic :: iso_c_binding
  implicit none


  private

  public :: rect
  public :: rect_new_with_points


  !* Defines a rectangle in pixels with the origin at the top-left of the texture atlas.
  type :: rect
    integer(c_int) :: x = 1
    integer(c_int) :: y = 1
    integer(c_int) :: w = 0
    integer(c_int) :: h = 0
  contains
    procedure :: top => rect_top
    procedure :: bottom => rect_bottom
    procedure :: left => rect_left
    procedure :: right => rect_right

    procedure :: area => rect_area
    procedure :: intersects => rect_intersects
    procedure :: contains => rect_contains
    procedure :: contains_point => rect_contains_point
    procedure :: is_outline => rect_is_outline
    procedure :: crop => rect_crop
    procedure :: from => rect_from_rgba8_texture
  end type rect


  interface rect
    module procedure :: constructor_rect
  end interface rect


contains


  !* Create a new [Rect] based on a position and its width and height.
  function constructor_rect(x, y, w, h) result(new_rect)
    implicit none

    integer(c_int), intent(in), value :: x, y, w, h
    type(rect) :: new_rect

    new_rect%x = x
    new_rect%y = y
    new_rect%w = w
    new_rect%h = h
  end function constructor_rect


  !* Create a new [Rect] based on two points spanning the rectangle.
  function rect_new_with_points(x1, y1, x2, y2) result(new_rect)
    implicit none

    integer(c_int), intent(in), value :: x1, y1, x2, y2
    type(rect) :: new_rect

    new_rect%x = x1
    new_rect%y = y1
    new_rect%w = x2 - x1 + 1
    new_rect%h = y2 - y1 + 1
  end function rect_new_with_points


  !* Get the top coordinate of the rectangle.
  function rect_top(this) result(this_top)
    implicit none

    class(rect), intent(in) :: this
    integer(c_int) :: this_top

    this_top = this%y
  end function rect_top


  !* Get the bottom coordinate of the rectangle.
  function rect_bottom(this) result(this_bottom)
    implicit none

    class(rect), intent(in) :: this
    integer(c_int) :: this_bottom

    this_bottom = this%y + this%h - 1
  end function rect_bottom


  !* Get the left coordinate of the rectangle.
  function rect_left(this) result(this_left)
    implicit none

    class(rect), intent(in) :: this
    integer(c_int) :: this_left

    this_left = this%x
  end function rect_left


  !* Get the right coordinate of the rectangle.
  function rect_right(this) result(this_right)
    implicit none

    class(rect), intent(in) :: this
    integer(c_int) :: this_right

    this_right = this%x + this%w - 1
  end function rect_right


  !* Get the area of the rectangle.
  function rect_area(this) result(this_area)
    implicit none

    class(rect), intent(in) :: this
    integer(c_int) :: this_area

    this_area = this%w * this%h
  end function rect_area


  !* Check if this rectangle intersects with another.
  function rect_intersects(this, other) result(does_intersect)
    implicit none

    class(rect), intent(in) :: this
    type(rect), intent(in) :: other
    logical(c_bool) :: does_intersect

    does_intersect = this%left() < other%right() .and. &
      this%right() > other%left() .and. &
      this%top() < other%bottom() .and. &
      this%bottom() > other%top()
  end function rect_intersects


  !* Check if this rectangle contains another.
  function rect_contains(this, other) result(does_contain)
    implicit none

    class(rect), intent(in) :: this
    type(rect), intent(in) :: other
    logical(c_bool) :: does_contain

    does_contain = this%left() <= other%left() .and. &
      this%right() >= other%right() .and. &
      this%top() <= other%top() .and. &
      this%bottom() >= other%bottom()
  end function rect_contains


  !* Check if this rectangle contains a point. Includes the edges of the rectangle.
  function rect_contains_point(this, x, y) result(does_contain)
    implicit none

    class(rect), intent(in) :: this
    integer(c_int), intent(in), value :: x, y
    logical(c_bool) :: does_contain

    does_contain = this%left() <= x .and. this%right() >= x .and. this%top() <= y .and. this%bottom() >= y
  end function rect_contains_point


  !* Check if a point falls on the rectangle's boundaries.
  function rect_is_outline(this, x, y) result(is_outline)
    implicit none

    class(rect), intent(in) :: this
    integer(c_int), intent(in), value :: x, y
    logical(c_bool) :: is_outline

    is_outline = x == this%left() .or. x == this%right() .or. y == this%top() .or. y == this%bottom()
  end function rect_is_outline

  !* Split two rectangles into non-overlapping regions.
  function rect_crop(this, other) result(rect_array)
    implicit none

    class(rect), intent(in) :: this
    type(rect), intent(in) :: other
    type(rect), dimension(:), allocatable :: rect_array
    integer(c_int) :: inside_x1, inside_y1, inside_x2, inside_y2
    type(rect) :: r1, r2, r3, r4

    allocate(rect_array(0))

    if (.not. this%intersects(other)) then
      rect_array = [rect_array, this]
      return
    end if

    if (other%left() < this%left()) then
      inside_x1 = this%left()
    else
      inside_x1 = other%left()
    end if

    if (other%top() < this%top()) then
      inside_y1 = this%top()
    else
      inside_y1 = other%top()
    end if

    if (other%right() > this%right()) then
      inside_x2 = this%right()
    else
      inside_x2 = other%right()
    end if

    if (other%bottom() > this%bottom()) then
      inside_y2 = this%bottom()
    else
      inside_y2 = other%bottom()
    end if

    !
    ! *******************
    ! *    | r3  |      *
    ! *    |     |      *
    ! *    +++++++      *
    ! * r1 +     +      *
    ! *    +     +  r2  *
    ! *    +++++++      *
    ! *    |     |      *
    ! *    | r4  |      *
    ! *******************
    !

    r1 = rect_new_with_points(this%left(), this%top(), inside_x1, this%bottom());
    if (r1%area() > 0) then
      rect_array = [rect_array, r1]
    end if

    r2 = rect_new_with_points(inside_x2, this%top(), this%right(), this%bottom());
    if (r2%area() > 0) then
      rect_array = [rect_array, r2]
    end if

    r3 = rect_new_with_points(inside_x1, this%top(), inside_x2, inside_y1);
    if (r3%area() > 0) then
      rect_array = [rect_array, r3]
    end if

    r4 = rect_new_with_points(inside_x1, inside_y2, inside_x2, this%bottom());
    if (r4%area() > 0) then
      rect_array = [rect_array, r4]
    end if
  end function rect_crop


  !* Allow translating the dimensions from an rgba8_texture into a rectangle.
  subroutine rect_from_rgba8_texture(this, texture)
    use :: rgba8_texture_module
    implicit none

    class(rect), intent(inout) :: this
    type(rgba8_texture), intent(in) :: texture

    this%x = 0
    this%y = 0
    this%w = texture%width
    this%h = texture%height
  end subroutine rect_from_rgba8_texture


end module texture_packer_rectangle
