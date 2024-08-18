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
  contains
    procedure :: can_put => skyline_packer_can_put
    procedure :: find_skyline => skyline_packer_find_skyline
  end type skyline_packer


  interface skyline_packer
    module procedure :: constructor_skyline_packer
  end interface skyline_packer

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

!* SKYLINE PACKER. =================================================================================

  function constructor_skyline_packer(config) result(new_skyline_packer)
    implicit none

    type(texture_packer_conf), intent(in) :: config
    type(skyline_packer) :: new_skyline_packer
    type(skyline), dimension(:), allocatable :: skylines

    allocate(skylines(0))

    skylines = [skylines, skyline(0, 0, config%max_width)]

    new_skyline_packer = skyline_packer(config, rect(0, 0, config%max_width, config%max_height), skylines)
  end function constructor_skyline_packer


  !* return `rect` if rectangle (w, h) can fit the skyline started at `i`
  !* This had to be reworked because Fortran has a different style.
  !* This will always return, but it will signal if it could put it or not via [can_put]
  function skyline_packer_can_put(this, i, w, h, option_rectangle) result(can_put)
    implicit none

    class(skyline_packer), intent(in) :: this
    integer(c_int), intent(inout) :: i
    integer(c_int), intent(in), value :: w, h
    type(rect), intent(inout) :: option_rectangle
    logical(c_bool) :: can_put
    integer(c_int) :: width_left

    can_put = .false.
    option_rectangle = rect(this%skylines(i)%x, 0, w, h)
    width_left = option_rectangle%w

    do
      option_rectangle%y = max(option_rectangle%y, this%skylines(i)%y)
      ! the source rect is too large
      if (.not. this%border%contains(option_rectangle)) then
        can_put = .false.
        return
      end if

      if (this%skylines(i)%w >= width_left) then
        option_rectangle = option_rectangle
        can_put = .true.
        return
      end if

      width_left = width_left - this%skylines(i)%w
      i = i + 1

      if (i >= size(this%skylines)) then
        error stop "[Skyline Packer] Error: Out of bounds."
      end if
    end do
  end function skyline_packer_can_put


  !* This had to be reworked because Fortran has a different style.
  !* This will always return, but it will signal if it could put it or not via [can_find]
  function skyline_packer_find_skyline(this, w, h, option_rectangle, option_index) result(can_find)
    use :: constants
    implicit none

    class(skyline_packer), intent(in) :: this
    integer(c_int), intent(in), value :: w, h
    type(rect), intent(inout) :: option_rectangle
    integer(c_int), intent(inout) :: option_index
    logical(c_bool) :: can_find
    type(rect) :: r
    integer(c_int) :: bottom, width, i

    bottom = C_INT_MAX
    width = C_INT_MAX
    option_index = 0
    option_rectangle = rect(0, 0, 0, 0)
    r = rect(0, 0, 0, 0)
    can_find = .false.

    ! keep the `bottom` and `width` as small as possible
    do i = 0, size(this%skylines)
      if (this%can_put(i, w, h, r))  then
        if (r%bottom() < bottom .or. (r%bottom() == bottom .and. this%skylines(i)%w < width)) then
          bottom = r%bottom();
          width = this%skylines(i)%w;
          option_index = i
          can_find = .true.
          option_rectangle = r;
        end if
      end if

      if (this%config%allow_rotation) then
        if (this%can_put(i, h, w, r)) then
          if (r%bottom() < bottom .or. (r%bottom() == bottom .and. this%skylines(i)%w < width)) then
            bottom = r%bottom();
            width = this%skylines(i)%w;
            option_index = i;
            can_find = .true.
            option_rectangle = r;
          end if
        end if
      end if
    end do
  end function skyline_packer_find_skyline

! fn split(&mut self, index: usize, rect: &Rect) {
!   let skyline = Skyline {
!       x: rect.left(),
!       y: rect.bottom() + 1,
!       w: rect.w,
!   };

!   assert!(skyline.right() <= self.border.right());
!   assert!(skyline.y <= self.border.bottom());

!   self.skylines.insert(index, skyline);

!   let i = index + 1;
!   while i < self.skylines.len() {
!       assert!(self.skylines[i - 1].left() <= self.skylines[i].left());

!       if self.skylines[i].left() <= self.skylines[i - 1].right() {
!           let shrink = self.skylines[i - 1].right() - self.skylines[i].left() + 1;
!           if self.skylines[i].w <= shrink {
!               self.skylines.remove(i);
!           } else {
!               self.skylines[i].x += shrink;
!               self.skylines[i].w -= shrink;
!               break;
!           }
!       } else {
!           break;
!       }
!   }
! }

! fn merge(&mut self) {
!   let mut i = 1;
!   while i < self.skylines.len() {
!       if self.skylines[i - 1].y == self.skylines[i].y {
!           self.skylines[i - 1].w += self.skylines[i].w;
!           self.skylines.remove(i);
!           i -= 1;
!       }
!       i += 1;
!   }
! }

! fn pack(&mut self, key: K, texture_rect: &Rect) -> Option<Frame<K>> {
!     let mut width = texture_rect.w;
!     let mut height = texture_rect.h;

!     width += self.config.texture_padding + self.config.texture_extrusion * 2;
!     height += self.config.texture_padding + self.config.texture_extrusion * 2;

!     if let Some((i, mut rect)) = self.find_skyline(width, height) {
!         self.split(i, &rect);
!         self.merge();

!         let rotated = width != rect.w;

!         rect.w -= self.config.texture_padding + self.config.texture_extrusion * 2;
!         rect.h -= self.config.texture_padding + self.config.texture_extrusion * 2;

!         Some(Frame {
!             key,
!             frame: rect,
!             rotated,
!             trimmed: false,
!             source: Rect {
!                 x: 0,
!                 y: 0,
!                 w: texture_rect.w,
!                 h: texture_rect.h,
!             },
!         })
!     } else {
!         None
!     }
! }

! fn can_pack(&self, texture_rect: &Rect) -> bool {
!     if let Some((_, rect)) = self.find_skyline(
!         texture_rect.w + self.config.texture_padding + self.config.texture_extrusion * 2,
!         texture_rect.h + self.config.texture_padding + self.config.texture_extrusion * 2,
!     ) {
!         let skyline = Skyline {
!             x: rect.left(),
!             y: rect.bottom() + 1,
!             w: rect.w,
!         };

!         return skyline.right() <= self.border.right() && skyline.y <= self.border.bottom();
!     }
!     false
! }

end module texture_packer_skyline_packer


