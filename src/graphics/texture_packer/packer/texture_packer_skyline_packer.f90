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

!* SKYLINE PACKER. =================================================================================

pub fn new(config: TexturePackerConfig) -> Self {
  let skylines = vec![Skyline {
      x: 0,
      y: 0,
      w: config.max_width,
  }];

  SkylinePacker {
      config,
      border: Rect::new(0, 0, config.max_width, config.max_height),
      skylines,
  }
}

// return `rect` if rectangle (w, h) can fit the skyline started at `i`
fn can_put(&self, mut i: usize, w: u32, h: u32) -> Option<Rect> {
  let mut rect = Rect::new(self.skylines[i].x, 0, w, h);
  let mut width_left = rect.w;
  loop {
      rect.y = max(rect.y, self.skylines[i].y);
      // the source rect is too large
      if !self.border.contains(&rect) {
          return None;
      }
      if self.skylines[i].w >= width_left {
          return Some(rect);
      }
      width_left -= self.skylines[i].w;
      i += 1;
      assert!(i < self.skylines.len());
  }
}

fn find_skyline(&self, w: u32, h: u32) -> Option<(usize, Rect)> {
  let mut bottom = std::u32::MAX;
  let mut width = std::u32::MAX;
  let mut index = None;
  let mut rect = Rect::new(0, 0, 0, 0);

  // keep the `bottom` and `width` as small as possible
  for i in 0..self.skylines.len() {
      if let Some(r) = self.can_put(i, w, h) {
          if r.bottom() < bottom || (r.bottom() == bottom && self.skylines[i].w < width) {
              bottom = r.bottom();
              width = self.skylines[i].w;
              index = Some(i);
              rect = r;
          }
      }

      if self.config.allow_rotation {
          if let Some(r) = self.can_put(i, h, w) {
              if r.bottom() < bottom || (r.bottom() == bottom && self.skylines[i].w < width) {
                  bottom = r.bottom();
                  width = self.skylines[i].w;
                  index = Some(i);
                  rect = r;
              }
          }
      }
  }

  index.map(|x| (x, rect))
}

fn split(&mut self, index: usize, rect: &Rect) {
  let skyline = Skyline {
      x: rect.left(),
      y: rect.bottom() + 1,
      w: rect.w,
  };

  assert!(skyline.right() <= self.border.right());
  assert!(skyline.y <= self.border.bottom());

  self.skylines.insert(index, skyline);

  let i = index + 1;
  while i < self.skylines.len() {
      assert!(self.skylines[i - 1].left() <= self.skylines[i].left());

      if self.skylines[i].left() <= self.skylines[i - 1].right() {
          let shrink = self.skylines[i - 1].right() - self.skylines[i].left() + 1;
          if self.skylines[i].w <= shrink {
              self.skylines.remove(i);
          } else {
              self.skylines[i].x += shrink;
              self.skylines[i].w -= shrink;
              break;
          }
      } else {
          break;
      }
  }
}

fn merge(&mut self) {
  let mut i = 1;
  while i < self.skylines.len() {
      if self.skylines[i - 1].y == self.skylines[i].y {
          self.skylines[i - 1].w += self.skylines[i].w;
          self.skylines.remove(i);
          i -= 1;
      }
      i += 1;
  }
}

fn pack(&mut self, key: K, texture_rect: &Rect) -> Option<Frame<K>> {
    let mut width = texture_rect.w;
    let mut height = texture_rect.h;

    width += self.config.texture_padding + self.config.texture_extrusion * 2;
    height += self.config.texture_padding + self.config.texture_extrusion * 2;

    if let Some((i, mut rect)) = self.find_skyline(width, height) {
        self.split(i, &rect);
        self.merge();

        let rotated = width != rect.w;

        rect.w -= self.config.texture_padding + self.config.texture_extrusion * 2;
        rect.h -= self.config.texture_padding + self.config.texture_extrusion * 2;

        Some(Frame {
            key,
            frame: rect,
            rotated,
            trimmed: false,
            source: Rect {
                x: 0,
                y: 0,
                w: texture_rect.w,
                h: texture_rect.h,
            },
        })
    } else {
        None
    }
}

fn can_pack(&self, texture_rect: &Rect) -> bool {
    if let Some((_, rect)) = self.find_skyline(
        texture_rect.w + self.config.texture_padding + self.config.texture_extrusion * 2,
        texture_rect.h + self.config.texture_padding + self.config.texture_extrusion * 2,
    ) {
        let skyline = Skyline {
            x: rect.left(),
            y: rect.bottom() + 1,
            w: rect.w,
        };

        return skyline.right() <= self.border.right() && skyline.y <= self.border.bottom();
    }
    false
}

end module texture_packer_skyline_packer


