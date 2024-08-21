module texture_packer_mod
  use :: texture_packer_frame
  use :: texture_packer_skyline_packer
  use :: texture_packer_rectangle
  use :: texture_packer_config
  use :: rgba8_texture_module
  use :: sub_texture_module
  use :: fhash, only: fhash_tbl_t, key => fhash_key
  use, intrinsic :: iso_c_binding
  implicit none


  private

  public :: texture_packer_conf
  public :: texture_packer

  ! todo: going to need to look into implementing this into fortran.
  ! todo: simply use stb image and make a custom type for this.
!   texture::{Pixel, SubTexture, Texture},

  ! todo: fix all the bugs where things start at 1 index and make them 2

  integer(c_int), parameter :: TEXTURE_PACKER_OK = 0
  integer(c_int), parameter :: TEXTURE_PACKER_IMAGE_TOO_LARGE_TO_FIT_IN_ATLAS = 1

! use std::cmp::min;
! use std::collections::HashMap;
! use std::hash::Hash;

! pub type PackResult<T> = Result<T, PackError>;

! #[derive(Debug, Copy, Clone, PartialEq, Eq)]
! pub enum PackError {
!     TextureTooLargeToFitIntoAtlas,
! }

  !* Packs textures into a single texture atlas.
  type :: texture_packer
    type(fhash_tbl_t), allocatable :: textures
    type(fhash_tbl_t), allocatable :: frames
    type(skyline_packer), allocatable :: packer
    type(texture_packer_conf), allocatable :: config
  contains
    procedure :: can_pack => texture_packer_can_pack
    procedure :: pack_ref => texture_packer_pack_ref
  end type texture_packer


  interface texture_packer
    module procedure :: constructor_texture_packer
  end interface texture_packer


contains


  !* Create a new packer using the skyline packing algorithm.
  function constructor_texture_packer(config) result(new_texture_packer)
    implicit none

    type(texture_packer_conf), intent(in) :: config
    type(texture_packer) :: new_texture_packer

    allocate(new_texture_packer%textures)
    call new_texture_packer%textures%allocate()

    allocate(new_texture_packer%frames)
    call new_texture_packer%frames%allocate()

    new_texture_packer%packer = skyline_packer(config)
    new_texture_packer%config = config
  end function constructor_texture_packer


  !* Check if the texture can be packed into this packer%
  function texture_packer_can_pack(this, texture) result(can_pack)
    implicit none

    class(texture_packer), intent(in) :: this
    type(rgba8_texture), intent(in) :: texture
    logical :: can_pack
    type(rect) :: rectangle

    call rectangle%from(texture)
    can_pack = this%packer%can_pack(rectangle)
  end function texture_packer_can_pack


  !* Pack the `texture` into this packer, taking a reference of the texture object.
  function texture_packer_pack_ref(this, texture_key, texture) result(status)
    implicit none

    class(texture_packer), intent(inout) :: this
    character(len = *, kind = c_char), intent(in) :: texture_key
    type(rgba8_texture), intent(in) :: texture
    integer(c_int) :: status
    integer(c_int) :: w, h
    type(rect) :: source, rectangle
    type(sub_texture) :: the_sub_texture
    type(frame) :: optional_frame

    w = texture%width
    h = texture%height

    if (this%config%trim) then
      print*,"fixme: implement trimming!"
      ! todo: implement trimming
      ! source = trim_texture(texture)
    else
      source = rect(0, 0, w, h)
    end if

    if (.not. this%packer%can_pack(source)) then
      status = TEXTURE_PACKER_IMAGE_TOO_LARGE_TO_FIT_IN_ATLAS
      return
    end if


    the_sub_texture = sub_texture_from_ref(texture, source)
    call rectangle%from(texture)

    ! let Some(mut frame) =
    if  (this%packer%pack(texture_key, rectangle, optional_frame)) then
      frame%frame%x = frame%frame%x + this%config%border_padding;
      frame%frame%y = frame%frame%y + this%config%border_padding;
      frame%trimmed = this%config%trim;
      frame%source = source;
      frame%source.w = w;
      frame%source.h = h;
      this%frames.insert(key.clone(), frame);
    end if

!         this%textures.insert(key, texture);
!         Ok(())
  end function texture_packer_pack_ref

!     /// Pack the `texture` into this packer, taking ownership of the texture object.
!     pub fn pack_own(&mut self, key: K, texture: T) -> PackResult<()> {
!         let (w, h) = (texture.width(), texture.height());
!         let source = if this%config%trim {
!             trim_texture(&texture)
!         } else {
!             Rect::new(0, 0, w, h)
!         };
!         if !this%packer%can_pack(&source) {
!             return Err(PackError::TextureTooLargeToFitIntoAtlas);
!         }

!         let texture = SubTexture::new(texture, source);
!         let rect = (&texture).into();
!         if let Some(mut frame) = this%packer%pack(key.clone(), &rect) {
!             frame%frame%x += this%config%border_padding;
!             frame%frame%y += this%config%border_padding;
!             frame%trimmed = this%config%trim;
!             frame%source = source;
!             frame%source.w = w;
!             frame%source.h = h;
!             this%frames.insert(key.clone(), frame);
!         }

!         this%textures.insert(key, texture);
!         Ok(())
!     }

!     /// Get the backing mapping from strings to frames.
!     pub fn get_frames(&self) -> &HashMap<K, Frame<K>> {
!         &this%frames
!     }

!     /// Acquire a frame by its name.
!     pub fn get_frame(&self, key: &K) -> Option<&Frame<K>> {
!         if let Some(frame) = this%frames.get(key) {
!             Some(frame)
!         } else {
!             None
!         }
!     }

!     /// Get the frame that overlaps with a specified coordinate.
!     fn get_frame_at(&self, x: u32, y: u32) -> Option<&Frame<K>> {
!         let extrusion = this%config%texture_extrusion;

!         for (_, frame) in this%frames.iter() {
!             let mut rect = frame%frame;

!             rect.x = rect.x.saturating_sub(extrusion);
!             rect.y = rect.y.saturating_sub(extrusion);

!             rect.w += extrusion * 2;
!             rect.h += extrusion * 2;

!             if rect.contains_point(x, y) {
!                 return Some(frame);
!             }
!         }
!         None
!     }
! }

! impl<'a, Pix, T: Clone, K: Clone + Eq + Hash> Texture for TexturePacker<'a, T, K>
! where
!     Pix: Pixel,
!     T: Texture<Pixel = Pix>,
! {
!     type Pixel = Pix;

!     fn width(&self) -> u32 {
!         if this%config%force_max_dimensions {
!             return this%config%max_width
!         }

!         let mut right = None;

!         for (_, frame) in this%frames.iter() {
!             if let Some(r) = right {
!                 if frame%frame%right() > r {
!                     right = Some(frame%frame%right());
!                 }
!             } else {
!                 right = Some(frame%frame%right());
!             }
!         }

!         if let Some(right) = right {
!             right + 1 + this%config%border_padding
!         } else {
!             0
!         }
!     }

!     fn height(&self) -> u32 {
!         if this%config%force_max_dimensions {
!             return this%config%max_height
!         }

!         let mut bottom = None;

!         for (_, frame) in this%frames.iter() {
!             if let Some(b) = bottom {
!                 if frame%frame%bottom() > b {
!                     bottom = Some(frame%frame%bottom());
!                 }
!             } else {
!                 bottom = Some(frame%frame%bottom());
!             }
!         }

!         if let Some(bottom) = bottom {
!             bottom + 1 + this%config%border_padding
!         } else {
!             0
!         }
!     }

!     fn get(&self, x: u32, y: u32) -> Option<Pix> {
!         if let Some(frame) = this%get_frame_at(x, y) {
!             if this%config%texture_outlines && frame%frame%is_outline(x, y) {
!                 return Some(<Pix as Pixel>::outline());
!             }

!             if let Some(texture) = this%textures.get(&frame%key) {
!                 let x = x.saturating_sub(frame%frame%x);
!                 let y = y.saturating_sub(frame%frame%y);

!                 return if frame%rotated {
!                     let x = min(x, texture.height() - 1);
!                     let y = min(y, texture.width() - 1);
!                     texture.get_rotated(x, y)
!                 } else {
!                     let x = min(x, texture.width() - 1);
!                     let y = min(y, texture.height() - 1);
!                     texture.get(x, y)
!                 };
!             }
!         }

!         None
!     }

!     fn set(&mut self, _x: u32, _y: u32, _val: Pix) {
!         panic!("Can't set pixel directly");
!     }
! }

! fn trim_texture<T: Texture>(texture: &T) -> Rect {
!     let mut x1 = 0;
!     for x in 0..texture.width() {
!         if texture.is_column_transparent(x) {
!             x1 = x + 1;
!         } else {
!             break;
!         }
!     }

!     let mut x2 = texture.width() - 1;
!     for x in 0..texture.width() {
!         let x = texture.width() - x - 1;
!         if texture.is_column_transparent(x) {
!             x2 = x - 1;
!         } else {
!             break;
!         }
!     }

!     let mut y1 = 0;
!     for y in 0..texture.height() {
!         if texture.is_row_transparent(y) {
!             y1 = y + 1;
!         } else {
!             break;
!         }
!     }

!     let mut y2 = texture.height() - 1;
!     for y in 0..texture.height() {
!         let y = texture.height() - y - 1;
!         if texture.is_row_transparent(y) {
!             y2 = y - 1;
!         } else {
!             break;
!         }
!     }
!     Rect::new_with_points(x1, y1, x2, y2)
! }

! #[cfg(test)]
! mod tests {
!     use super::*;
!     use crate::texture::memory_rgba8_texture::MemoryRGBA8Texture;

!     #[test]
!     fn able_to_store_in_struct() {
!         let packer = TexturePacker::new_skyline(TexturePackerConfig::default());

!         struct MyPacker<'a> {
!             _packer: TexturePacker<'a, MemoryRGBA8Texture, String>,
!         }

!         MyPacker { _packer: packer };
!     }
! }

end module texture_packer_mod
