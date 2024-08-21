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

  public :: TEXTURE_PACKER_OK
  public :: TEXTURE_PACKER_IMAGE_TOO_LARGE_TO_FIT_IN_ATLAS
  public :: TEXTURE_PACKER_WRONG_TYPE
  public :: TEXTURE_PACKER_NOT_FOUND

  ! todo: going to need to look into implementing this into fortran.
  ! todo: simply use stb image and make a custom type for this.
!   texture::{Pixel, SubTexture, Texture},

  ! todo: fix all the bugs where things start at 1 index and make them 2
  !*
  !* Implementation note: The intrinsics of rust are different from fortran.
  !* You're going to see a bunch of duplicate and nonsensical things while I prototype this.

  integer(c_int), parameter :: TEXTURE_PACKER_OK = 0
  integer(c_int), parameter :: TEXTURE_PACKER_IMAGE_TOO_LARGE_TO_FIT_IN_ATLAS = 1
  ! I hope I never hit this return.
  integer(c_int), parameter :: TEXTURE_PACKER_WRONG_TYPE = 2
  integer(c_int), parameter :: TEXTURE_PACKER_NOT_FOUND = 3



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
    !! This doesn't make any sense to have two of these lol.
    ! fixme: undo this nonsense.
    procedure :: pack_ref => texture_packer_pack_ref
    procedure :: pack_own => texture_packer_pack_own
    procedure :: get_frames => texture_packer_get_frames
    procedure :: get_frame => texture_packer_get_frame
    procedure :: get_frame_at => texture_packer_get_frame_at
    procedure :: width => texture_packer_width
    procedure :: height => texture_packer_height
    procedure :: get => texture_packer_get
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
    integer(c_int) :: status, w, h
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

    if (this%packer%pack(texture_key, rectangle, optional_frame)) then
      optional_frame%frame%x = optional_frame%frame%x + this%config%border_padding;
      optional_frame%frame%y = optional_frame%frame%y + this%config%border_padding;
      optional_frame%trimmed = this%config%trim;
      optional_frame%source = source;
      optional_frame%source%w = w;
      optional_frame%source%h = h;

      call this%frames%set(key(texture_key), optional_frame)
    end if

    call this%textures%set(key(texture_key), texture);

    status = TEXTURE_PACKER_OK
  end function texture_packer_pack_ref

  !* Pack the `texture` into this packer, taking ownership of the texture object.
  function texture_packer_pack_own(this, texture_key, texture) result(status)
    implicit none

    class(texture_packer), intent(inout) :: this
    character(len = *, kind = c_char), intent(in) :: texture_key
    type(rgba8_texture), intent(in) :: texture
    integer(c_int) :: status, w, h
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

    if (this%packer%pack(texture_key, rectangle, optional_frame)) then
      optional_frame%frame%x = optional_frame%frame%x + this%config%border_padding;
      optional_frame%frame%y = optional_frame%frame%y + this%config%border_padding;
      optional_frame%trimmed = this%config%trim;
      optional_frame%source = source;
      optional_frame%source%w = w;
      optional_frame%source%h = h;

      call this%frames%set(key(texture_key), optional_frame)
    end if

    call this%textures%set(key(texture_key), texture);

    status = TEXTURE_PACKER_OK
  end function texture_packer_pack_own


  !* Get the backing mapping from strings to frames.
  function texture_packer_get_frames(this) result(the_frames)
    implicit none

    class(texture_packer), intent(in), target :: this
    type(fhash_tbl_t), pointer :: the_frames

    the_frames => this%frames
  end function texture_packer_get_frames


  !* Acquire a frame by its name.
  function texture_packer_get_frame(this, frame_key, optional_frame) result(status)
    implicit none

    class(texture_packer), intent(in), target :: this
    character(len = *, kind = c_char), intent(in) :: frame_key
    type(frame), intent(inout) :: optional_frame
    class(*), allocatable :: generic_data
    integer :: status

    call this%frames%get_raw(key(frame_key), generic_data, stat = status)

    if (status == 0) then
      select type(generic_data)
       type is (frame)
        optional_frame = generic_data
       class default
        status = TEXTURE_PACKER_WRONG_TYPE
      end select
    else
      status = TEXTURE_PACKER_NOT_FOUND
    end if
  end function texture_packer_get_frame


  !* Get the frame that overlaps with a specified coordinate.
  function texture_packer_get_frame_at(this, x, y, optional_frame) result(status)
    use :: math_helpers, only: saturating_sub
    use :: fhash, only: fhash_iter_t, fhash_key_t
    implicit none

    class(texture_packer), intent(in), target :: this
    integer(c_int), intent(in), value :: x, y
    type(frame), intent(inout) :: optional_frame
    integer(c_int) :: status
    integer(c_int) :: extrusion
    type(fhash_iter_t) :: iterator
    class(fhash_key_t), allocatable :: generic_key
    class(*), allocatable :: generic_data
    type(frame) :: worker_frame
    type(rect) :: rectangle

    extrusion = this%config%texture_extrusion;

    iterator = fhash_iter_t(this%frames)

    do while(iterator%next(generic_key, generic_data))
      select type (generic_data)
       type is (frame)
        worker_frame = generic_data
       class default
        error stop "[Texture Packer] Error: How did this type get in here?"
      end select

      rectangle = worker_frame%frame

      rectangle%x = saturating_sub(rectangle%x, extrusion, 0)
      rectangle%y = saturating_sub(rectangle%y, extrusion, 0);

      rectangle%w = rectangle%w + (extrusion * 2);
      rectangle%h = rectangle%h + (extrusion * 2);

      if (rectangle%contains_point(x, y)) then
        optional_frame = worker_frame
        status = TEXTURE_PACKER_OK
        return
      end if
    end do

    status = TEXTURE_PACKER_NOT_FOUND
  end function texture_packer_get_frame_at


  function texture_packer_width(this) result(the_width)
    use :: fhash, only: fhash_iter_t, fhash_key_t
    use :: constants
    implicit none

    class(texture_packer), intent(in) :: this
    integer(c_int) :: the_width
    type(fhash_iter_t) :: iterator
    class(fhash_key_t), allocatable :: generic_key
    class(*), allocatable :: generic_data
    type(frame) :: worker_frame
    logical :: found_right
    integer(c_int) :: the_right


    if (this%config%force_max_dimensions) then
      the_width = this%config%max_width
      return
    end if

    found_right = .false.
    the_right = C_INT_MIN

    iterator = fhash_iter_t(this%frames)

    do while(iterator%next(generic_key, generic_data))
      select type (generic_data)
       type is (frame)
        worker_frame = generic_data
       class default
        error stop "[Texture Packer] Error: How did this type get in here?"
      end select

      if (found_right) then
        if (worker_frame%frame%right() > the_right) then
          found_right = .true.
          the_right = worker_frame%frame%right();
        end if
      else
        found_right = .true.
        the_right = worker_frame%frame%right();
      end if
    end do

    if (found_right) then
      the_width = the_right + 1 + this%config%border_padding
    else
      the_width = 0
    end if
  end function texture_packer_width


  function texture_packer_height(this) result(the_height)
    use :: fhash, only: fhash_iter_t, fhash_key_t
    use :: constants
    implicit none

    class(texture_packer), intent(in) :: this
    integer(c_int) :: the_height
    type(fhash_iter_t) :: iterator
    class(fhash_key_t), allocatable :: generic_key
    class(*), allocatable :: generic_data
    type(frame) :: worker_frame
    logical :: found_bottom
    integer(c_int) :: the_bottom


    if (this%config%force_max_dimensions) then
      the_height = this%config%max_height
      return
    end if

    found_bottom = .false.
    the_bottom = C_INT_MIN

    iterator = fhash_iter_t(this%frames)

    do while(iterator%next(generic_key, generic_data))
      select type (generic_data)
       type is (frame)
        worker_frame = generic_data
       class default
        error stop "[Texture Packer] Error: How did this type get in here?"
      end select

      if (found_bottom) then
        if (worker_frame%frame%bottom() > the_bottom) then
          found_bottom = .true.
          the_bottom = worker_frame%frame%bottom()
        end if
      else
        found_bottom = .true.
        the_bottom = worker_frame%frame%bottom()
      end if
    end do

    if (found_bottom) then
      the_height = the_bottom + 1 + this%config%border_padding
    else
      the_height = 0
    end if
  end function texture_packer_height


  function texture_packer_get(this, x, y, optional_pixel) result(status)
    use :: math_helpers, only: saturating_sub
    implicit none

    class(texture_packer), intent(in) :: this
    integer(c_int), intent(in), value :: x, y
    type(rgba8_pixel), intent(inout) :: optional_pixel
    integer(c_int) :: status, a, b
    type(frame) :: optional_frame
    class(*), allocatable :: generic_data
    type(rgba8_texture) :: worker_texture

    status = this%get_frame_at(x, y, optional_frame)

    if (status == TEXTURE_PACKER_OK) then
      if (this%config%texture_outlines .and. optional_frame%frame%is_outline(x, y)) then
        status = TEXTURE_PACKER_OK
        optional_pixel = this%config%outline
        return
      end if

      call this%textures%get_raw(key(optional_frame%key), generic_data, stat = status)

      if (status == 0) then
        select type (generic_data)
         type is (rgba8_texture)
          worker_texture = generic_data
         class default
          error stop "[Texture Packer] Error: How did this type get in here?"
        end select

        a = saturating_sub(x, optional_frame%frame%x, 0);
        b = saturating_sub(y, optional_frame%frame%y, 0);

        if (optional_frame%rotated) then
          a = min(x, worker_texture%height - 1);
          b = min(y, worker_texture%width - 1);
          ! todo: implement rotated (or not)
          print*,"fixme: get_rotated is not implemented, hooray"
          ! optional_pixel = worker_texture%get_rotated(a, b)
          status = TEXTURE_PACKER_OK
          return
        else
          a = min(x, worker_texture%width - 1);
          b = min(y, worker_texture%height - 1);
          optional_pixel = worker_texture%get_color(a, b)
          status = TEXTURE_PACKER_OK
          return
        end if
      end if
    end if
  end function texture_packer_get

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
