module texture_packer_frame
  use :: texture_packer_rectangle
  use, intrinsic :: iso_c_binding
  implicit none

  !* Boundaries and properties of a packed texture.
  type :: frame
    !* Key used to uniquely identify this frame.
    character(len = :, kind = c_char), allocatable :: key
    !* Rectangle describing the texture coordinates and size.
    type(rect) :: frame
    !* True if the texture was rotated during packing.
    !* If it was rotated, it was rotated 90 degrees clockwise.
    logical(c_bool) :: rotated
    !* True if the texture was trimmed during packing.
    logical(c_bool) :: trimmed

    !* (x, y) is the trimmed frame position at original image
    !* (w, h) is original image size
    !*
    !*            w
    !*     +--------------+
    !*     | (x, y)       |
    !*     |  ^           |
    !*     |  |           |
    !*     |  *********   |
    !*     |  *       *   |  h
    !*     |  *       *   |
    !*     |  *********   |
    !*     |              |
    !*     +--------------+
    !* Source texture size before any trimming.
    type(rect) :: source
  end type frame
end module texture_packer_frame
