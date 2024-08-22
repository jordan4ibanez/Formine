module texture_packer_config
  use, intrinsic :: iso_c_binding
  use :: rgba8_texture_module
  implicit none


  type :: texture_packer_conf
    !*
    !* layout configuration
    !*
    !* Max width of the packed image. Default value is `1024`.
    integer(c_int) :: max_width = 1024
    !* Max height of the packed image. Default value is `1024`.
    integer(c_int) :: max_height = 1024
    !* True to allow rotation of the input images. Default value is `false`. Images rotated will be
    !* rotated 90 degrees clockwise.
    logical(c_bool) :: allow_rotation = .false.

    !* If enabled, the size of the output texture will always match [max_width] and [max_height]
    !* leaving potentially much unused space on the texture.
    logical(c_bool) :: force_max_dimensions = .false.

    !*
    !* texture configuration
    !*
    !* Size of the padding on the outer edge of the packed image in pixel. Default value is `0`.
    integer(c_int) :: border_padding = 0
    !* Size of the padding between frames in pixel. Default value is `2`
    integer(c_int) :: texture_padding = 2
    !* Size of the repeated pixels at the border of each image. Default value is `0`.
    integer(c_int) :: texture_extrusion = 0

    !* True to trim the empty pixels of the input images. Default value is `false`.
    logical(c_bool) :: trim = .false.

    !* True to draw the red line on the edge of the each frames. Useful for debugging. Default
    !* value is `false`.
    logical(c_bool) :: texture_outlines = .false.

    !* This was halfhazardly added in cause I don't feel like translating more rust. :D
    type(rgba8_pixel) :: outline
  end type texture_packer_conf

end module texture_packer_config
