module terminal
  use, intrinsic :: iso_c_binding
  implicit none

  private

  ! public :: to_rgb_string
  ! public :: colorize_rgb
  public :: colorize_term

  public :: ERROR
  public :: WARNING


  ! The 4 seasons, because I think that's neat.

  ! Winter red. Christmas ornament.
  character(9), parameter :: ERROR = "176;27;46"

  ! Autumn orange. Pumpkin.
  character(10), parameter :: WARNING = "255;117;24"

  ! Summer yellow. Sunflower.
  character(10), parameter :: ADVISORY = "232;222;42"

  ! Spring blue. Easter egg.
  character(11), parameter :: NOTIFICATION = "118;236;251"


contains


  !* Colorize a string which when output to ANSI terminals will print in color. Yay!
  !* Utilizes an RGB string. Use to_rgb_string() for an easy way to get this.
  function colorize_term(input_string, rgb_string) result(colorized_text)
    implicit none

    character(len = *, kind = c_char), intent(in) :: input_string
    character(len = *, kind = c_char), intent(in) :: rgb_string
    character(len = :), allocatable :: colorized_text

    ! Using 24 bit color standard.
    ! This might break on REALLY old terminals.
    ! Use semicolons only.
    ! Reference: https://en.wikipedia.org/wiki/ANSI_escape_code#24-bit
    colorized_text = achar(27)//"[38;2;"//rgb_string//"m"//input_string//achar(27)//"[m"
  end function colorize_term


  ! Internal subroutine to stop me from doing something stupid.
  ! subroutine rgb_check(i)
  !   use :: string
  !   implicit none

  !   integer(c_int) :: i

  !   if (i < 0 .or. i > 255) then
  !     error stop "[Terminal] Error: RGB range check failed. Got: ["//int_to_string(i)//"] | Valid range: 0-255."
  !   end if
  ! end subroutine rgb_check


  ! Convert rgb values into an ANSI rgb string.
  ! RGB value range: 0-255
  ! function to_rgb_string(r,g,b) result(rgb_string)
  !   use :: string
  !   implicit none

  !   integer, intent(in) :: r
  !   integer, intent(in) :: g
  !   integer, intent(in) :: b
  !   character(len = :), allocatable :: rgb_string

  !   call rgb_check(r)
  !   call rgb_check(g)
  !   call rgb_check(b)

  !   ! Simply concatenate the whole thing together.
  !   rgb_string = int_to_string(r)//";"//int_to_string(g)//";"//int_to_string(b)
  ! end function to_rgb_string


  ! Colorize a string which when output to ANSI terminals will print in color. Yay!
  ! RGB value range: 0-255
  ! function colorize_rgb(input_string, r,g,b) result(colorized_text)
  !   use :: string
  !   implicit none

  !   character(len = *, kind = c_char), intent(in) :: input_string
  !   integer, intent(in) :: r
  !   integer, intent(in) :: g
  !   integer, intent(in) :: b
  !   character(len = :), allocatable :: colorized_text

  !   call rgb_check(r)
  !   call rgb_check(g)
  !   call rgb_check(b)

  !   ! Using 24 bit color standard.
  !   ! This might break on REALLY old terminals.
  !   ! Use semicolons only.
  !   ! Reference: https://en.wikipedia.org/wiki/ANSI_escape_code#24-bit
  !   colorized_text = achar(27)//"[38;2;"//int_to_string(r)//";"//int_to_string(g)//";"//int_to_string(b)//"m"//input_string//achar(27)//"[m"
  ! end function colorize_rgb


end module terminal
