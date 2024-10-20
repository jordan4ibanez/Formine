module terminal
  use, intrinsic :: iso_c_binding
  implicit none

  private


  public :: print_color
  public :: color_term


  integer(c_int), parameter, public :: ERROR = 4
  integer(c_int), parameter, public :: WARNING = 3
  integer(c_int), parameter, public :: ADVISORY = 2
  integer(c_int), parameter, public :: NOTIFICATION = 1


  ! The 4 seasons, because I think that's neat.

  ! Winter red. Christmas ornament.
  character(8), parameter :: ERROR_COLOR = "255;0;16"

  ! Autumn orange. Pumpkin.
  character(10), parameter :: WARNING_COLOR = "255;117;24"

  ! Summer yellow. Sunflower.
  character(10), parameter :: ADVISORY_COLOR = "232;222;42"

  ! Spring blue. Easter egg.
  character(11), parameter :: NOTIFICATION_COLOR = "118;236;251"


contains


  !* Colorize print a string which when output to ANSI terminals will print in color.
  !*
  !* Severity levels: ERROR, WARNING, ADVISORY, NOTIFICATION
  !*
  !* If you put in anything else for severity, it will not colorize it.
  subroutine print_color(severity_level, input_string)
    character(len = *, kind = c_char), intent(in) :: input_string
    integer(c_int), intent(in), value :: severity_level

    print"(A)",color_term(severity_level, input_string)
  end subroutine print_color


  !* Colorize a string which when output to ANSI terminals will print in color.
  !*
  !* Severity levels: ERROR, WARNING, ADVISORY, NOTIFICATION
  !*
  !* If you put in anything else for severity, it will not colorize it.
  function color_term(severity_level, input_string) result(colorized_text)
    implicit none

    integer(c_int), intent(in), value :: severity_level
    character(len = *, kind = c_char), intent(in) :: input_string
    character(len = :), allocatable :: colorized_text

    ! Using 24 bit color standard.
    ! This might break on REALLY old terminals.
    ! Use semicolons only.
    ! Reference: https://en.wikipedia.org/wiki/ANSI_escape_code#24-bit
    select case(severity_level)
     case (ERROR)
      colorized_text = achar(27)//"[38;2;"//ERROR_COLOR//"m"//input_string//achar(27)//"[m"
     case (WARNING)
      colorized_text = achar(27)//"[38;2;"//WARNING_COLOR//"m"//input_string//achar(27)//"[m"
     case (ADVISORY)
      colorized_text = achar(27)//"[38;2;"//ADVISORY_COLOR//"m"//input_string//achar(27)//"[m"
     case (NOTIFICATION)
      colorized_text = achar(27)//"[38;2;"//NOTIFICATION_COLOR//"m"//input_string//achar(27)//"[m"
     case default
      !? Nothing happens if you try to mess with it.
      colorized_text = input_string
    end select
  end function color_term


end module terminal
