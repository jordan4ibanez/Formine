module terminal
  implicit none

  private

  public :: colorize_rgb

contains

  function colorize_rgb(input_string, r,g,b) result(colorized_text)
    use string
    implicit none

    character(len = *) :: input_string
    integer :: r
    integer :: g
    integer :: b
    character(len = :), allocatable :: colorized_text

    ! Using 24 bit color standard.
    ! This might break on REALLY old terminals.
    ! Use semicolons only.
    ! Reference: https://en.wikipedia.org/wiki/ANSI_escape_code#24-bit
    colorized_text = achar(27)//"[38;2;"//int_to_string(r)//";"//int_to_string(g)//";"//int_to_string(b)//"m"//input_string//achar(27)//"[m"
  end function colorize_rgb

end module terminal
