module test_suite_spread
  implicit none

contains

  subroutine example()
    implicit none

    integer, dimension(3) :: i
    integer, dimension(2,3) :: j
    integer, dimension(3,3) :: k

    i = spread(1, 1, 3)

    j = spread(i, 1, 2)

    k = spread(i, 1, 3)

  end subroutine example

end module test_suite_spread

program test_spread
  use :: test_suite_spread
  implicit none

  call example()
end program test_spread
