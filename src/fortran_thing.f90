module fortran_thing
  implicit none

  private

  public :: say_hello
  public :: test_mul
  public :: test_loop

  ! "Class variable"
  real :: yep = 0.0

contains

  subroutine say_hello
    print *, "Hello, fortran_thing!"
  end subroutine say_hello

  function test_mul() result(result)
    real :: result
    if (yep == 0.0) then
      yep = 1.0
    end if
    yep = yep * 2.0
    result = yep
  end function test_mul

  subroutine test_loop
    integer :: i
    do i = 1,10
      print '(I2, F10.1)', i, test_mul()
    end do

  end subroutine test_loop

end module fortran_thing
