module test_suite_trancation
  implicit none

  !? I'm literally using a unit test as a testing environment instead of a unit test here.
  !? This is to mirror how java behaves with casting.

contains


  subroutine mirror_java_tests()
    use, intrinsic :: iso_c_binding, only: c_float
    implicit none

    real(c_float) :: x, y

    x = 0.9
    y = anint(x)

    print*,y


  end subroutine mirror_java_tests

end module test_suite_trancation

program test_trancation
  use :: test_suite_trancation
  implicit none

  call mirror_java_tests()


end program test_trancation
