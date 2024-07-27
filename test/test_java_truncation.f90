module test_suite_trancation
  use testament
  implicit none

  !? I'm literally using a unit test as a testing environment instead of a unit test here.
  !? This is to mirror how java behaves with casting.

contains


  subroutine mirror_java_tests()
    use, intrinsic :: iso_c_binding, only: c_float
    implicit none

    real(c_float) :: x, y

    ! I'm just going to reuse these variables because this is a testing environment.

    x = 0.9
    y = int(x)

    call assert_true(y == 0)

    x = 0.2
    y = int(x)

    call assert_true(y == 0)

    x = 105.5
    y = int(x)

    call assert_true(y == 105)


    x = -105.5
    y = int(x)

    call assert_true(y == -105)

    x = -3.5
    y = int(x)

    call assert_true(y == -3)


    x = -0.5
    y = int(x)

    call assert_true(y == 0)
  end subroutine mirror_java_tests

end module test_suite_trancation

program test_trancation
  use :: test_suite_trancation
  implicit none

  call mirror_java_tests()


end program test_trancation
