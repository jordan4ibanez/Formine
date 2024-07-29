module test_suite_vec3d
  use :: vector_3d
  use :: float_compare
  use :: testament
  implicit none

contains

  subroutine test_assign()
    implicit none

    type(vec3d) :: unit_1
    type(vec3d) :: unit_2
    type(vec3d) :: unit_3


    unit_1 = 1.000123d0


    call assert_true(f64_is_equal(unit_1%x, 1.000123d0))
    call assert_true(f64_is_equal(unit_1%y, 1.000123d0))
    call assert_true(f64_is_equal(unit_1%z, 1.000123d0))

    unit_2 = [1.00101d0, 1.23456d0, 1.0003d0]

    call assert_true(f64_is_equal(unit_2%x, 1.00101d0))
    call assert_true(f64_is_equal(unit_2%y, 1.23456d0))
    call assert_true(f64_is_equal(unit_2%z, 1.0003d0))

    unit_3 = vec3d(2.87d0, 9.0001d0, 10.0d0)

    call assert_true(f64_is_equal(unit_3%x, 2.8700000d0))
    call assert_true(f64_is_equal(unit_3%y, 9.0001d0))
    ! One step out of precision check.
    call assert_true(f64_is_equal(unit_3%z, 10.0000000000000001d0))
  end subroutine test_assign


  subroutine test_assign_from_vec3d()
    use :: vector_3d
    implicit none

    type(vec3d) :: unit_1

    unit_1 = 0.0d0

    ! This causes random imprecision past 15 digits, but we do not care.
    unit_1 = vec3d(0.5d0, 1.88d0, 5.019d0)

    call assert_true(unit_1 == vec3d(0.5d0, 1.88d0, 5.019d0))

  end subroutine


  subroutine test_equality()
    implicit none

    type(vec3d) :: unit_1
    type(vec3d) :: unit_2
    type(vec3d) :: unit_3

    unit_1 = 1.000101d0

    call assert_true(unit_1 == 1.000101d0)

    unit_2 = [7.1256d0, 6.127423d0, 9.02412d0]

    call assert_true(unit_2 == [7.1256d0, 6.127423d0, 9.02412d0])

    ! That's right, you can literally construct a variable off a constructor.
    unit_3 = vec3d(871.134d0, 7899.134d0, 87.91324d0)

    ! And you can compare it to a constructed variable which pops off the stack immediately.
    call assert_true(unit_3 == vec3d(871.134d0, 7899.13400d0, 87.913240d0))

  end subroutine test_equality


  subroutine test_add()
    implicit none

    ! We're going to mix it up in this test.
    ! Build upon the previous tests.

    type(vec3d) :: unit_1
    type(vec3d) :: unit_2
    type(vec3d) :: unit_3

    unit_1 = vec3d(0.0d0, 1.0d0, 2.0d0)

    unit_1 = unit_1 + [3.0d0, 2.0d0, 1.0d0]

    call assert_true(unit_1 == 3.0d0)

    unit_2 = 5.0d0

    unit_2 = unit_2 + vec3d(1.01234d0, 2.00070d0, 3.01000000d0)

    !* Notice the floating point errors.
    !* As long as it's doing the same thing on my machine, then we're A-OK.
    call assert_true(unit_2 == [6.01234000000000000d0, 7.0007d0, 8.0099999999999998d0])

    unit_3 = [9.0d0, 3.14d0, 5.5d0]

    unit_3 = unit_3 + 1.0d0

    !* Again, floating point smack dab in the middle.
    call assert_true(unit_3 == vec3d(10.0d0, 4.1400000000000006d0, 6.5d0))
  end subroutine test_add


  subroutine test_multiply()
    implicit none


    type(vec3d) :: unit_1
    type(vec3d) :: unit_2
    type(vec3d) :: unit_3

    unit_1 = 5.0d0

    unit_1 = unit_1 * 5.0d0

    call assert_true(unit_1 == 25.0d0)

    unit_2 = [1.0d0, 2.0d0, 3.0d0]

    unit_2 = unit_2 * [2.0d0, 4.0d0, 6.0d0]

    call assert_true(unit_2 == [2.0d0, 8.0d0, 18.0d0])

    unit_3 = vec3d(3.0d0, 3.0d0, 3.0d0)

    unit_3 = unit_3 * vec3d(2.0d0, 3.5d0, 4.0d0)

    call assert_true(unit_3 == vec3d(6.0d0, 10.5d0, 12.0d0))
  end subroutine test_multiply


  subroutine test_divide()
    implicit none

    type(vec3d) :: unit_1
    type(vec3d) :: unit_2
    type(vec3d) :: unit_3

    unit_1 = 5.0d0

    unit_1 = unit_1 / 5.0d0

    call assert_true(unit_1 == 1.0d0)

    unit_2 = [1.0d0, 2.0d0, 3.0d0]

    unit_2 = unit_2 / [2.0d0, 1.0d0, 6.0d0]

    call assert_true(unit_2 == [0.5d0, 2.0d0, 0.5d0])

    unit_3 = vec3d(3.0d0, 3.0d0, 3.0d0)

    unit_3 = unit_3 / vec3d(2.0d0, 3.5d0, 4.0d0)

    call assert_true(unit_3 == vec3d(1.50000000d0, 0.85714285714285710d0, 0.750000000d0))
  end subroutine test_divide


end module test_suite_vec3d


program test_vec3d
  use :: test_suite_vec3d
  implicit none

  call test_assign()

  call test_assign_from_vec3d()

  call test_equality()

  call test_add()

  call test_multiply()

  call test_divide()


end program test_vec3d
