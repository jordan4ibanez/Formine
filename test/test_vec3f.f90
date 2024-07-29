module test_suite_vec3f
  use :: vector_3f
  use :: float_compare
  use :: testament
  implicit none

contains

  subroutine test_assign()
    implicit none

    type(vec3f) :: unit_1
    type(vec3f) :: unit_2
    type(vec3f) :: unit_3


    unit_1 = 1.000123


    call assert_true(f32_is_equal(unit_1%x, 1.000123))
    call assert_true(f32_is_equal(unit_1%y, 1.000123))
    call assert_true(f32_is_equal(unit_1%z, 1.000123))

    unit_2 = [1.00101, 1.23456, 1.0003]

    call assert_true(f32_is_equal(unit_2%x, 1.00101))
    call assert_true(f32_is_equal(unit_2%y, 1.23456))
    call assert_true(f32_is_equal(unit_2%z, 1.0003))

    unit_3 = vec3f(2.87, 9.0001, 10.0)

    call assert_true(f32_is_equal(unit_3%x, 2.8700000))
    call assert_true(f32_is_equal(unit_3%y, 9.0001))
    ! One step out of precision check.
    call assert_true(f32_is_equal(unit_3%z, 10.0000001))
  end subroutine test_assign


  subroutine test_assign_from_vec3d()
    use :: vector_3d
    implicit none

    type(vec3f) :: unit_1

    unit_1 = 0.0

    ! This causes random imprecision past 6 digits, but we do not care.
    unit_1 = vec3d(0.5d0, 1.88d0, 5.019d0)

    call assert_true(unit_1 == vec3f(0.5, 1.88, 5.019))

  end subroutine


  subroutine test_equality()
    implicit none

    type(vec3f) :: unit_1
    type(vec3f) :: unit_2
    type(vec3f) :: unit_3

    unit_1 = 1.000101

    call assert_true(unit_1 == 1.000101)

    unit_2 = [7.1256, 6.127423, 9.02412]

    call assert_true(unit_2 == [7.1256, 6.127423, 9.02412])

    ! That's right, you can literally construct a variable off a constructor.
    unit_3 = vec3f(871.134, 7899.134, 87.91324)

    ! And you can compare it to a constructed variable which pops off the stack immediately.
    call assert_true(unit_3 == vec3f(871.134, 7899.13400, 87.913240))

  end subroutine test_equality


  subroutine test_add()
    implicit none

    ! We're going to mix it up in this test.
    ! Build upon the previous tests.

    type(vec3f) :: unit_1
    type(vec3f) :: unit_2
    type(vec3f) :: unit_3

    unit_1 = vec3f(0.0, 1.0, 2.0)

    unit_1 = unit_1 + [3.0, 2.0, 1.0]

    call assert_true(unit_1 == 3.0)

    unit_2 = 5.0

    unit_2 = unit_2 + vec3f(1.01234, 2.00070, 3.01000000)

    !* Notice the floating point errors.
    !* As long as it's doing the same thing on my machine, then we're A-OK.
    call assert_true(unit_2 == [6.01234007, 7.0007, 8.01000023])

    unit_3 = [9.0, 3.14, 5.5]

    unit_3 = unit_3 + 1.0

    !* Again, floating point smack dab in the middle.
    call assert_true(unit_3 == vec3f(10.0, 4.14000034, 6.5))
  end subroutine test_add


  subroutine test_multiply()
    implicit none


    type(vec3f) :: unit_1
    type(vec3f) :: unit_2
    type(vec3f) :: unit_3

    unit_1 = 5.0

    unit_1 = unit_1 * 5.0

    call assert_true(unit_1 == 25.0)

    unit_2 = [1.0, 2.0, 3.0]

    unit_2 = unit_2 * [2.0, 4.0, 6.0]

    call assert_true(unit_2 == [2.0, 8.0, 18.0])

    unit_3 = vec3f(3.0, 3.0, 3.0)

    unit_3 = unit_3 * vec3f(2.0, 3.5, 4.0)

    call assert_true(unit_3 == vec3f(6.0, 10.5, 12.0))
  end subroutine test_multiply


  subroutine test_divide()
    implicit none

    type(vec3f) :: unit_1
    type(vec3f) :: unit_2
    type(vec3f) :: unit_3

    unit_1 = 5.0

    unit_1 = unit_1 / 5.0

    call assert_true(unit_1 == 1.0)

    unit_2 = [1.0, 2.0, 3.0]

    unit_2 = unit_2 / [2.0, 1.0, 6.0]

    call assert_true(unit_2 == [0.5, 2.0, 0.5])

    unit_3 = vec3f(3.0, 3.0, 3.0)

    unit_3 = unit_3 / vec3f(2.0, 3.5, 4.0)

    call assert_true(unit_3 == vec3f(1.50000000, 0.857142866, 0.750000000))
  end subroutine test_divide


end module test_suite_vec3f


program test_vec3f
  use :: test_suite_vec3f
  implicit none

  call test_assign()

  call test_assign_from_vec3d()

  call test_equality()

  call test_add()

  call test_multiply()

  call test_divide()

end program test_vec3f
