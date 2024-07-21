module test_suite_vec3f
  use vec3
  use float_compare
  use testament
  implicit none

contains

  subroutine test_assign()
    implicit none

    type(vec3f) :: unit_1
    type(vec3f) :: unit_2
    type(vec3f) :: unit_3


    unit_1 = 1.000123


    call assert_true(f32_is_equal(unit_1%data(1), 1.000123))
    call assert_true(f32_is_equal(unit_1%data(2), 1.000123))
    call assert_true(f32_is_equal(unit_1%data(3), 1.000123))

    unit_2 = [1.00101, 1.23456, 1.0003]

    call assert_true(f32_is_equal(unit_2%data(1), 1.00101))
    call assert_true(f32_is_equal(unit_2%data(2), 1.23456))
    call assert_true(f32_is_equal(unit_2%data(3), 1.0003))

    unit_3 = vec3f(2.87, 9.0001, 10.0)

    call assert_true(f32_is_equal(unit_3%data(1), 2.8700000))
    call assert_true(f32_is_equal(unit_3%data(2), 9.0001))
    ! One step out of precision check.
    call assert_true(f32_is_equal(unit_3%data(3), 10.0000001))



  end subroutine test_assign

end module test_suite_vec3f


program test_vec3f
  use test_suite_vec3f
  implicit none

  call test_assign()

end program test_vec3f
