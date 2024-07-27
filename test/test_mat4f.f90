module test_suite_mat4f
  use :: matrix_4f
  implicit none

contains

  subroutine basic_test

    type(mat4f) :: unit_1

    unit_1 = mat4f()

    ! print*,unit_1

  end subroutine basic_test

end module test_suite_mat4f


program test_mat4f
  use :: test_suite_mat4f
  implicit none

  call basic_test()

end program test_mat4f
