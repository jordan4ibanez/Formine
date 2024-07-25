module matrix_4f
  use, intrinsic :: iso_c_binding, only: c_float
  implicit none

  private

  type mat4f
    real(c_float), dimension(16) :: data = [ &
      1.0, 0.0, 0.0, 0.0, &
      0.0, 1.0, 0.0, 0.0, &
      0.0, 0.0, 1.0, 0.0, &
      0.0, 0.0, 0.0, 1.0 &
      ]
  contains
    ! generic :: assignment(=) => assign_scalar, assign_array, assign
  end type mat4f

contains

end module matrix_4f
