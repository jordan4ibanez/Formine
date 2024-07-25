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

  interface mat4f
    module procedure :: constructor_scalar, constructor_raw, constructor_array
  end interface mat4f

contains


  type(mat4f) function constructor_scalar(i) result(new_mat4f)
    implicit none
    real(c_float), intent(in), value :: i

    new_mat4f%data(1:16) = [i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i]
  end function constructor_scalar

  
  type(mat4f) function constructor_raw(x1,y1,z1,w1,x2,y2,z2,w2,x3,y3,z3,w3,x4,y4,z4,w4) result(new_mat4f)
    implicit none

    real(c_float), intent(in), value :: x1,y1,z1,w1,x2,y2,z2,w2,x3,y3,z3,w3,x4,y4,z4,w4

    new_mat4f%data(1:16) = [x1,y1,z1,w1,x2,y2,z2,w2,x3,y3,z3,w3,x4,y4,z4,w4]
  end function constructor_raw


  type(mat4f) function constructor_array(matrix_array) result(new_mat4f)
    implicit none

    real(c_float), dimension(16), intent(in) :: matrix_array

    new_mat4f%data(1:16) = matrix_array(1:16)
  end function constructor_array

end module matrix_4f
