module matrix_4f
  use, intrinsic :: iso_c_binding, only: c_float
  implicit none

  private

  !
  ! As you can see, mat4f differs greatly from vec3f because it's
  ! only purpose is to do matrix math and shove that math into an
  ! OpenGL uniform.
  !
  ! You can also probably see this differs greatly from regular fortran matrix
  ! math because it's using a flat array as a backing structure.
  !

  public :: mat4f

  type mat4f
    real(c_float), dimension(16) :: data = [ &
      1.0, 0.0, 0.0, 0.0, &
      0.0, 1.0, 0.0, 0.0, &
      0.0, 0.0, 1.0, 0.0, &
      0.0, 0.0, 0.0, 1.0 &
      ]
  contains
    generic :: assignment(=) => assign_mat4f
    procedure :: assign_mat4f

    procedure :: identity
  end type mat4f

  interface mat4f
    module procedure :: constructor_scalar_f32, constructor_raw_f32, constructor_array_f32
  end interface mat4f

contains


  !* Constructor.


  type(mat4f) function constructor_scalar_f32(i) result(new_mat4f)
    implicit none
    real(c_float), intent(in), value :: i

    new_mat4f%data(1:16) = [i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i]
  end function constructor_scalar_f32


  type(mat4f) function constructor_raw_f32(x1,y1,z1,w1,x2,y2,z2,w2,x3,y3,z3,w3,x4,y4,z4,w4) result(new_mat4f)
    implicit none

    real(c_float), intent(in), value :: x1,y1,z1,w1,x2,y2,z2,w2,x3,y3,z3,w3,x4,y4,z4,w4

    new_mat4f%data(1:16) = [x1,y1,z1,w1,x2,y2,z2,w2,x3,y3,z3,w3,x4,y4,z4,w4]
  end function constructor_raw_f32


  type(mat4f) function constructor_array_f32(matrix_array) result(new_mat4f)
    implicit none

    real(c_float), dimension(16), intent(in) :: matrix_array

    new_mat4f%data(1:16) = matrix_array(1:16)
  end function constructor_array_f32


  !* Assignment.

  subroutine assign_mat4f(this, other)
    implicit none

    class(mat4f), intent(inout) :: this
    type(mat4f), intent(in), value :: other

    this%data(1:16) = other%data(1:16)
  end subroutine assign_mat4f


  !* Methods.

  subroutine identity(this)
    implicit none

    class(mat4f), intent(inout) :: this

    this%data(1:16) = [ &
      1.0, 0.0, 0.0, 0.0, &
      0.0, 1.0, 0.0, 0.0, &
      0.0, 0.0, 1.0, 0.0, &
      0.0, 0.0, 0.0, 1.0 &
      ]
  end subroutine identity

end module matrix_4f
