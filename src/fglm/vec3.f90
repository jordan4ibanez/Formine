module vec3
  use, intrinsic :: iso_c_binding
  implicit none

  private

  public :: vec3f

  ! Vec3f and Vec3d are transparent containers.
  ! You can use the methods, or you can use the raw data.
  !
  !* They do not mix. Can't add vec3f to vec3d, and so forth.

  type vec3f
    real(c_float), dimension(3) :: data = [0.0, 0.0, 0.0]
  contains
    generic :: assignment(=) => assign_scalar, assign_array, assign_vec3f
    procedure :: assign_scalar
    procedure :: assign_array
    procedure :: assign_vec3f
  end type vec3f


  interface vec3f
    module procedure :: constructor_raw,constructor_array
  end interface


contains


  type(vec3f) function constructor_raw(x,y,z) result(new_vec3f)
    implicit none

    real, intent(in), value :: x,y,z

    new_vec3f%data(1:3) = [x,y,z]
  end function constructor_raw


  type(vec3f) function constructor_array(xyz_array) result(new_vec3f)
    implicit none

    real, dimension(3), intent(in) :: xyz_array

    new_vec3f%data(1:3) = xyz_array
  end function constructor_array


  subroutine assign_scalar(this, i)
    implicit none

    class(vec3f), intent(inout) :: this
    real, intent(in), value :: i

    this%data(1:3) = [i, i, i]
  end subroutine assign_scalar


  subroutine assign_array(this, arr)
    implicit none

    class(vec3f), intent(inout) :: this
    real, dimension(3), intent(in) :: arr

    this%data(1:3) = arr
  end subroutine assign_array


  subroutine assign_vec3f(this, other)
    implicit none

    class(vec3f), intent(inout) :: this
    type(vec3f), intent(in), value :: other

    this%data(1:3) = other%data(1:3)
  end subroutine assign_vec3f





end module vec3
