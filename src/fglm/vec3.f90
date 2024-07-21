module vec3
  use, intrinsic :: iso_c_binding
  implicit none

  private

  ! Vec3f and Vec3d are transparent containers.
  ! You can use the methods, or you can use the raw data.

  type vec3f
    real(c_float), dimension(3) :: data = [0.0, 0.0, 0.0]
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



end module vec3
