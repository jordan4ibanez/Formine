module vector_3f
  use, intrinsic :: iso_c_binding
  implicit none

  private

  public :: vec3f

  ! Vec3f and Vec3 are transparent containers.
  ! You can use the methods, or you can use the raw data.
  !
  !* They do not mix. Can't add vec3f to vec3, and so forth.

  type vec3f
    real(c_float), dimension(3) :: data = [0.0, 0.0, 0.0]
  contains
    generic :: assignment(=) => assign_scalar, assign_array, assign_vec3f
    procedure :: assign_scalar
    procedure :: assign_array
    procedure :: assign_vec3f
    !* Note: Float equality is very dumb.
    generic :: operator(==) => equal_scalar, equal_array, equal_vec3f
    procedure :: equal_scalar
    procedure :: equal_array
    procedure :: equal_vec3f
    generic :: operator(+) => add_scalar, add_array, add_vec3f
    procedure :: add_scalar
    procedure :: add_array
    procedure :: add_vec3f
    generic :: operator(-) => subtract_scalar, subtract_array, subtract_vec3f
    procedure :: subtract_scalar
    procedure :: subtract_array
    procedure :: subtract_vec3f
    generic :: operator(*) => multiply_scalar, multiply_array, multiply_vec3f
    procedure :: multiply_scalar
    procedure :: multiply_array
    procedure :: multiply_vec3f
  end type vec3f


  interface vec3f
    module procedure :: constructor_scalar, constructor_raw, constructor_array
  end interface


contains

  type(vec3f) function constructor_scalar(i) result(new_vec3f)
    implicit none
    real, intent(in), value :: i

    new_vec3f%data(1:3) = [i,i,i]
  end function constructor_scalar

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


  logical function equal_scalar(this, i) result(equality)
    use float_compare
    implicit none

    class(vec3f), intent(in) :: this
    real, intent(in), value :: i

    equality = f32_is_equal(this%data(1), i) .and. f32_is_equal(this%data(2), i) .and. f32_is_equal(this%data(3), i)
  end function equal_scalar


  logical function equal_array(this, arr) result(equality)
    use float_compare
    implicit none

    class(vec3f), intent(in) :: this
    real, dimension(3), intent(in) :: arr

    equality = f32_is_equal(this%data(1), arr(1)) .and. f32_is_equal(this%data(2), arr(2)) .and. f32_is_equal(this%data(3), arr(3))
  end function equal_array


  logical function equal_vec3f(this, other) result(equality)
    use float_compare
    implicit none

    class(vec3f), intent(in) :: this
    type(vec3f), intent(in), value :: other

    equality = f32_is_equal(this%data(1), other%data(1)) .and. f32_is_equal(this%data(2), other%data(2)) .and. f32_is_equal(this%data(3), other%data(3))
  end function equal_vec3f


  type(vec3f) function add_scalar(this, i) result(new_vec3f)
    implicit none

    class(vec3f), intent(in) :: this
    real, intent(in), value :: i

    new_vec3f = this%data(1:3) + i
  end function add_scalar


  type(vec3f) function add_array(this, arr) result(new_vec3f)
    implicit none

    class(vec3f), intent(in) :: this
    real, dimension(3), intent(in) :: arr

    new_vec3f = this%data(1:3) + arr
  end function add_array


  type(vec3f) function add_vec3f(this, other) result(new_vec3f)
    implicit none

    class(vec3f), intent(in) :: this
    type(vec3f), intent(in), value :: other

    new_vec3f = this%data(1:3) + other%data(1:3)
  end function add_vec3f


  type(vec3f) function subtract_scalar(this, i) result(new_vec3f)
    implicit none

    class(vec3f), intent(in) :: this
    real, intent(in), value :: i

    new_vec3f = this%data(1:3) - i
  end function subtract_scalar


  type(vec3f) function subtract_array(this, arr) result(new_vec3f)
    implicit none

    class(vec3f), intent(in) :: this
    real, dimension(3), intent(in) :: arr

    new_vec3f = this%data(1:3) - arr
  end function subtract_array


  type(vec3f) function subtract_vec3f(this, other) result(new_vec3f)
    implicit none

    class(vec3f), intent(in) :: this
    type(vec3f), intent(in), value :: other

    new_vec3f = this%data(1:3) + other%data(1:3)
  end function subtract_vec3f


  type(vec3f) function multiply_scalar(this, i) result(new_vec3f)
    implicit none

    class(vec3f), intent(in) :: this
    real, intent(in), value :: i

    new_vec3f = this%data(1:3) * i
  end function multiply_scalar


  type(vec3f) function multiply_array(this, arr) result(new_vec3f)
    implicit none

    class(vec3f), intent(in) :: this
    real, dimension(3), intent(in) :: arr

    new_vec3f = this%data(1:3) * arr(1:3)
  end function multiply_array


  type(vec3f) function multiply_vec3f(this, other) result(new_vec3f)
    implicit none

    class(vec3f), intent(in) :: this
    type(vec3f), intent(in), value :: other

    new_vec3f = this%data(1:3) * other%data(1:3)
  end function multiply_vec3f

end module vector_3f
