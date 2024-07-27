module vector_3f
  use,intrinsic :: iso_c_binding, only: c_float
  implicit none


  private


  public :: vec3f


  ! Vec3f and Vec3d are transparent containers.
  ! You can use the methods, or you can use the raw data.
  !
  !* They do not mix. Can't add vec3f to vec3d, and so forth. This will cause weird problems that I don't feel like solving.

  
  type vec3f
    real(c_float), dimension(3) :: data = [0.0, 0.0, 0.0]
  contains
    generic :: assignment(=) => assign_scalar_f32, assign_array_f32, assign_vec3f, assign_vec3d
    procedure :: assign_scalar_f32
    procedure :: assign_array_f32
    procedure :: assign_vec3f
    procedure :: assign_vec3d
    !* Note: Float equality is very dumb.
    generic :: operator(==) => equal_scalar_f32, equal_array_f32, equal_vec3f
    procedure :: equal_scalar_f32
    procedure :: equal_array_f32
    procedure :: equal_vec3f
    generic :: operator(+) => add_scalar_f32, add_array_f32, add_vec3f
    procedure :: add_scalar_f32
    procedure :: add_array_f32
    procedure :: add_vec3f
    generic :: operator(-) => subtract_scalar_f32, subtract_array_f32, subtract_vec3f
    procedure :: subtract_scalar_f32
    procedure :: subtract_array_f32
    procedure :: subtract_vec3f
    generic :: operator(*) => multiply_scalar_f32, multiply_array_f32, multiply_vec3f
    procedure :: multiply_scalar_f32
    procedure :: multiply_array_f32
    procedure :: multiply_vec3f
    generic :: operator(/) => divide_scalar_f32, divide_array_f32, divide_vec3f
    procedure :: divide_scalar_f32
    procedure :: divide_array_f32
    procedure :: divide_vec3f

    !* Getters.
    procedure :: get_x
    procedure :: get_y
    procedure :: get_z
    !* Setters.
    procedure :: set_x
    procedure :: set_y
    procedure :: set_z
    procedure :: set
    !* Raw access.
    procedure :: x
    procedure :: y
    procedure :: z
  end type vec3f


  interface vec3f
    module procedure :: constructor_scalar, constructor_raw, constructor_array
  end interface


contains


  !* Constructors.


  type(vec3f) function constructor_scalar(i) result(new_vec3f)
    implicit none
    real(c_float), intent(in), value :: i

    new_vec3f%data(1:3) = [i,i,i]
  end function constructor_scalar


  type(vec3f) function constructor_raw(x1,y1,z1) result(new_vec3f)
    implicit none

    real(c_float), intent(in), value :: x1,y1,z1

    new_vec3f%data(1:3) = [x1,y1,z1]
  end function constructor_raw


  type(vec3f) function constructor_array(xyz_array) result(new_vec3f)
    implicit none

    real(c_float), dimension(3), intent(in) :: xyz_array

    new_vec3f%data(1:3) = xyz_array(1:3)
  end function constructor_array


  !* Assignment.


  subroutine assign_scalar_f32(this, i)
    implicit none

    class(vec3f), intent(inout) :: this
    real(c_float), intent(in), value :: i

    this%data(1:3) = [i, i, i]
  end subroutine assign_scalar_f32


  subroutine assign_array_f32(this, arr)
    implicit none

    class(vec3f), intent(inout) :: this
    real(c_float), dimension(3), intent(in) :: arr

    this%data(1:3) = arr(1:3)
  end subroutine assign_array_f32


  subroutine assign_vec3f(this, other)
    implicit none

    class(vec3f), intent(inout) :: this
    type(vec3f), intent(in), value :: other

    this%data(1:3) = other%data(1:3)
  end subroutine assign_vec3f


  subroutine assign_vec3d(this, other)
    use :: vector_3d
    implicit none

    class(vec3f), intent(inout) :: this
    type(vec3d), intent(in), value :: other

    ! Explicit cast to shut up compiler.
    ! f64 -> f32
    this%data(1:3) = real(other%data(1:3), kind=c_float)
  end subroutine assign_vec3d


  !* Equality.


  logical function equal_scalar_f32(this, i) result(equality)
    use :: float_compare
    implicit none

    class(vec3f), intent(in) :: this
    real(c_float), intent(in), value :: i

    equality = f32_is_equal(this%data(1), i) .and. f32_is_equal(this%data(2), i) .and. f32_is_equal(this%data(3), i)
  end function equal_scalar_f32


  logical function equal_array_f32(this, arr) result(equality)
    use :: float_compare
    implicit none

    class(vec3f), intent(in) :: this
    real(c_float), dimension(3), intent(in) :: arr

    equality = f32_is_equal(this%data(1), arr(1)) .and. f32_is_equal(this%data(2), arr(2)) .and. f32_is_equal(this%data(3), arr(3))
  end function equal_array_f32


  logical function equal_vec3f(this, other) result(equality)
    use :: float_compare
    implicit none

    class(vec3f), intent(in) :: this
    type(vec3f), intent(in), value :: other

    equality = f32_is_equal(this%data(1), other%data(1)) .and. f32_is_equal(this%data(2), other%data(2)) .and. f32_is_equal(this%data(3), other%data(3))
  end function equal_vec3f


  !* Addition.


  type(vec3f) function add_scalar_f32(this, i) result(new_vec3f)
    implicit none

    class(vec3f), intent(in) :: this
    real(c_float), intent(in), value :: i

    new_vec3f = this%data(1:3) + i
  end function add_scalar_f32


  type(vec3f) function add_array_f32(this, arr) result(new_vec3f)
    implicit none

    class(vec3f), intent(in) :: this
    real(c_float), dimension(3), intent(in) :: arr

    new_vec3f = this%data(1:3) + arr(1:3)
  end function add_array_f32


  type(vec3f) function add_vec3f(this, other) result(new_vec3f)
    implicit none

    class(vec3f), intent(in) :: this
    type(vec3f), intent(in), value :: other

    new_vec3f = this%data(1:3) + other%data(1:3)
  end function add_vec3f


  !* Subtraction.


  type(vec3f) function subtract_scalar_f32(this, i) result(new_vec3f)
    implicit none

    class(vec3f), intent(in) :: this
    real(c_float), intent(in), value :: i

    new_vec3f = this%data(1:3) - i
  end function subtract_scalar_f32


  type(vec3f) function subtract_array_f32(this, arr) result(new_vec3f)
    implicit none

    class(vec3f), intent(in) :: this
    real(c_float), dimension(3), intent(in) :: arr

    new_vec3f = this%data(1:3) - arr(1:3)
  end function subtract_array_f32


  type(vec3f) function subtract_vec3f(this, other) result(new_vec3f)
    implicit none

    class(vec3f), intent(in) :: this
    type(vec3f), intent(in), value :: other

    new_vec3f = this%data(1:3) + other%data(1:3)
  end function subtract_vec3f


  !* Multiplication.


  type(vec3f) function multiply_scalar_f32(this, i) result(new_vec3f)
    implicit none

    class(vec3f), intent(in) :: this
    real(c_float), intent(in), value :: i

    new_vec3f = this%data(1:3) * i
  end function multiply_scalar_f32


  type(vec3f) function multiply_array_f32(this, arr) result(new_vec3f)
    implicit none

    class(vec3f), intent(in) :: this
    real(c_float), dimension(3), intent(in) :: arr

    new_vec3f = this%data(1:3) * arr(1:3)
  end function multiply_array_f32


  type(vec3f) function multiply_vec3f(this, other) result(new_vec3f)
    implicit none

    class(vec3f), intent(in) :: this
    type(vec3f), intent(in), value :: other

    new_vec3f = this%data(1:3) * other%data(1:3)
  end function multiply_vec3f


  !* Division.


  type(vec3f) function divide_scalar_f32(this, i) result(new_vec3f)
    implicit none

    class(vec3f), intent(in) :: this
    real(c_float), intent(in), value :: i

    new_vec3f = this%data(1:3) / i
  end function divide_scalar_f32


  type(vec3f) function divide_array_f32(this, arr) result(new_vec3f)
    implicit none

    class(vec3f), intent(in) :: this
    real(c_float), dimension(3), intent(in) :: arr

    new_vec3f = this%data(1:3) / arr(1:3)
  end function divide_array_f32


  type(vec3f) function divide_vec3f(this, other) result(new_vec3f)
    implicit none

    class(vec3f), intent(in) :: this
    type(vec3f), intent(in), value :: other

    new_vec3f = this%data(1:3) / other%data(1:3)
  end function divide_vec3f


  !* Getters.


  real(c_float) function get_x(this) result(val)
    implicit none

    class(vec3f), intent(in) :: this

    val = this%data(1)
  end function get_x


  real(c_float) function get_y(this) result(val)
    implicit none

    class(vec3f), intent(in) :: this

    val = this%data(2)
  end function get_y


  real(c_float) function get_z(this) result(val)
    implicit none

    class(vec3f), intent(in) :: this

    val = this%data(3)
  end function get_z


  !* Setters.


  subroutine set_x(this, val)
    implicit none

    class(vec3f), intent(inout) :: this
    real(c_float), intent(in), value :: val

    this%data(1) = val
  end subroutine set_x


  subroutine set_y(this, val)
    implicit none

    class(vec3f), intent(inout) :: this
    real(c_float), intent(in), value :: val

    this%data(2) = val
  end subroutine set_y


  subroutine set_z(this, val)
    implicit none

    class(vec3f), intent(inout) :: this
    real(c_float), intent(in), value :: val

    this%data(3) = val
  end subroutine set_z


  subroutine set(this, x1, y1, z1)
    implicit none

    class(vec3f), intent(inout) :: this
    real(c_float), intent(in), value :: x1, y1, z1

    this%data(1:3) = [x1, y1, z1]
  end subroutine set


  !* Raw access.


  function x(this) result(x_pointer)
    implicit none

    class(vec3f), intent(in), target :: this
    real(c_float), pointer :: x_pointer

    x_pointer => this%data(1)
  end function x


  function y(this) result(y_pointer)
    implicit none

    class(vec3f), intent(in), target :: this
    real(c_float), pointer :: y_pointer

    y_pointer => this%data(2)
  end function y


  function z(this) result(z_pointer)
    implicit none

    class(vec3f), intent(in), target :: this
    real(c_float), pointer :: z_pointer

    z_pointer => this%data(3)
  end function z


end module vector_3f
