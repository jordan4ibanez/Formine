module vector_3d
  use :: iso_c_binding, only: c_double, c_float
  implicit none


  private


  public :: vec3d


  ! vec3f and Vec3d are transparent containers.
  ! You can use the methods, or you can use the raw data.
  !
  !* They do not mix. Can't add vec3f to vec3d, and so forth. This will cause weird problems that I don't feel like solving.
  !
  !? Implementation note: I added a whole bunch of f32 assignment and math because it's easier to write out 0.0 than it is 0.0d0.

  type vec3d
    real(c_double), dimension(3) :: data = [0.0, 0.0, 0.0]
  contains
    generic :: assignment(=) => assign_scalar_f64, assign_array_f64, assign_vec3d, assign_scalar_f32, assign_array_f32
    procedure :: assign_scalar_f64
    procedure :: assign_array_f64
    procedure :: assign_vec3d
    procedure :: assign_scalar_f32
    procedure :: assign_array_f32
    !* Note: Float equality is very dumb. There is no way to enforce f32 and f64 equality so we're not even going to try.
    generic :: operator(==) => equal_scalar_f64, equal_array_f64, equal_vec3d
    procedure :: equal_scalar_f64
    procedure :: equal_array_f64
    procedure :: equal_vec3d
    generic :: operator(+) => add_scalar_f64, add_array_f64, add_vec3d, add_scalar_f32, add_array_f32
    procedure :: add_scalar_f64
    procedure :: add_array_f64
    procedure :: add_vec3d
    procedure :: add_scalar_f32
    procedure :: add_array_f32
    generic :: operator(-) => subtract_scalar_f64, subtract_array_f64, subtract_vec3d, subtract_scalar_f32, subtract_array_f32
    procedure :: subtract_scalar_f64
    procedure :: subtract_array_f64
    procedure :: subtract_vec3d
    procedure :: subtract_scalar_f32
    procedure :: subtract_array_f32
    generic :: operator(*) => multiply_scalar_f64, multiply_array_f64, multiply_vec3d, multiply_scalar_f32, multiply_array_f32
    procedure :: multiply_scalar_f64
    procedure :: multiply_array_f64
    procedure :: multiply_vec3d
    procedure :: multiply_scalar_f32
    procedure :: multiply_array_f32
    generic :: operator(/) => divide_scalar_f64, divide_array_f64, divide_vec3d, divide_scalar_f32, divide_array_f32
    procedure :: divide_scalar_f64
    procedure :: divide_array_f64
    procedure :: divide_vec3d
    procedure :: divide_scalar_f32
    procedure :: divide_array_f32

    !* Getters
    procedure :: get_x
    procedure :: get_y
    procedure :: get_z
    !* Setters
    procedure :: set_x
    procedure :: set_y
    procedure :: set_z
  end type vec3d


  interface vec3d
    module procedure :: constructor_scalar_f64, constructor_raw_f64, constructor_array_f64, constructor_scalar_f32, constructor_raw_f32, constructor_array_f32
  end interface


contains


  !* Constructor.


  type(vec3d) function constructor_scalar_f64(i) result(new_vec3d)
    implicit none
    real(c_double), intent(in), value :: i

    new_vec3d%data(1:3) = [i,i,i]
  end function constructor_scalar_f64


  type(vec3d) function constructor_raw_f64(x,y,z) result(new_vec3d)
    implicit none

    real(c_double), intent(in), value :: x,y,z

    new_vec3d%data(1:3) = [x,y,z]
  end function constructor_raw_f64


  type(vec3d) function constructor_array_f64(xyz_array) result(new_vec3d)
    implicit none

    real(c_double), dimension(3), intent(in) :: xyz_array

    new_vec3d%data(1:3) = xyz_array(1:3)
  end function constructor_array_f64


  type(vec3d) function constructor_scalar_f32(i) result(new_vec3d)
    implicit none
    real(c_float), intent(in), value :: i

    new_vec3d%data(1:3) = [i,i,i]
  end function constructor_scalar_f32


  type(vec3d) function constructor_raw_f32(x,y,z) result(new_vec3d)
    implicit none

    real(c_float), intent(in), value :: x,y,z

    new_vec3d%data(1:3) = [x,y,z]
  end function constructor_raw_f32


  type(vec3d) function constructor_array_f32(xyz_array) result(new_vec3d)
    implicit none

    real(c_float), dimension(3), intent(in) :: xyz_array

    new_vec3d%data(1:3) = xyz_array(1:3)
  end function constructor_array_f32


  !* Assignment.


  subroutine assign_scalar_f64(this, i)
    implicit none

    class(vec3d), intent(inout) :: this
    real(c_double), intent(in), value :: i

    this%data(1:3) = [i, i, i]
  end subroutine assign_scalar_f64


  subroutine assign_array_f64(this, arr)
    implicit none

    class(vec3d), intent(inout) :: this
    real(c_double), dimension(3), intent(in) :: arr

    this%data(1:3) = arr(1:3)
  end subroutine assign_array_f64


  subroutine assign_vec3d(this, other)
    implicit none

    class(vec3d), intent(inout) :: this
    type(vec3d), intent(in), value :: other

    this%data(1:3) = other%data(1:3)
  end subroutine assign_vec3d


  subroutine assign_scalar_f32(this, i)
    implicit none

    class(vec3d), intent(inout) :: this
    real(c_float), intent(in), value :: i

    this%data(1:3) = [i, i, i]
  end subroutine assign_scalar_f32


  subroutine assign_array_f32(this, arr)
    implicit none

    class(vec3d), intent(inout) :: this
    real(c_float), dimension(3), intent(in) :: arr

    this%data(1:3) = arr(1:3)
  end subroutine assign_array_f32


  !* Equality.


  logical function equal_scalar_f64(this, i) result(equality)
    use :: float_compare
    implicit none

    class(vec3d), intent(in) :: this
    real(c_double), intent(in), value :: i

    equality = f64_is_equal(this%data(1), i) .and. f64_is_equal(this%data(2), i) .and. f64_is_equal(this%data(3), i)
  end function equal_scalar_f64


  logical function equal_array_f64(this, arr) result(equality)
    use :: float_compare
    implicit none

    class(vec3d), intent(in) :: this
    real(c_double), dimension(3), intent(in) :: arr

    equality = f64_is_equal(this%data(1), arr(1)) .and. f64_is_equal(this%data(2), arr(2)) .and. f64_is_equal(this%data(3), arr(3))
  end function equal_array_f64


  logical function equal_vec3d(this, other) result(equality)
    use :: float_compare
    implicit none

    class(vec3d), intent(in) :: this
    type(vec3d), intent(in), value :: other

    equality = f64_is_equal(this%data(1), other%data(1)) .and. f64_is_equal(this%data(2), other%data(2)) .and. f64_is_equal(this%data(3), other%data(3))
  end function equal_vec3d


  !* Addition


  type(vec3d) function add_scalar_f64(this, i) result(new_vec3d)
    implicit none

    class(vec3d), intent(in) :: this
    real(c_double), intent(in), value :: i

    new_vec3d = this%data(1:3) + i
  end function add_scalar_f64


  type(vec3d) function add_array_f64(this, arr) result(new_vec3d)
    implicit none

    class(vec3d), intent(in) :: this
    real(c_double), dimension(3), intent(in) :: arr

    new_vec3d = this%data(1:3) + arr(1:3)
  end function add_array_f64


  type(vec3d) function add_vec3d(this, other) result(new_vec3d)
    implicit none

    class(vec3d), intent(in) :: this
    type(vec3d), intent(in), value :: other

    new_vec3d = this%data(1:3) + other%data(1:3)
  end function add_vec3d


  type(vec3d) function add_scalar_f32(this, i) result(new_vec3d)
    implicit none

    class(vec3d), intent(in) :: this
    real(c_float), intent(in), value :: i

    new_vec3d = this%data(1:3) + i
  end function add_scalar_f32


  type(vec3d) function add_array_f32(this, arr) result(new_vec3d)
    implicit none

    class(vec3d), intent(in) :: this
    real(c_float), dimension(3), intent(in) :: arr

    new_vec3d = this%data(1:3) + arr(1:3)
  end function add_array_f32


  !* Subtraction.


  type(vec3d) function subtract_scalar_f64(this, i) result(new_vec3d)
    implicit none

    class(vec3d), intent(in) :: this
    real(c_double), intent(in), value :: i

    new_vec3d = this%data(1:3) - i
  end function subtract_scalar_f64


  type(vec3d) function subtract_array_f64(this, arr) result(new_vec3d)
    implicit none

    class(vec3d), intent(in) :: this
    real(c_double), dimension(3), intent(in) :: arr

    new_vec3d = this%data(1:3) - arr(1:3)
  end function subtract_array_f64


  type(vec3d) function subtract_vec3d(this, other) result(new_vec3d)
    implicit none

    class(vec3d), intent(in) :: this
    type(vec3d), intent(in), value :: other

    new_vec3d = this%data(1:3) + other%data(1:3)
  end function subtract_vec3d


  type(vec3d) function subtract_scalar_f32(this, i) result(new_vec3d)
    implicit none

    class(vec3d), intent(in) :: this
    real(c_float), intent(in), value :: i

    new_vec3d = this%data(1:3) - i
  end function subtract_scalar_f32


  type(vec3d) function subtract_array_f32(this, arr) result(new_vec3d)
    implicit none

    class(vec3d), intent(in) :: this
    real(c_float), dimension(3), intent(in) :: arr

    new_vec3d = this%data(1:3) - arr(1:3)
  end function subtract_array_f32


  !* Multiplication.


  type(vec3d) function multiply_scalar_f64(this, i) result(new_vec3d)
    implicit none

    class(vec3d), intent(in) :: this
    real(c_double), intent(in), value :: i

    new_vec3d = this%data(1:3) * i
  end function multiply_scalar_f64


  type(vec3d) function multiply_array_f64(this, arr) result(new_vec3d)
    implicit none

    class(vec3d), intent(in) :: this
    real(c_double), dimension(3), intent(in) :: arr

    new_vec3d = this%data(1:3) * arr(1:3)
  end function multiply_array_f64


  type(vec3d) function multiply_vec3d(this, other) result(new_vec3d)
    implicit none

    class(vec3d), intent(in) :: this
    type(vec3d), intent(in), value :: other

    new_vec3d = this%data(1:3) * other%data(1:3)
  end function multiply_vec3d


  type(vec3d) function multiply_scalar_f32(this, i) result(new_vec3d)
    implicit none

    class(vec3d), intent(in) :: this
    real(c_float), intent(in), value :: i

    new_vec3d = this%data(1:3) * i
  end function multiply_scalar_f32


  type(vec3d) function multiply_array_f32(this, arr) result(new_vec3d)
    implicit none

    class(vec3d), intent(in) :: this
    real(c_float), dimension(3), intent(in) :: arr

    new_vec3d = this%data(1:3) * arr(1:3)
  end function multiply_array_f32


  !* Division.


  type(vec3d) function divide_scalar_f64(this, i) result(new_vec3d)
    implicit none

    class(vec3d), intent(in) :: this
    real(c_double), intent(in), value :: i

    new_vec3d = this%data(1:3) / i
  end function divide_scalar_f64


  type(vec3d) function divide_array_f64(this, arr) result(new_vec3d)
    implicit none

    class(vec3d), intent(in) :: this
    real(c_double), dimension(3), intent(in) :: arr

    new_vec3d = this%data(1:3) / arr(1:3)
  end function divide_array_f64


  type(vec3d) function divide_vec3d(this, other) result(new_vec3d)
    implicit none

    class(vec3d), intent(in) :: this
    type(vec3d), intent(in), value :: other

    new_vec3d = this%data(1:3) / other%data(1:3)
  end function divide_vec3d


  type(vec3d) function divide_scalar_f32(this, i) result(new_vec3d)
    implicit none

    class(vec3d), intent(in) :: this
    real(c_float), intent(in), value :: i

    new_vec3d = this%data(1:3) / i
  end function divide_scalar_f32


  type(vec3d) function divide_array_f32(this, arr) result(new_vec3d)
    implicit none

    class(vec3d), intent(in) :: this
    real(c_float), dimension(3), intent(in) :: arr

    new_vec3d = this%data(1:3) / arr(1:3)
  end function divide_array_f32


  !* Getters.


  real(c_double) function get_x(this) result(val)
    implicit none

    class(vec3d), intent(in) :: this

    val = this%data(1)
  end function get_x


  real(c_double) function get_y(this) result(val)
    implicit none

    class(vec3d), intent(in) :: this

    val = this%data(2)
  end function get_y


  real(c_double) function get_z(this) result(val)
    implicit none

    class(vec3d), intent(in) :: this

    val = this%data(3)
  end function get_z


  !* Setters.


  subroutine set_x(this, val)
    implicit none

    class(vec3d), intent(inout) :: this
    real(c_double), intent(in), value :: val

    this%data(1) = val
  end subroutine set_x


  subroutine set_y(this, val)
    implicit none

    class(vec3d), intent(inout) :: this
    real(c_double), intent(in), value :: val

    this%data(2) = val
  end subroutine set_y


  subroutine set_z(this, val)
    implicit none

    class(vec3d), intent(inout) :: this
    real(c_double), intent(in), value :: val

    this%data(3) = val
  end subroutine set_z


end module vector_3d
