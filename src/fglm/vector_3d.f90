module vector_3d
  use :: iso_c_binding, only: c_double, c_float
  implicit none


  private


  public :: vec3d


  ! vec3f and vec3d are transparent containers.
  ! You can use the methods, or you can use the raw data.
  !
  !* They do not mix. Can't add vec3f to vec3d, and so forth. This will cause weird problems that I don't feel like solving.
  !
  !? Implementation note: I added a whole bunch of f32 assignment and math because it's easier to write out 0.0 than it is 0.0d0.

  type :: vec3d
    real(c_double):: x = 0.0d0
    real(c_double):: y = 0.0d0
    real(c_double):: z = 0.0d0
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

    !* General methods.
    procedure :: as_array

    !* Precision choppers.
    procedure :: x_f32
    procedure :: y_f32
    procedure :: z_f32
  end type vec3d


  interface vec3d
    module procedure :: constructor_scalar_f64, constructor_raw_f64, constructor_array_f64, constructor_scalar_f32, constructor_raw_f32, constructor_array_f32
  end interface vec3d


contains


  !* Constructor.


  type(vec3d) function constructor_scalar_f64(i) result(new_vec3d)
    implicit none
    real(c_double), intent(in), value :: i

    new_vec3d = i
  end function constructor_scalar_f64


  type(vec3d) function constructor_raw_f64(x,y,z) result(new_vec3d)
    implicit none

    real(c_double), intent(in), value :: x,y,z

    new_vec3d = [x,y,z]
  end function constructor_raw_f64


  type(vec3d) function constructor_array_f64(xyz_array) result(new_vec3d)
    implicit none

    real(c_double), dimension(3), intent(in) :: xyz_array

    new_vec3d = xyz_array(1:3)
  end function constructor_array_f64


  type(vec3d) function constructor_scalar_f32(i) result(new_vec3d)
    implicit none
    real(c_float), intent(in), value :: i

    new_vec3d = i
  end function constructor_scalar_f32


  type(vec3d) function constructor_raw_f32(x,y,z) result(new_vec3d)
    implicit none

    real(c_float), intent(in), value :: x,y,z

    new_vec3d = [x,y,z]
  end function constructor_raw_f32


  type(vec3d) function constructor_array_f32(xyz_array) result(new_vec3d)
    implicit none

    real(c_float), dimension(3), intent(in) :: xyz_array

    new_vec3d = xyz_array(1:3)
  end function constructor_array_f32


  !* Assignment.


  subroutine assign_scalar_f64(this, i)
    implicit none

    class(vec3d), intent(inout) :: this
    real(c_double), intent(in), value :: i

    this%x = i
    this%y = i
    this%z = i
  end subroutine assign_scalar_f64


  subroutine assign_array_f64(this, arr)
    implicit none

    class(vec3d), intent(inout) :: this
    real(c_double), dimension(3), intent(in) :: arr

    this%x = arr(1)
    this%y = arr(2)
    this%z = arr(3)
  end subroutine assign_array_f64


  subroutine assign_vec3d(this, other)
    implicit none

    class(vec3d), intent(inout) :: this
    type(vec3d), intent(in), value :: other

    this%x = other%x
    this%y = other%y
    this%z = other%z
  end subroutine assign_vec3d


  subroutine assign_scalar_f32(this, i)
    implicit none

    class(vec3d), intent(inout) :: this
    real(c_float), intent(in), value :: i

    this%x = i
    this%y = i
    this%z = i
  end subroutine assign_scalar_f32


  subroutine assign_array_f32(this, arr)
    implicit none

    class(vec3d), intent(inout) :: this
    real(c_float), dimension(3), intent(in) :: arr

    this%x = arr(1)
    this%y = arr(2)
    this%z = arr(3)
  end subroutine assign_array_f32


  !* Equality.


  logical function equal_scalar_f64(this, i) result(equality)
    use :: float_compare
    implicit none

    class(vec3d), intent(in) :: this
    real(c_double), intent(in), value :: i

    equality = f64_is_equal(this%x, i) .and. f64_is_equal(this%y, i) .and. f64_is_equal(this%z, i)
  end function equal_scalar_f64


  logical function equal_array_f64(this, arr) result(equality)
    use :: float_compare
    implicit none

    class(vec3d), intent(in) :: this
    real(c_double), dimension(3), intent(in) :: arr

    equality = f64_is_equal(this%x, arr(1)) .and. f64_is_equal(this%y, arr(2)) .and. f64_is_equal(this%z, arr(3))
  end function equal_array_f64


  logical function equal_vec3d(this, other) result(equality)
    use :: float_compare
    implicit none

    class(vec3d), intent(in) :: this
    type(vec3d), intent(in), value :: other

    equality = f64_is_equal(this%x, other%x) .and. f64_is_equal(this%y, other%y) .and. f64_is_equal(this%z, other%z)
  end function equal_vec3d


  !* Addition


  type(vec3d) function add_scalar_f64(this, i) result(new_vec3d)
    implicit none

    class(vec3d), intent(in) :: this
    real(c_double), intent(in), value :: i

    new_vec3d%x = this%x + i
    new_vec3d%y = this%y + i
    new_vec3d%z = this%z + i
  end function add_scalar_f64


  type(vec3d) function add_array_f64(this, arr) result(new_vec3d)
    implicit none

    class(vec3d), intent(in) :: this
    real(c_double), dimension(3), intent(in) :: arr

    new_vec3d%x = this%x + arr(1)
    new_vec3d%y = this%y + arr(2)
    new_vec3d%z = this%z + arr(3)
  end function add_array_f64


  type(vec3d) function add_vec3d(this, other) result(new_vec3d)
    implicit none

    class(vec3d), intent(in) :: this
    type(vec3d), intent(in), value :: other

    new_vec3d%x = this%x + other%x
    new_vec3d%y = this%y + other%y
    new_vec3d%z = this%z + other%z
  end function add_vec3d


  type(vec3d) function add_scalar_f32(this, i) result(new_vec3d)
    implicit none

    class(vec3d), intent(in) :: this
    real(c_float), intent(in), value :: i

    new_vec3d%x = this%x + i
    new_vec3d%y = this%y + i
    new_vec3d%z = this%z + i
  end function add_scalar_f32


  type(vec3d) function add_array_f32(this, arr) result(new_vec3d)
    implicit none

    class(vec3d), intent(in) :: this
    real(c_float), dimension(3), intent(in) :: arr

    new_vec3d%x = this%x + arr(1)
    new_vec3d%y = this%y + arr(2)
    new_vec3d%z = this%z + arr(3)
  end function add_array_f32


  !* Subtraction.


  type(vec3d) function subtract_scalar_f64(this, i) result(new_vec3d)
    implicit none

    class(vec3d), intent(in) :: this
    real(c_double), intent(in), value :: i

    new_vec3d%x = this%x - i
    new_vec3d%y = this%y - i
    new_vec3d%z = this%z - i
  end function subtract_scalar_f64


  type(vec3d) function subtract_array_f64(this, arr) result(new_vec3d)
    implicit none

    class(vec3d), intent(in) :: this
    real(c_double), dimension(3), intent(in) :: arr

    new_vec3d%x = this%x - arr(1)
    new_vec3d%y = this%y - arr(2)
    new_vec3d%z = this%z - arr(3)
  end function subtract_array_f64


  type(vec3d) function subtract_vec3d(this, other) result(new_vec3d)
    implicit none

    class(vec3d), intent(in) :: this
    type(vec3d), intent(in), value :: other

    new_vec3d%x = this%x - other%x
    new_vec3d%y = this%y - other%y
    new_vec3d%z = this%z - other%z
  end function subtract_vec3d


  type(vec3d) function subtract_scalar_f32(this, i) result(new_vec3d)
    implicit none

    class(vec3d), intent(in) :: this
    real(c_float), intent(in), value :: i

    new_vec3d%x = this%x - i
    new_vec3d%y = this%y - i
    new_vec3d%z = this%z - i
  end function subtract_scalar_f32


  type(vec3d) function subtract_array_f32(this, arr) result(new_vec3d)
    implicit none

    class(vec3d), intent(in) :: this
    real(c_float), dimension(3), intent(in) :: arr

    new_vec3d%x = this%x - arr(1)
    new_vec3d%y = this%y - arr(2)
    new_vec3d%z = this%z - arr(3)
  end function subtract_array_f32


  !* Multiplication.


  type(vec3d) function multiply_scalar_f64(this, i) result(new_vec3d)
    implicit none

    class(vec3d), intent(in) :: this
    real(c_double), intent(in), value :: i

    new_vec3d%x = this%x * i
    new_vec3d%y = this%y * i
    new_vec3d%z = this%z * i
  end function multiply_scalar_f64


  type(vec3d) function multiply_array_f64(this, arr) result(new_vec3d)
    implicit none

    class(vec3d), intent(in) :: this
    real(c_double), dimension(3), intent(in) :: arr

    new_vec3d%x = this%x * arr(1)
    new_vec3d%y = this%y * arr(2)
    new_vec3d%z = this%z * arr(3)
  end function multiply_array_f64


  type(vec3d) function multiply_vec3d(this, other) result(new_vec3d)
    implicit none

    class(vec3d), intent(in) :: this
    type(vec3d), intent(in), value :: other

    new_vec3d%x = this%x * other%x
    new_vec3d%y = this%y * other%y
    new_vec3d%z = this%z * other%z
  end function multiply_vec3d


  type(vec3d) function multiply_scalar_f32(this, i) result(new_vec3d)
    implicit none

    class(vec3d), intent(in) :: this
    real(c_float), intent(in), value :: i

    new_vec3d%x = this%x * i
    new_vec3d%y = this%y * i
    new_vec3d%z = this%z * i
  end function multiply_scalar_f32


  type(vec3d) function multiply_array_f32(this, arr) result(new_vec3d)
    implicit none

    class(vec3d), intent(in) :: this
    real(c_float), dimension(3), intent(in) :: arr

    new_vec3d%x = this%x * arr(1)
    new_vec3d%y = this%y * arr(2)
    new_vec3d%z = this%z * arr(3)
  end function multiply_array_f32


  !* Division.


  type(vec3d) function divide_scalar_f64(this, i) result(new_vec3d)
    implicit none

    class(vec3d), intent(in) :: this
    real(c_double), intent(in), value :: i

    new_vec3d%x = this%x / i
    new_vec3d%y = this%y / i
    new_vec3d%z = this%z / i
  end function divide_scalar_f64


  type(vec3d) function divide_array_f64(this, arr) result(new_vec3d)
    implicit none

    class(vec3d), intent(in) :: this
    real(c_double), dimension(3), intent(in) :: arr

    new_vec3d%x = this%x / arr(1)
    new_vec3d%y = this%y / arr(2)
    new_vec3d%z = this%z / arr(3)
  end function divide_array_f64


  type(vec3d) function divide_vec3d(this, other) result(new_vec3d)
    implicit none

    class(vec3d), intent(in) :: this
    type(vec3d), intent(in), value :: other

    new_vec3d%x = this%x / other%x
    new_vec3d%y = this%y / other%y
    new_vec3d%z = this%z / other%z
  end function divide_vec3d


  type(vec3d) function divide_scalar_f32(this, i) result(new_vec3d)
    implicit none

    class(vec3d), intent(in) :: this
    real(c_float), intent(in), value :: i

    new_vec3d%x = this%x / i
    new_vec3d%y = this%y / i
    new_vec3d%z = this%z / i
  end function divide_scalar_f32


  type(vec3d) function divide_array_f32(this, arr) result(new_vec3d)
    implicit none

    class(vec3d), intent(in) :: this
    real(c_float), dimension(3), intent(in) :: arr

    new_vec3d%x = this%x / arr(1)
    new_vec3d%y = this%y / arr(2)
    new_vec3d%z = this%z / arr(3)
  end function divide_array_f32


  function as_array(this) result(new_array)
    implicit none

    class(vec3d), intent(in) :: this
    real(c_double), dimension(3) :: new_array

    new_array = [this%x, this%y, this%z]
  end function


  !* Precision choppers.


  real(c_float) function x_f32(this) result(result_x_f32)
    use :: math_helpers, only: into_f32
    class(vec3d), intent(in) :: this

    result_x_f32 = into_f32(this%x)
  end function x_f32


  real(c_float) function y_f32(this) result(result_y_f32)
    use :: math_helpers, only: into_f32
    class(vec3d), intent(in) :: this

    result_y_f32 = into_f32(this%y)
  end function y_f32


  real(c_float) function z_f32(this) result(result_z_f32)
    use :: math_helpers, only: into_f32
    class(vec3d), intent(in) :: this

    result_z_f32 = into_f32(this%z)
  end function z_f32


end module vector_3d
