module vector_2f
  use :: iso_c_binding, only: c_float
  implicit none


  private


  public :: vec2f


  ! vec2f and vec2f are transparent containers.
  ! You can use the methods, or you can use the raw data.
  !
  !* They do not mix. Can't add vec2f to vec2f, and so forth. This will cause weird problems that I don't feel like solving.
  !
  !? Implementation note: I added a whole bunch of f32 assignment and math because it's easier to write out 0.0 than it is 0.0d0.

  type :: vec2f
    real(c_float):: x = 0.0
    real(c_float):: y = 0.0
  contains
    generic :: assignment(=) => assign_scalar_f32, assign_array_f32, assign_vec2f
    procedure :: assign_scalar_f32
    procedure :: assign_array_f32
    procedure :: assign_vec2f
    !* Note: Float equality is very dumb.
    generic :: operator(==) => equal_scalar_f32, equal_array_f32, equal_vec2f
    procedure :: equal_scalar_f32
    procedure :: equal_array_f32
    procedure :: equal_vec2f
    generic :: operator(+) => add_scalar_f32, add_array_f32, add_vec2f
    procedure :: add_scalar_f32
    procedure :: add_array_f32
    procedure :: add_vec2f
    generic :: operator(-) => subtract_scalar_f32, subtract_array_f32, subtract_vec2f
    procedure :: subtract_scalar_f32
    procedure :: subtract_array_f32
    procedure :: subtract_vec2f
    generic :: operator(*) => multiply_scalar_f32, multiply_array_f32, multiply_vec2f
    procedure :: multiply_scalar_f32
    procedure :: multiply_array_f32
    procedure :: multiply_vec2f
    generic :: operator(/) => divide_scalar_f32, divide_array_f32, divide_vec2f
    procedure :: divide_scalar_f32
    procedure :: divide_array_f32
    procedure :: divide_vec2f

    !* General methods.
    procedure :: as_array

  end type vec2f


  interface vec2f
    module procedure :: constructor_scalar_f32, constructor_raw_f32, constructor_array_f32
  end interface vec2f


contains


  !* Constructor.


  type(vec2f) function constructor_scalar_f32(i) result(new_vec2f)
    implicit none
    real(c_float), intent(in), value :: i

    new_vec2f = i
  end function constructor_scalar_f32


  type(vec2f) function constructor_raw_f32(x,y,z) result(new_vec2f)
    implicit none

    real(c_float), intent(in), value :: x,y,z

    new_vec2f = [x,y,z]
  end function constructor_raw_f32


  type(vec2f) function constructor_array_f32(xyz_array) result(new_vec2f)
    implicit none

    real(c_float), dimension(2), intent(in) :: xyz_array

    new_vec2f = xyz_array(1:2)
  end function constructor_array_f32


  !* Assignment.


  subroutine assign_scalar_f32(this, i)
    implicit none

    class(vec2f), intent(inout) :: this
    real(c_float), intent(in), value :: i

    this%x = i
    this%y = i
  end subroutine assign_scalar_f32


  subroutine assign_array_f32(this, arr)
    implicit none

    class(vec2f), intent(inout) :: this
    real(c_float), dimension(2), intent(in) :: arr

    this%x = arr(1)
    this%y = arr(2)
  end subroutine assign_array_f32


  subroutine assign_vec2f(this, other)
    implicit none

    class(vec2f), intent(inout) :: this
    type(vec2f), intent(in), value :: other

    this%x = other%x
    this%y = other%y
  end subroutine assign_vec2f


  !* Equality.


  logical function equal_scalar_f32(this, i) result(equality)
    use :: float_compare
    implicit none

    class(vec2f), intent(in) :: this
    real(c_float), intent(in), value :: i

    equality = f32_is_equal(this%x, i) .and. f32_is_equal(this%y, i)
  end function equal_scalar_f32


  logical function equal_array_f32(this, arr) result(equality)
    use :: float_compare
    implicit none

    class(vec2f), intent(in) :: this
    real(c_float), dimension(2), intent(in) :: arr

    equality = f32_is_equal(this%x, arr(1)) .and. f32_is_equal(this%y, arr(2))
  end function equal_array_f32


  logical function equal_vec2f(this, other) result(equality)
    use :: float_compare
    implicit none

    class(vec2f), intent(in) :: this
    type(vec2f), intent(in), value :: other

    equality = f32_is_equal(this%x, other%x) .and. f32_is_equal(this%y, other%y)
  end function equal_vec2f


  !* Addition


  type(vec2f) function add_scalar_f32(this, i) result(new_vec2f)
    implicit none

    class(vec2f), intent(in) :: this
    real(c_float), intent(in), value :: i

    new_vec2f%x = this%x + i
    new_vec2f%y = this%y + i
  end function add_scalar_f32


  type(vec2f) function add_array_f32(this, arr) result(new_vec2f)
    implicit none

    class(vec2f), intent(in) :: this
    real(c_float), dimension(2), intent(in) :: arr

    new_vec2f%x = this%x + arr(1)
    new_vec2f%y = this%y + arr(2)
  end function add_array_f32


  type(vec2f) function add_vec2f(this, other) result(new_vec2f)
    implicit none

    class(vec2f), intent(in) :: this
    type(vec2f), intent(in), value :: other

    new_vec2f%x = this%x + other%x
    new_vec2f%y = this%y + other%y
  end function add_vec2f


  !* Subtraction.


  type(vec2f) function subtract_scalar_f32(this, i) result(new_vec2f)
    implicit none

    class(vec2f), intent(in) :: this
    real(c_float), intent(in), value :: i

    new_vec2f%x = this%x - i
    new_vec2f%y = this%y - i
  end function subtract_scalar_f32


  type(vec2f) function subtract_array_f32(this, arr) result(new_vec2f)
    implicit none

    class(vec2f), intent(in) :: this
    real(c_float), dimension(2), intent(in) :: arr

    new_vec2f%x = this%x - arr(1)
    new_vec2f%y = this%y - arr(2)
  end function subtract_array_f32


  type(vec2f) function subtract_vec2f(this, other) result(new_vec2f)
    implicit none

    class(vec2f), intent(in) :: this
    type(vec2f), intent(in), value :: other

    new_vec2f%x = this%x - other%x
    new_vec2f%y = this%y - other%y
  end function subtract_vec2f


  !* Multiplication.


  type(vec2f) function multiply_scalar_f32(this, i) result(new_vec2f)
    implicit none

    class(vec2f), intent(in) :: this
    real(c_float), intent(in), value :: i

    new_vec2f%x = this%x * i
    new_vec2f%y = this%y * i
  end function multiply_scalar_f32


  type(vec2f) function multiply_array_f32(this, arr) result(new_vec2f)
    implicit none

    class(vec2f), intent(in) :: this
    real(c_float), dimension(2), intent(in) :: arr

    new_vec2f%x = this%x * arr(1)
    new_vec2f%y = this%y * arr(2)
  end function multiply_array_f32


  type(vec2f) function multiply_vec2f(this, other) result(new_vec2f)
    implicit none

    class(vec2f), intent(in) :: this
    type(vec2f), intent(in), value :: other

    new_vec2f%x = this%x * other%x
    new_vec2f%y = this%y * other%y
  end function multiply_vec2f


  !* Division.


  type(vec2f) function divide_scalar_f32(this, i) result(new_vec2f)
    implicit none

    class(vec2f), intent(in) :: this
    real(c_float), intent(in), value :: i

    new_vec2f%x = this%x / i
    new_vec2f%y = this%y / i
  end function divide_scalar_f32


  type(vec2f) function divide_array_f32(this, arr) result(new_vec2f)
    implicit none

    class(vec2f), intent(in) :: this
    real(c_float), dimension(2), intent(in) :: arr

    new_vec2f%x = this%x / arr(1)
    new_vec2f%y = this%y / arr(2)
  end function divide_array_f32


  type(vec2f) function divide_vec2f(this, other) result(new_vec2f)
    implicit none

    class(vec2f), intent(in) :: this
    type(vec2f), intent(in), value :: other

    new_vec2f%x = this%x / other%x
    new_vec2f%y = this%y / other%y
  end function divide_vec2f


  function as_array(this) result(new_array)
    implicit none

    class(vec2f), intent(in) :: this
    real(c_float), dimension(2) :: new_array

    new_array = [this%x, this%y]
  end function


end module vector_2f
