module vector_2i
  use,intrinsic :: iso_c_binding, only: c_int
  implicit none


  private


  public :: vec2i


  ! vec2i and vec2d are transparent containers.
  ! You can use the methods, or you can use the raw data.
  !
  !* They do not mix. Can't add vec2i to vec2d, and so forth. This will cause weird problems that I don't feel like solving.


  type :: vec2i
    integer(c_int) :: x, y = 0
  contains
    generic :: assignment(=) => assign_scalar_i32, assign_array_i32, assign_vec2i
    procedure :: assign_scalar_i32
    procedure :: assign_array_i32
    procedure :: assign_vec2i
    !* Note: Float equality is very dumb.
    generic :: operator(==) => equal_scalar_i32, equal_array_i32, equal_vec2i
    procedure :: equal_scalar_i32
    procedure :: equal_array_i32
    procedure :: equal_vec2i
    generic :: operator(+) => add_scalar_i32, add_array_i32, add_vec2i
    procedure :: add_scalar_i32
    procedure :: add_array_i32
    procedure :: add_vec2i
    generic :: operator(-) => subtract_scalar_i32, subtract_array_i32, subtract_vec2i
    procedure :: subtract_scalar_i32
    procedure :: subtract_array_i32
    procedure :: subtract_vec2i
    generic :: operator(*) => multiply_scalar_i32, multiply_array_i32, multiply_vec2i
    procedure :: multiply_scalar_i32
    procedure :: multiply_array_i32
    procedure :: multiply_vec2i
    generic :: operator(/) => divide_scalar_i32, divide_array_i32, divide_vec2i
    procedure :: divide_scalar_i32
    procedure :: divide_array_i32
    procedure :: divide_vec2i

    procedure :: as_array
  end type vec2i


  interface vec2i
    module procedure :: constructor_scalar, constructor_raw, constructor_array
  end interface


contains


  !* Constructors.


  type(vec2i) function constructor_scalar(i) result(vec2i_new)
    implicit none
    integer(c_int), intent(in), value :: i

    vec2i_new = i
  end function constructor_scalar


  type(vec2i) function constructor_raw(x1,y1) result(vec2i_new)
    implicit none

    integer(c_int), intent(in), value :: x1,y1

    vec2i_new = [x1,y1]
  end function constructor_raw


  type(vec2i) function constructor_array(xyz_array) result(vec2i_new)
    implicit none

    integer(c_int), dimension(2), intent(in) :: xyz_array

    vec2i_new = xyz_array(1:2)
  end function constructor_array


  !* Assignment.


  subroutine assign_scalar_i32(this, i)
    implicit none

    class(vec2i), intent(inout) :: this
    integer(c_int), intent(in), value :: i

    this%x = i
    this%y = i
  end subroutine assign_scalar_i32


  subroutine assign_array_i32(this, arr)
    implicit none

    class(vec2i), intent(inout) :: this
    integer(c_int), dimension(2), intent(in) :: arr

    this%x = arr(1)
    this%y = arr(2)
  end subroutine assign_array_i32


  subroutine assign_vec2i(this, other)
    implicit none

    class(vec2i), intent(inout) :: this
    type(vec2i), intent(in), value :: other

    this%x = other%x
    this%y = other%y
  end subroutine assign_vec2i


  !* Equality.


  logical function equal_scalar_i32(this, i) result(equality)
    implicit none

    class(vec2i), intent(in) :: this
    integer(c_int), intent(in), value :: i

    equality = this%x == i .and. this%y == i
  end function equal_scalar_i32


  logical function equal_array_i32(this, arr) result(equality)
    use :: float_compare
    implicit none

    class(vec2i), intent(in) :: this
    integer(c_int), dimension(2), intent(in) :: arr

    equality = this%x == arr(1) .and. this%y == arr(2)
  end function equal_array_i32


  logical function equal_vec2i(this, other) result(equality)
    use :: float_compare
    implicit none

    class(vec2i), intent(in) :: this
    type(vec2i), intent(in), value :: other

    equality = this%x == other%x .and. this%y == other%y
  end function equal_vec2i


  !* Addition.


  type(vec2i) function add_scalar_i32(this, i) result(vec2i_new)
    implicit none

    class(vec2i), intent(in) :: this
    integer(c_int), intent(in), value :: i

    vec2i_new%x = this%x + i
    vec2i_new%y = this%y + i
  end function add_scalar_i32


  type(vec2i) function add_array_i32(this, arr) result(vec2i_new)
    implicit none

    class(vec2i), intent(in) :: this
    integer(c_int), dimension(2), intent(in) :: arr

    vec2i_new%x = this%x + arr(1)
    vec2i_new%y = this%y + arr(2)
  end function add_array_i32


  type(vec2i) function add_vec2i(this, other) result(vec2i_new)
    implicit none

    class(vec2i), intent(in) :: this
    type(vec2i), intent(in), value :: other

    vec2i_new%x = this%x + other%x
    vec2i_new%y = this%y + other%y
  end function add_vec2i


  !* Subtraction.


  type(vec2i) function subtract_scalar_i32(this, i) result(vec2i_new)
    implicit none

    class(vec2i), intent(in) :: this
    integer(c_int), intent(in), value :: i

    vec2i_new%x = this%x - i
    vec2i_new%y = this%y - i
  end function subtract_scalar_i32


  type(vec2i) function subtract_array_i32(this, arr) result(vec2i_new)
    implicit none

    class(vec2i), intent(in) :: this
    integer(c_int), dimension(2), intent(in) :: arr

    vec2i_new%x = this%x - arr(1)
    vec2i_new%y = this%y - arr(2)
  end function subtract_array_i32


  type(vec2i) function subtract_vec2i(this, other) result(vec2i_new)
    implicit none

    class(vec2i), intent(in) :: this
    type(vec2i), intent(in), value :: other

    vec2i_new%x = this%x - other%x
    vec2i_new%y = this%y - other%y
  end function subtract_vec2i


  !* Multiplication.


  type(vec2i) function multiply_scalar_i32(this, i) result(vec2i_new)
    implicit none

    class(vec2i), intent(in) :: this
    integer(c_int), intent(in), value :: i

    vec2i_new%x = this%x * i
    vec2i_new%y = this%y * i
  end function multiply_scalar_i32


  type(vec2i) function multiply_array_i32(this, arr) result(vec2i_new)
    implicit none

    class(vec2i), intent(in) :: this
    integer(c_int), dimension(2), intent(in) :: arr

    vec2i_new%x = this%x * arr(1)
    vec2i_new%y = this%y * arr(2)
  end function multiply_array_i32


  type(vec2i) function multiply_vec2i(this, other) result(vec2i_new)
    implicit none

    class(vec2i), intent(in) :: this
    type(vec2i), intent(in), value :: other

    vec2i_new%x = this%x * other%x
    vec2i_new%y = this%y * other%y
  end function multiply_vec2i


  !* Division.


  type(vec2i) function divide_scalar_i32(this, i) result(vec2i_new)
    implicit none

    class(vec2i), intent(in) :: this
    integer(c_int), intent(in), value :: i

    vec2i_new%x = this%x / i
    vec2i_new%y = this%y / i
  end function divide_scalar_i32


  type(vec2i) function divide_array_i32(this, arr) result(vec2i_new)
    implicit none

    class(vec2i), intent(in) :: this
    integer(c_int), dimension(2), intent(in) :: arr

    vec2i_new%x = this%x / arr(1)
    vec2i_new%y = this%y / arr(2)
  end function divide_array_i32


  type(vec2i) function divide_vec2i(this, other) result(vec2i_new)
    implicit none

    class(vec2i), intent(in) :: this
    type(vec2i), intent(in), value :: other

    vec2i_new%x = this%x / other%x
    vec2i_new%y = this%y / other%y
  end function divide_vec2i


  function as_array(this) result(array_new)
    implicit none

    class(vec2i), intent(in) :: this
    integer(c_int), dimension(2) :: array_new

    array_new = [this%x, this%y]
  end function


end module vector_2i
