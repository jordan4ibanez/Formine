module vector_3i
  use,intrinsic :: iso_c_binding, only: c_int
  implicit none


  private


  public :: vec3i


  type :: vec3i
    integer(c_int) :: x = 0.0
    integer(c_int) :: y = 0.0
    integer(c_int) :: z = 0.0
  contains
    generic :: assignment(=) => assign_scalar_i32, assign_array_i32, assign_vec3i
    procedure :: assign_scalar_i32
    procedure :: assign_array_i32
    procedure :: assign_vec3i
    generic :: operator(==) => equal_scalar_i32, equal_array_i32, equal_vec3i
    procedure :: equal_scalar_i32
    procedure :: equal_array_i32
    procedure :: equal_vec3i
    generic :: operator(+) => add_scalar_i32, add_array_i32, add_vec3i
    procedure :: add_scalar_i32
    procedure :: add_array_i32
    procedure :: add_vec3i
    generic :: operator(-) => subtract_scalar_i32, subtract_array_i32, subtract_vec3i
    procedure :: subtract_scalar_i32
    procedure :: subtract_array_i32
    procedure :: subtract_vec3i
    generic :: operator(*) => multiply_scalar_i32, multiply_array_i32, multiply_vec3i
    procedure :: multiply_scalar_i32
    procedure :: multiply_array_i32
    procedure :: multiply_vec3i
    generic :: operator(/) => divide_scalar_i32, divide_array_i32, divide_vec3i
    procedure :: divide_scalar_i32
    procedure :: divide_array_i32
    procedure :: divide_vec3i

    procedure :: as_array
  end type vec3i


  interface vec3i
    module procedure :: constructor_scalar, constructor_raw, constructor_array
  end interface vec3i


contains


  !* Constructors.


  type(vec3i) function constructor_scalar(i) result(new_vec3i)
    implicit none
    integer(c_int), intent(in), value :: i

    new_vec3i = i
  end function constructor_scalar


  type(vec3i) function constructor_raw(x1,y1,z1) result(new_vec3i)
    implicit none

    integer(c_int), intent(in), value :: x1,y1,z1

    new_vec3i = [x1,y1,z1]
  end function constructor_raw


  type(vec3i) function constructor_array(xyz_array) result(new_vec3i)
    implicit none

    integer(c_int), dimension(3), intent(in) :: xyz_array

    new_vec3i = xyz_array(1:3)
  end function constructor_array




  !* Assignment.


  subroutine assign_scalar_i32(this, i)
    implicit none

    class(vec3i), intent(inout) :: this
    integer(c_int), intent(in), value :: i

    this%x = i
    this%y = i
    this%z = i
  end subroutine assign_scalar_i32


  subroutine assign_array_i32(this, arr)
    implicit none

    class(vec3i), intent(inout) :: this
    integer(c_int), dimension(3), intent(in) :: arr

    this%x = arr(1)
    this%y = arr(2)
    this%z = arr(3)
  end subroutine assign_array_i32


  subroutine assign_vec3i(this, other)
    implicit none

    class(vec3i), intent(inout) :: this
    type(vec3i), intent(in), value :: other

    this%x = other%x
    this%y = other%y
    this%z = other%z
  end subroutine assign_vec3i


  !* Equality.


  logical function equal_scalar_i32(this, i) result(equality)
    implicit none

    class(vec3i), intent(in) :: this
    integer(c_int), intent(in), value :: i

    equality = this%x == i .and. this%y == i .and. this%z == i
  end function equal_scalar_i32


  logical function equal_array_i32(this, arr) result(equality)
    implicit none

    class(vec3i), intent(in) :: this
    integer(c_int), dimension(3), intent(in) :: arr

    equality = this%x == arr(1) .and. this%y == arr(2) .and. this%z == arr(3)
  end function equal_array_i32


  logical function equal_vec3i(this, other) result(equality)
    implicit none

    class(vec3i), intent(in) :: this
    type(vec3i), intent(in), value :: other

    equality = this%x == other%x .and. this%y == other%y .and. this%z == other%z
  end function equal_vec3i


  !* Addition.


  type(vec3i) function add_scalar_i32(this, i) result(new_vec3i)
    implicit none

    class(vec3i), intent(in) :: this
    integer(c_int), intent(in), value :: i

    new_vec3i%x = this%x + i
    new_vec3i%y = this%y + i
    new_vec3i%z = this%z + i
  end function add_scalar_i32


  type(vec3i) function add_array_i32(this, arr) result(new_vec3i)
    implicit none

    class(vec3i), intent(in) :: this
    integer(c_int), dimension(3), intent(in) :: arr

    new_vec3i%x = this%x + arr(1)
    new_vec3i%y = this%y + arr(2)
    new_vec3i%z = this%z + arr(3)
  end function add_array_i32


  type(vec3i) function add_vec3i(this, other) result(new_vec3i)
    implicit none

    class(vec3i), intent(in) :: this
    type(vec3i), intent(in), value :: other

    new_vec3i%x = this%x + other%x
    new_vec3i%y = this%y + other%y
    new_vec3i%z = this%z + other%z
  end function add_vec3i


  !* Subtraction.


  type(vec3i) function subtract_scalar_i32(this, i) result(new_vec3i)
    implicit none

    class(vec3i), intent(in) :: this
    integer(c_int), intent(in), value :: i

    new_vec3i%x = this%x - i
    new_vec3i%y = this%y - i
    new_vec3i%z = this%z - i
  end function subtract_scalar_i32


  type(vec3i) function subtract_array_i32(this, arr) result(new_vec3i)
    implicit none

    class(vec3i), intent(in) :: this
    integer(c_int), dimension(3), intent(in) :: arr

    new_vec3i%x = this%x - arr(1)
    new_vec3i%y = this%y - arr(2)
    new_vec3i%z = this%z - arr(3)
  end function subtract_array_i32


  type(vec3i) function subtract_vec3i(this, other) result(new_vec3i)
    implicit none

    class(vec3i), intent(in) :: this
    type(vec3i), intent(in), value :: other

    new_vec3i%x = this%x - other%x
    new_vec3i%y = this%y - other%y
    new_vec3i%z = this%z - other%z
  end function subtract_vec3i


  !* Multiplication.


  type(vec3i) function multiply_scalar_i32(this, i) result(new_vec3i)
    implicit none

    class(vec3i), intent(in) :: this
    integer(c_int), intent(in), value :: i

    new_vec3i%x = this%x * i
    new_vec3i%y = this%y * i
    new_vec3i%z = this%z * i
  end function multiply_scalar_i32


  type(vec3i) function multiply_array_i32(this, arr) result(new_vec3i)
    implicit none

    class(vec3i), intent(in) :: this
    integer(c_int), dimension(3), intent(in) :: arr

    new_vec3i%x = this%x * arr(1)
    new_vec3i%y = this%y * arr(2)
    new_vec3i%z = this%z * arr(3)
  end function multiply_array_i32


  type(vec3i) function multiply_vec3i(this, other) result(new_vec3i)
    implicit none

    class(vec3i), intent(in) :: this
    type(vec3i), intent(in), value :: other

    new_vec3i%x = this%x * other%x
    new_vec3i%y = this%y * other%y
    new_vec3i%z = this%z * other%z
  end function multiply_vec3i


  !* Division.


  type(vec3i) function divide_scalar_i32(this, i) result(new_vec3i)
    implicit none

    class(vec3i), intent(in) :: this
    integer(c_int), intent(in), value :: i

    new_vec3i%x = this%x / i
    new_vec3i%y = this%y / i
    new_vec3i%z = this%z / i
  end function divide_scalar_i32


  type(vec3i) function divide_array_i32(this, arr) result(new_vec3i)
    implicit none

    class(vec3i), intent(in) :: this
    integer(c_int), dimension(3), intent(in) :: arr

    new_vec3i%x = this%x / arr(1)
    new_vec3i%y = this%y / arr(2)
    new_vec3i%z = this%z / arr(3)
  end function divide_array_i32


  type(vec3i) function divide_vec3i(this, other) result(new_vec3i)
    implicit none

    class(vec3i), intent(in) :: this
    type(vec3i), intent(in), value :: other

    new_vec3i%x = this%x / other%x
    new_vec3i%y = this%y / other%y
    new_vec3i%z = this%z / other%z
  end function divide_vec3i


  function as_array(this) result(new_array)
    implicit none

    class(vec3i), intent(in) :: this
    integer(c_int), dimension(3) :: new_array

    new_array = [this%x, this%y, this%z]
  end function


end module vector_3i
