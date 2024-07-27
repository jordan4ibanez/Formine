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
    real(c_float) :: x, y, z = 0.0
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

    procedure :: as_array
  end type vec3f


  interface vec3f
    module procedure :: constructor_scalar, constructor_raw, constructor_array
  end interface


contains


  !* Constructors.


  type(vec3f) function constructor_scalar(i) result(new_vec3f)
    implicit none
    real(c_float), intent(in), value :: i

    new_vec3f = [i,i,i]
  end function constructor_scalar


  type(vec3f) function constructor_raw(x1,y1,z1) result(new_vec3f)
    implicit none

    real(c_float), intent(in), value :: x1,y1,z1

    new_vec3f = [x1,y1,z1]
  end function constructor_raw


  type(vec3f) function constructor_array(xyz_array) result(new_vec3f)
    implicit none

    real(c_float), dimension(3), intent(in) :: xyz_array

    new_vec3f = xyz_array(1:3)
  end function constructor_array


  !* Assignment.


  subroutine assign_scalar_f32(this, i)
    implicit none

    class(vec3f), intent(inout) :: this
    real(c_float), intent(in), value :: i

    this = [i, i, i]
  end subroutine assign_scalar_f32


  subroutine assign_array_f32(this, arr)
    implicit none

    class(vec3f), intent(inout) :: this
    real(c_float), dimension(3), intent(in) :: arr

    this%x = arr(1)
    this%y = arr(2)
    this%z = arr(3)
  end subroutine assign_array_f32


  subroutine assign_vec3f(this, other)
    implicit none

    class(vec3f), intent(inout) :: this
    type(vec3f), intent(in), value :: other

    this = other%as_array()
  end subroutine assign_vec3f


  subroutine assign_vec3d(this, other)
    use :: vector_3d
    implicit none

    class(vec3f), intent(inout) :: this
    type(vec3d), intent(in), value :: other

    ! Explicit cast to shut up compiler.
    ! f64 -> f32
    this = real(other%as_array(), kind=c_float)
  end subroutine assign_vec3d


  !* Equality.


  logical function equal_scalar_f32(this, i) result(equality)
    use :: float_compare
    implicit none

    class(vec3f), intent(in) :: this
    real(c_float), intent(in), value :: i

    equality = f32_is_equal(this%x, i) .and. f32_is_equal(this%y, i) .and. f32_is_equal(this%z, i)
  end function equal_scalar_f32


  logical function equal_array_f32(this, arr) result(equality)
    use :: float_compare
    implicit none

    class(vec3f), intent(in) :: this
    real(c_float), dimension(3), intent(in) :: arr

    equality = f32_is_equal(this%x, arr(1)) .and. f32_is_equal(this%y, arr(2)) .and. f32_is_equal(this%z, arr(3))
  end function equal_array_f32


  logical function equal_vec3f(this, other) result(equality)
    use :: float_compare
    implicit none

    class(vec3f), intent(in) :: this
    type(vec3f), intent(in), value :: other

    equality = f32_is_equal(this%x, other%x) .and. f32_is_equal(this%y, other%y) .and. f32_is_equal(this%z, other%z)
  end function equal_vec3f


  !* Addition.


  type(vec3f) function add_scalar_f32(this, i) result(new_vec3f)
    implicit none

    class(vec3f), intent(in) :: this
    real(c_float), intent(in), value :: i

    new_vec3f = this%as_array() + i
  end function add_scalar_f32


  type(vec3f) function add_array_f32(this, arr) result(new_vec3f)
    implicit none

    class(vec3f), intent(in) :: this
    real(c_float), dimension(3), intent(in) :: arr

    new_vec3f = this%as_array() + arr(1:3)
  end function add_array_f32


  type(vec3f) function add_vec3f(this, other) result(new_vec3f)
    implicit none

    class(vec3f), intent(in) :: this
    type(vec3f), intent(in), value :: other

    new_vec3f = this%as_array() + other%as_array()
  end function add_vec3f


  !* Subtraction.


  type(vec3f) function subtract_scalar_f32(this, i) result(new_vec3f)
    implicit none

    class(vec3f), intent(in) :: this
    real(c_float), intent(in), value :: i

    new_vec3f = this%as_array() - i
  end function subtract_scalar_f32


  type(vec3f) function subtract_array_f32(this, arr) result(new_vec3f)
    implicit none

    class(vec3f), intent(in) :: this
    real(c_float), dimension(3), intent(in) :: arr

    new_vec3f = this%as_array() - arr(1:3)
  end function subtract_array_f32


  type(vec3f) function subtract_vec3f(this, other) result(new_vec3f)
    implicit none

    class(vec3f), intent(in) :: this
    type(vec3f), intent(in), value :: other

    new_vec3f = this%as_array() - other%as_array()
  end function subtract_vec3f


  !* Multiplication.


  type(vec3f) function multiply_scalar_f32(this, i) result(new_vec3f)
    implicit none

    class(vec3f), intent(in) :: this
    real(c_float), intent(in), value :: i

    new_vec3f = this%as_array() * i
  end function multiply_scalar_f32


  type(vec3f) function multiply_array_f32(this, arr) result(new_vec3f)
    implicit none

    class(vec3f), intent(in) :: this
    real(c_float), dimension(3), intent(in) :: arr

    new_vec3f = this%as_array() * arr(1:3)
  end function multiply_array_f32


  type(vec3f) function multiply_vec3f(this, other) result(new_vec3f)
    implicit none

    class(vec3f), intent(in) :: this
    type(vec3f), intent(in), value :: other

    new_vec3f = this%as_array() * other%as_array()
  end function multiply_vec3f


  !* Division.


  type(vec3f) function divide_scalar_f32(this, i) result(new_vec3f)
    implicit none

    class(vec3f), intent(in) :: this
    real(c_float), intent(in), value :: i

    new_vec3f = this%as_array() / i
  end function divide_scalar_f32


  type(vec3f) function divide_array_f32(this, arr) result(new_vec3f)
    implicit none

    class(vec3f), intent(in) :: this
    real(c_float), dimension(3), intent(in) :: arr

    new_vec3f = this%as_array() / arr(1:3)
  end function divide_array_f32


  type(vec3f) function divide_vec3f(this, other) result(new_vec3f)
    implicit none

    class(vec3f), intent(in) :: this
    type(vec3f), intent(in), value :: other

    new_vec3f = this%as_array() / other%as_array()
  end function divide_vec3f


  function as_array(this) result(new_array)
    implicit none

    class(vec3f), intent(in) :: this
    real(c_float), dimension(3) :: new_array

    new_array = [this%x, this%y, this%z]
  end function

end module vector_3f
