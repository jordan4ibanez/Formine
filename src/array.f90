module array
  use :: string
  use, intrinsic :: iso_c_binding
  implicit none


  private


  !* This is just a bunch of different array types.
  !* They can be used as scalars for tricky situations.


  public :: int_array
  public :: int64_array
  public :: float_array
  public :: double_array
  public :: string_array


  type :: int_array
    integer(c_int), dimension(:), allocatable :: data
  contains
    generic :: assignment(=) => assign_int
    procedure :: assign_int
  end type int_array


  interface int_array
    module procedure :: constructor_int
  end interface int_array


  type :: int64_array
    integer(c_int64_t), dimension(:), allocatable :: data
  contains
    generic :: assignment(=) => assign_int64
    procedure :: assign_int64
  end type int64_array


  interface int64_array
    module procedure :: constructor_int64
  end interface int64_array


  type :: float_array
    real(c_float), dimension(:), allocatable :: data
  contains
    generic :: assignment(=) => assign_float
    procedure :: assign_float
  end type float_array


  interface float_array
    module procedure :: constructor_float
  end interface float_array


  type :: double_array
    real(c_double), dimension(:), allocatable :: data
  contains
    generic :: assignment(=) => assign_double
    procedure :: assign_double
  end type double_array


  interface double_array
    module procedure :: constructor_double
  end interface double_array


  type :: string_array
    type(heap_string), dimension(:), allocatable :: data
  contains
    generic :: assignment(=) => assign_string
    procedure :: assign_string
  end type string_array


  interface string_array
    module procedure :: constructor_string
  end interface string_array


contains


  subroutine assign_int(this, new_data)
    implicit none

    class(int_array), intent(inout) :: this
    integer(c_int), dimension(:), intent(in) :: new_data

    this%data = new_data
  end subroutine assign_int


  subroutine assign_int64(this, new_data)
    implicit none

    class(int64_array), intent(inout) :: this
    integer(c_int64_t), dimension(:), intent(in) :: new_data

    this%data = new_data
  end subroutine assign_int64


  subroutine assign_float(this, new_data)
    implicit none

    class(float_array), intent(inout) :: this
    real(c_float), dimension(:), intent(in) :: new_data

    this%data = new_data
  end subroutine assign_float


  subroutine assign_double(this, new_data)
    implicit none

    class(double_array), intent(inout) :: this
    real(c_double), dimension(:), intent(in) :: new_data

    this%data = new_data
  end subroutine assign_double


  subroutine assign_string(this, new_data)
    implicit none

    class(string_array), intent(inout) :: this
    type(heap_string), dimension(:), intent(in) :: new_data

    this%data = new_data
  end subroutine assign_string


end module array
