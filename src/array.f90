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


  !* CONSTRUCTORS.


  !* Convert an optional variable length string into an integral representation of a boolean.
  integer function ex(input) result(i)
    implicit none

    class(*), intent(in), optional :: input

    if (present(input)) then
      i = 1
    else
      i = 0
    end if
  end function ex


  function constructor_int(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z) result(new_int_array)
    implicit none

    integer(c_int), intent(in), optional :: a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z
    type(int_array) :: new_int_array
    integer(c_int) :: allocation_size

    allocation_size = ex(a)+ex(b)+ex(c)+ex(d)+ex(e)+ex(f)+ex(g)+ex(h)+ex(i)+ex(j)+ex(k)+ex(l)+ex(m)+ex(n)+ex(o)+ex(p)+ex(q)+ex(r)+ex(s)+ex(t)+ex(u)+ex(v)+ex(w)+ex(x)+ex(y)+ex(z)

    allocate(new_int_array%data(allocation_size))

    new_int_array%data(1:allocation_size) = [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z]
  end function constructor_int


  function constructor_int64(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z) result(new_int64_array)
    implicit none

    integer(c_int64_t), intent(in), optional :: a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z
    type(int64_array) :: new_int64_array

    allocate(new_int64_array%data(ex(a)+ex(b)+ex(c)+ex(d)+ex(e)+ex(f)+ex(g)+ex(h)+ex(i)+ex(j)+ex(k)+ex(l)+ex(m)+ex(n)+ex(o)+ex(p)+ex(q)+ex(r)+ex(s)+ex(t)+ex(u)+ex(v)+ex(w)+ex(x)+ex(y)+ex(z)))

    if (present(a)) then
      new_int64_array%data(1) = a
    end if
    if (present(b)) then
      new_int64_array%data(2) = b
    end if
    if (present(c)) then
      new_int64_array%data(3) = c
    end if
    if (present(d)) then
      new_int64_array%data(4) = d
    end if
    if (present(e)) then
      new_int64_array%data(5) = e
    end if
    if (present(f)) then
      new_int64_array%data(6) = f
    end if
    if (present(g)) then
      new_int64_array%data(7) = g
    end if
    if (present(h)) then
      new_int64_array%data(8) = h
    end if
    if (present(i)) then
      new_int64_array%data(9) = i
    end if
    if (present(j)) then
      new_int64_array%data(10) = j
    end if
    if (present(k)) then
      new_int64_array%data(11) = k
    end if
    if (present(l)) then
      new_int64_array%data(12) = l
    end if
    if (present(m)) then
      new_int64_array%data(13) = m
    end if
    if (present(n)) then
      new_int64_array%data(14) = n
    end if
    if (present(o)) then
      new_int64_array%data(15) = o
    end if
    if (present(p)) then
      new_int64_array%data(16) = p
    end if
    if (present(q)) then
      new_int64_array%data(17) = q
    end if
    if (present(r)) then
      new_int64_array%data(18) = r
    end if
    if (present(s)) then
      new_int64_array%data(19) = s
    end if
    if (present(t)) then
      new_int64_array%data(20) = t
    end if
    if (present(u)) then
      new_int64_array%data(21) = u
    end if
    if (present(v)) then
      new_int64_array%data(22) = v
    end if
    if (present(w)) then
      new_int64_array%data(23) = w
    end if
    if (present(x)) then
      new_int64_array%data(24) = x
    end if
    if (present(y)) then
      new_int64_array%data(25) = y
    end if
    if (present(z)) then
      new_int64_array%data(26) = z
    end if
  end function constructor_int64


  function constructor_float(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z) result(new_float_array)
    implicit none

    real(c_float), intent(in), optional :: a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z
    type(float_array) :: new_float_array

    allocate(new_float_array%data(ex(a)+ex(b)+ex(c)+ex(d)+ex(e)+ex(f)+ex(g)+ex(h)+ex(i)+ex(j)+ex(k)+ex(l)+ex(m)+ex(n)+ex(o)+ex(p)+ex(q)+ex(r)+ex(s)+ex(t)+ex(u)+ex(v)+ex(w)+ex(x)+ex(y)+ex(z)))

  end function constructor_float


  function constructor_double(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z) result(new_double_array)
    implicit none

    real(c_double), intent(in), optional :: a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z
    type(double_array) :: new_double_array

    allocate(new_double_array%data(ex(a)+ex(b)+ex(c)+ex(d)+ex(e)+ex(f)+ex(g)+ex(h)+ex(i)+ex(j)+ex(k)+ex(l)+ex(m)+ex(n)+ex(o)+ex(p)+ex(q)+ex(r)+ex(s)+ex(t)+ex(u)+ex(v)+ex(w)+ex(x)+ex(y)+ex(z)))

  end function constructor_double


  function constructor_string(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z) result(new_string_array)
    implicit none

    character(len = *, kind = c_char), intent(in), optional :: a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z
    type(string_array) :: new_string_array

    allocate(new_string_array%data(ex(a)+ex(b)+ex(c)+ex(d)+ex(e)+ex(f)+ex(g)+ex(h)+ex(i)+ex(j)+ex(k)+ex(l)+ex(m)+ex(n)+ex(o)+ex(p)+ex(q)+ex(r)+ex(s)+ex(t)+ex(u)+ex(v)+ex(w)+ex(x)+ex(y)+ex(z)))

  end function constructor_string


end module array
