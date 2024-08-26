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
  public :: array_i32_small_to_large_unique
  public :: array_i32_remove
  public :: array_i32_insert
  public :: array_string_insert
  public :: array_memory_texture_insert


  type :: int_array
    integer(c_int), dimension(:), allocatable :: data
  contains
    private
    generic :: assignment(=) => assign_int
    procedure :: assign_int
  end type int_array


  interface int_array
    module procedure :: constructor_int
  end interface int_array


  type :: int64_array
    integer(c_int64_t), dimension(:), allocatable :: data
  contains
    private
    generic :: assignment(=) => assign_int64
    procedure :: assign_int64
  end type int64_array


  interface int64_array
    module procedure :: constructor_int64
  end interface int64_array


  type :: float_array
    real(c_float), dimension(:), allocatable :: data
  contains
    private
    generic :: assignment(=) => assign_float
    procedure :: assign_float
  end type float_array


  interface float_array
    module procedure :: constructor_float
  end interface float_array


  type :: double_array
    real(c_double), dimension(:), allocatable :: data
  contains
    private
    generic :: assignment(=) => assign_double
    procedure :: assign_double
  end type double_array


  interface double_array
    module procedure :: constructor_double
  end interface double_array


  type :: string_array
    type(heap_string), dimension(:), allocatable :: data
  contains
    private
    generic :: assignment(=) => assign_string
    procedure :: assign_string
  end type string_array


  interface string_array
    module procedure :: constructor_string
  end interface string_array


contains


  subroutine assign_int(this, data_new)
    implicit none

    class(int_array), intent(inout) :: this
    integer(c_int), dimension(:), intent(in) :: data_new

    this%data = data_new
  end subroutine assign_int


  subroutine assign_int64(this, data_new)
    implicit none

    class(int64_array), intent(inout) :: this
    integer(c_int64_t), dimension(:), intent(in) :: data_new

    this%data = data_new
  end subroutine assign_int64


  subroutine assign_float(this, data_new)
    implicit none

    class(float_array), intent(inout) :: this
    real(c_float), dimension(:), intent(in) :: data_new

    this%data = data_new
  end subroutine assign_float


  subroutine assign_double(this, data_new)
    implicit none

    class(double_array), intent(inout) :: this
    real(c_double), dimension(:), intent(in) :: data_new

    this%data = data_new
  end subroutine assign_double


  subroutine assign_string(this, data_new)
    implicit none

    class(string_array), intent(inout) :: this
    type(heap_string), dimension(:), intent(in) :: data_new

    this%data = data_new
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


  function constructor_int(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z) result(int_array_new)
    implicit none

    integer(c_int), intent(in), optional :: a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z
    type(int_array) :: int_array_new
    integer(c_int) :: index, allocation_size

    allocation_size = ex(a)+ex(b)+ex(c)+ex(d)+ex(e)+ex(f)+ex(g)+ex(h)+ex(i)+ex(j)+ex(k)+ex(l)+ex(m)+ex(n)+ex(o)+ex(p)+ex(q)+ex(r)+ex(s)+ex(t)+ex(u)+ex(v)+ex(w)+ex(x)+ex(y)+ex(z)

    allocate(int_array_new%data(allocation_size))

    index = 1

    if (index > allocation_size) return
    if (present(a)) int_array_new%data(index) = a
    index = index + 1
    if (index > allocation_size) return
    if (present(b)) int_array_new%data(index) = b
    index = index + 1
    if (index > allocation_size) return
    if (present(c)) int_array_new%data(index) = c
    index = index + 1
    if (index > allocation_size) return
    if (present(d)) int_array_new%data(index) = d
    index = index + 1
    if (index > allocation_size) return
    if (present(e)) int_array_new%data(index) = e
    index = index + 1
    if (index > allocation_size) return
    if (present(f)) int_array_new%data(index) = f
    index = index + 1
    if (index > allocation_size) return
    if (present(g)) int_array_new%data(index) = g
    index = index + 1
    if (index > allocation_size) return
    if (present(h)) int_array_new%data(index) = h
    index = index + 1
    if (index > allocation_size) return
    if (present(i)) int_array_new%data(index) = i
    index = index + 1
    if (index > allocation_size) return
    if (present(j)) int_array_new%data(index) = j
    index = index + 1
    if (index > allocation_size) return
    if (present(k)) int_array_new%data(index) = k
    index = index + 1
    if (index > allocation_size) return
    if (present(l)) int_array_new%data(index) = l
    index = index + 1
    if (index > allocation_size) return
    if (present(m)) int_array_new%data(index) = m
    index = index + 1
    if (index > allocation_size) return
    if (present(n)) int_array_new%data(index) = n
    index = index + 1
    if (index > allocation_size) return
    if (present(o)) int_array_new%data(index) = o
    index = index + 1
    if (index > allocation_size) return
    if (present(p)) int_array_new%data(index) = p
    index = index + 1
    if (index > allocation_size) return
    if (present(q)) int_array_new%data(index) = q
    index = index + 1
    if (index > allocation_size) return
    if (present(r)) int_array_new%data(index) = r
    index = index + 1
    if (index > allocation_size) return
    if (present(s)) int_array_new%data(index) = s
    index = index + 1
    if (index > allocation_size) return
    if (present(t)) int_array_new%data(index) = t
    index = index + 1
    if (index > allocation_size) return
    if (present(u)) int_array_new%data(index) = u
    index = index + 1
    if (index > allocation_size) return
    if (present(v)) int_array_new%data(index) = v
    index = index + 1
    if (index > allocation_size) return
    if (present(w)) int_array_new%data(index) = w
    index = index + 1
    if (index > allocation_size) return
    if (present(x)) int_array_new%data(index) = x
    index = index + 1
    if (index > allocation_size) return
    if (present(y)) int_array_new%data(index) = y
    index = index + 1
    if (index > allocation_size) return
    if (present(z)) int_array_new%data(index) = z
  end function constructor_int


  function constructor_int64(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z) result(int64_array_new)
    implicit none

    integer(c_int64_t), intent(in), optional :: a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z
    type(int64_array) :: int64_array_new
    integer(c_int) :: index, allocation_size

    allocation_size = ex(a)+ex(b)+ex(c)+ex(d)+ex(e)+ex(f)+ex(g)+ex(h)+ex(i)+ex(j)+ex(k)+ex(l)+ex(m)+ex(n)+ex(o)+ex(p)+ex(q)+ex(r)+ex(s)+ex(t)+ex(u)+ex(v)+ex(w)+ex(x)+ex(y)+ex(z)

    allocate(int64_array_new%data(allocation_size))

    index = 1

    if (index > allocation_size) return
    if (present(a)) int64_array_new%data(index) = a
    index = index + 1
    if (index > allocation_size) return
    if (present(b)) int64_array_new%data(index) = b
    index = index + 1
    if (index > allocation_size) return
    if (present(c)) int64_array_new%data(index) = c
    index = index + 1
    if (index > allocation_size) return
    if (present(d)) int64_array_new%data(index) = d
    index = index + 1
    if (index > allocation_size) return
    if (present(e)) int64_array_new%data(index) = e
    index = index + 1
    if (index > allocation_size) return
    if (present(f)) int64_array_new%data(index) = f
    index = index + 1
    if (index > allocation_size) return
    if (present(g)) int64_array_new%data(index) = g
    index = index + 1
    if (index > allocation_size) return
    if (present(h)) int64_array_new%data(index) = h
    index = index + 1
    if (index > allocation_size) return
    if (present(i)) int64_array_new%data(index) = i
    index = index + 1
    if (index > allocation_size) return
    if (present(j)) int64_array_new%data(index) = j
    index = index + 1
    if (index > allocation_size) return
    if (present(k)) int64_array_new%data(index) = k
    index = index + 1
    if (index > allocation_size) return
    if (present(l)) int64_array_new%data(index) = l
    index = index + 1
    if (index > allocation_size) return
    if (present(m)) int64_array_new%data(index) = m
    index = index + 1
    if (index > allocation_size) return
    if (present(n)) int64_array_new%data(index) = n
    index = index + 1
    if (index > allocation_size) return
    if (present(o)) int64_array_new%data(index) = o
    index = index + 1
    if (index > allocation_size) return
    if (present(p)) int64_array_new%data(index) = p
    index = index + 1
    if (index > allocation_size) return
    if (present(q)) int64_array_new%data(index) = q
    index = index + 1
    if (index > allocation_size) return
    if (present(r)) int64_array_new%data(index) = r
    index = index + 1
    if (index > allocation_size) return
    if (present(s)) int64_array_new%data(index) = s
    index = index + 1
    if (index > allocation_size) return
    if (present(t)) int64_array_new%data(index) = t
    index = index + 1
    if (index > allocation_size) return
    if (present(u)) int64_array_new%data(index) = u
    index = index + 1
    if (index > allocation_size) return
    if (present(v)) int64_array_new%data(index) = v
    index = index + 1
    if (index > allocation_size) return
    if (present(w)) int64_array_new%data(index) = w
    index = index + 1
    if (index > allocation_size) return
    if (present(x)) int64_array_new%data(index) = x
    index = index + 1
    if (index > allocation_size) return
    if (present(y)) int64_array_new%data(index) = y
    index = index + 1
    if (index > allocation_size) return
    if (present(z)) int64_array_new%data(index) = z
  end function constructor_int64


  function constructor_float(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z) result(float_array_new)
    implicit none

    real(c_float), intent(in), optional :: a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z
    type(float_array) :: float_array_new
    integer(c_int) :: index, allocation_size

    allocation_size = ex(a)+ex(b)+ex(c)+ex(d)+ex(e)+ex(f)+ex(g)+ex(h)+ex(i)+ex(j)+ex(k)+ex(l)+ex(m)+ex(n)+ex(o)+ex(p)+ex(q)+ex(r)+ex(s)+ex(t)+ex(u)+ex(v)+ex(w)+ex(x)+ex(y)+ex(z)

    allocate(float_array_new%data(allocation_size))

    index = 1

    if (index > allocation_size) return
    if (present(a)) float_array_new%data(index) = a
    index = index + 1
    if (index > allocation_size) return
    if (present(b)) float_array_new%data(index) = b
    index = index + 1
    if (index > allocation_size) return
    if (present(c)) float_array_new%data(index) = c
    index = index + 1
    if (index > allocation_size) return
    if (present(d)) float_array_new%data(index) = d
    index = index + 1
    if (index > allocation_size) return
    if (present(e)) float_array_new%data(index) = e
    index = index + 1
    if (index > allocation_size) return
    if (present(f)) float_array_new%data(index) = f
    index = index + 1
    if (index > allocation_size) return
    if (present(g)) float_array_new%data(index) = g
    index = index + 1
    if (index > allocation_size) return
    if (present(h)) float_array_new%data(index) = h
    index = index + 1
    if (index > allocation_size) return
    if (present(i)) float_array_new%data(index) = i
    index = index + 1
    if (index > allocation_size) return
    if (present(j)) float_array_new%data(index) = j
    index = index + 1
    if (index > allocation_size) return
    if (present(k)) float_array_new%data(index) = k
    index = index + 1
    if (index > allocation_size) return
    if (present(l)) float_array_new%data(index) = l
    index = index + 1
    if (index > allocation_size) return
    if (present(m)) float_array_new%data(index) = m
    index = index + 1
    if (index > allocation_size) return
    if (present(n)) float_array_new%data(index) = n
    index = index + 1
    if (index > allocation_size) return
    if (present(o)) float_array_new%data(index) = o
    index = index + 1
    if (index > allocation_size) return
    if (present(p)) float_array_new%data(index) = p
    index = index + 1
    if (index > allocation_size) return
    if (present(q)) float_array_new%data(index) = q
    index = index + 1
    if (index > allocation_size) return
    if (present(r)) float_array_new%data(index) = r
    index = index + 1
    if (index > allocation_size) return
    if (present(s)) float_array_new%data(index) = s
    index = index + 1
    if (index > allocation_size) return
    if (present(t)) float_array_new%data(index) = t
    index = index + 1
    if (index > allocation_size) return
    if (present(u)) float_array_new%data(index) = u
    index = index + 1
    if (index > allocation_size) return
    if (present(v)) float_array_new%data(index) = v
    index = index + 1
    if (index > allocation_size) return
    if (present(w)) float_array_new%data(index) = w
    index = index + 1
    if (index > allocation_size) return
    if (present(x)) float_array_new%data(index) = x
    index = index + 1
    if (index > allocation_size) return
    if (present(y)) float_array_new%data(index) = y
    index = index + 1
    if (index > allocation_size) return
    if (present(z)) float_array_new%data(index) = z
  end function constructor_float


  function constructor_double(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z) result(double_array_new)
    implicit none

    real(c_double), intent(in), optional :: a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z
    type(double_array) :: double_array_new
    integer(c_int) :: index, allocation_size

    allocation_size = ex(a)+ex(b)+ex(c)+ex(d)+ex(e)+ex(f)+ex(g)+ex(h)+ex(i)+ex(j)+ex(k)+ex(l)+ex(m)+ex(n)+ex(o)+ex(p)+ex(q)+ex(r)+ex(s)+ex(t)+ex(u)+ex(v)+ex(w)+ex(x)+ex(y)+ex(z)

    allocate(double_array_new%data(allocation_size))

    index = 1

    if (index > allocation_size) return
    if (present(a)) double_array_new%data(index) = a
    index = index + 1
    if (index > allocation_size) return
    if (present(b)) double_array_new%data(index) = b
    index = index + 1
    if (index > allocation_size) return
    if (present(c)) double_array_new%data(index) = c
    index = index + 1
    if (index > allocation_size) return
    if (present(d)) double_array_new%data(index) = d
    index = index + 1
    if (index > allocation_size) return
    if (present(e)) double_array_new%data(index) = e
    index = index + 1
    if (index > allocation_size) return
    if (present(f)) double_array_new%data(index) = f
    index = index + 1
    if (index > allocation_size) return
    if (present(g)) double_array_new%data(index) = g
    index = index + 1
    if (index > allocation_size) return
    if (present(h)) double_array_new%data(index) = h
    index = index + 1
    if (index > allocation_size) return
    if (present(i)) double_array_new%data(index) = i
    index = index + 1
    if (index > allocation_size) return
    if (present(j)) double_array_new%data(index) = j
    index = index + 1
    if (index > allocation_size) return
    if (present(k)) double_array_new%data(index) = k
    index = index + 1
    if (index > allocation_size) return
    if (present(l)) double_array_new%data(index) = l
    index = index + 1
    if (index > allocation_size) return
    if (present(m)) double_array_new%data(index) = m
    index = index + 1
    if (index > allocation_size) return
    if (present(n)) double_array_new%data(index) = n
    index = index + 1
    if (index > allocation_size) return
    if (present(o)) double_array_new%data(index) = o
    index = index + 1
    if (index > allocation_size) return
    if (present(p)) double_array_new%data(index) = p
    index = index + 1
    if (index > allocation_size) return
    if (present(q)) double_array_new%data(index) = q
    index = index + 1
    if (index > allocation_size) return
    if (present(r)) double_array_new%data(index) = r
    index = index + 1
    if (index > allocation_size) return
    if (present(s)) double_array_new%data(index) = s
    index = index + 1
    if (index > allocation_size) return
    if (present(t)) double_array_new%data(index) = t
    index = index + 1
    if (index > allocation_size) return
    if (present(u)) double_array_new%data(index) = u
    index = index + 1
    if (index > allocation_size) return
    if (present(v)) double_array_new%data(index) = v
    index = index + 1
    if (index > allocation_size) return
    if (present(w)) double_array_new%data(index) = w
    index = index + 1
    if (index > allocation_size) return
    if (present(x)) double_array_new%data(index) = x
    index = index + 1
    if (index > allocation_size) return
    if (present(y)) double_array_new%data(index) = y
    index = index + 1
    if (index > allocation_size) return
    if (present(z)) double_array_new%data(index) = z
  end function constructor_double


  function constructor_string(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z) result(string_array_new)
    implicit none

    character(len = *, kind = c_char), intent(in), optional :: a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z
    type(string_array) :: string_array_new
    integer(c_int) :: index, allocation_size

    allocation_size = ex(a)+ex(b)+ex(c)+ex(d)+ex(e)+ex(f)+ex(g)+ex(h)+ex(i)+ex(j)+ex(k)+ex(l)+ex(m)+ex(n)+ex(o)+ex(p)+ex(q)+ex(r)+ex(s)+ex(t)+ex(u)+ex(v)+ex(w)+ex(x)+ex(y)+ex(z)

    allocate(string_array_new%data(allocation_size))

    index = 1

    if (index > allocation_size) return
    if (present(a)) string_array_new%data(index) = a
    index = index + 1
    if (index > allocation_size) return
    if (present(b)) string_array_new%data(index) = b
    index = index + 1
    if (index > allocation_size) return
    if (present(c)) string_array_new%data(index) = c
    index = index + 1
    if (index > allocation_size) return
    if (present(d)) string_array_new%data(index) = d
    index = index + 1
    if (index > allocation_size) return
    if (present(e)) string_array_new%data(index) = e
    index = index + 1
    if (index > allocation_size) return
    if (present(f)) string_array_new%data(index) = f
    index = index + 1
    if (index > allocation_size) return
    if (present(g)) string_array_new%data(index) = g
    index = index + 1
    if (index > allocation_size) return
    if (present(h)) string_array_new%data(index) = h
    index = index + 1
    if (index > allocation_size) return
    if (present(i)) string_array_new%data(index) = i
    index = index + 1
    if (index > allocation_size) return
    if (present(j)) string_array_new%data(index) = j
    index = index + 1
    if (index > allocation_size) return
    if (present(k)) string_array_new%data(index) = k
    index = index + 1
    if (index > allocation_size) return
    if (present(l)) string_array_new%data(index) = l
    index = index + 1
    if (index > allocation_size) return
    if (present(m)) string_array_new%data(index) = m
    index = index + 1
    if (index > allocation_size) return
    if (present(n)) string_array_new%data(index) = n
    index = index + 1
    if (index > allocation_size) return
    if (present(o)) string_array_new%data(index) = o
    index = index + 1
    if (index > allocation_size) return
    if (present(p)) string_array_new%data(index) = p
    index = index + 1
    if (index > allocation_size) return
    if (present(q)) string_array_new%data(index) = q
    index = index + 1
    if (index > allocation_size) return
    if (present(r)) string_array_new%data(index) = r
    index = index + 1
    if (index > allocation_size) return
    if (present(s)) string_array_new%data(index) = s
    index = index + 1
    if (index > allocation_size) return
    if (present(t)) string_array_new%data(index) = t
    index = index + 1
    if (index > allocation_size) return
    if (present(u)) string_array_new%data(index) = u
    index = index + 1
    if (index > allocation_size) return
    if (present(v)) string_array_new%data(index) = v
    index = index + 1
    if (index > allocation_size) return
    if (present(w)) string_array_new%data(index) = w
    index = index + 1
    if (index > allocation_size) return
    if (present(x)) string_array_new%data(index) = x
    index = index + 1
    if (index > allocation_size) return
    if (present(y)) string_array_new%data(index) = y
    index = index + 1
    if (index > allocation_size) return
    if (present(z)) string_array_new%data(index) = z
  end function constructor_string



  !* Sort array of i32. Small to large.
  function array_i32_small_to_large_unique(input) result(output)
    implicit none

    integer(c_int), dimension(:), intent(in) :: input
    integer(c_int), dimension(:), allocatable :: output, temp, worker

    integer :: input_index, current_index

    allocate(output(0))

    input_index = 1
    current_index = 0
    temp = input

    do
      input_index = minloc(temp, dim = 1)

      if (input_index == 0) then
        exit
      end if

      if (current_index /= 0) then
        ! We want to ensure that the values do not repeat.
        if (output(current_index) /= temp(input_index)) then
          worker = array_i32_insert(output, temp(input_index))
          call move_alloc(worker, output)
          current_index = current_index + 1
        end if
      else
        worker = array_i32_insert(output, temp(input_index))
        call move_alloc(worker, output)
        current_index = 1
      end if

      temp = array_i32_remove(temp, input_index)
    end do
  end function array_i32_small_to_large_unique


  !* Remove an index from an i32 array.
  function array_i32_remove(input, index) result(output)
    implicit none

    integer(c_int), dimension(:), intent(in) :: input
    integer(c_int), intent(in), value :: index
    integer(c_int), dimension(:), allocatable :: output
    integer(c_int) :: i, old_index, input_length

    input_length = size(input)

    if (input_length <= 0) then
      output = input
      return
    end if

    allocate(output(input_length - 1))

    old_index = 1

    do i = 1,input_length
      if (i == index) then
        cycle
      end if

      output(old_index) = input(i)
      old_index = old_index + 1
    end do
  end function array_i32_remove


  !* Insert a value at the end of an i32 array.
  function array_i32_insert(input, value_new) result(output)
    implicit none

    integer(c_int), dimension(:), intent(in) :: input
    integer(c_int), intent(in), value :: value_new
    integer(c_int), dimension(:), allocatable :: output
    integer(c_int) :: old_size, i

    old_size = size(input)

    allocate(output(old_size + 1))

    do i = 1,old_size
      output(i) = input(i)
    end do

    output(old_size + 1) = value_new
  end function array_i32_insert


  !* Insert a value at the end of a heap string array.
  function array_string_insert(input, value_new) result(output)
    implicit none

    type(heap_string), dimension(:), intent(in) :: input
    type(heap_string), intent(in), value :: value_new
    type(heap_string), dimension(:), allocatable :: output
    integer(c_int) :: old_size, i

    old_size = size(input)

    allocate(output(old_size + 1))

    do i = 1,old_size
      output(i) = input(i)
    end do

    output(old_size + 1) = value_new
  end function array_string_insert


  !* Insert a value at the end of a memory texture array.
  function array_memory_texture_insert(input, value_new) result(output)
    use :: memory_texture_module
    implicit none

    type(memory_texture), dimension(:), intent(in) :: input
    type(memory_texture), intent(in), value :: value_new
    type(memory_texture), dimension(:), allocatable :: output
    integer(c_int) :: old_size, i

    old_size = size(input)

    allocate(output(old_size + 1))

    do i = 1,old_size
      output(i) = input(i)
    end do

    output(old_size + 1) = value_new
  end function array_memory_texture_insert


end module array
