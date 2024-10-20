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
    integer(c_int) :: index, allocation_size

    allocation_size = ex(a)+ex(b)+ex(c)+ex(d)+ex(e)+ex(f)+ex(g)+ex(h)+ex(i)+ex(j)+ex(k)+ex(l)+ex(m)+ex(n)+ex(o)+ex(p)+ex(q)+ex(r)+ex(s)+ex(t)+ex(u)+ex(v)+ex(w)+ex(x)+ex(y)+ex(z)

    allocate(new_int_array%data(allocation_size))

    index = 1

    if (index > allocation_size) return
    if (present(a)) new_int_array%data(index) = a
    index = index + 1
    if (index > allocation_size) return
    if (present(b)) new_int_array%data(index) = b
    index = index + 1
    if (index > allocation_size) return
    if (present(c)) new_int_array%data(index) = c
    index = index + 1
    if (index > allocation_size) return
    if (present(d)) new_int_array%data(index) = d
    index = index + 1
    if (index > allocation_size) return
    if (present(e)) new_int_array%data(index) = e
    index = index + 1
    if (index > allocation_size) return
    if (present(f)) new_int_array%data(index) = f
    index = index + 1
    if (index > allocation_size) return
    if (present(g)) new_int_array%data(index) = g
    index = index + 1
    if (index > allocation_size) return
    if (present(h)) new_int_array%data(index) = h
    index = index + 1
    if (index > allocation_size) return
    if (present(i)) new_int_array%data(index) = i
    index = index + 1
    if (index > allocation_size) return
    if (present(j)) new_int_array%data(index) = j
    index = index + 1
    if (index > allocation_size) return
    if (present(k)) new_int_array%data(index) = k
    index = index + 1
    if (index > allocation_size) return
    if (present(l)) new_int_array%data(index) = l
    index = index + 1
    if (index > allocation_size) return
    if (present(m)) new_int_array%data(index) = m
    index = index + 1
    if (index > allocation_size) return
    if (present(n)) new_int_array%data(index) = n
    index = index + 1
    if (index > allocation_size) return
    if (present(o)) new_int_array%data(index) = o
    index = index + 1
    if (index > allocation_size) return
    if (present(p)) new_int_array%data(index) = p
    index = index + 1
    if (index > allocation_size) return
    if (present(q)) new_int_array%data(index) = q
    index = index + 1
    if (index > allocation_size) return
    if (present(r)) new_int_array%data(index) = r
    index = index + 1
    if (index > allocation_size) return
    if (present(s)) new_int_array%data(index) = s
    index = index + 1
    if (index > allocation_size) return
    if (present(t)) new_int_array%data(index) = t
    index = index + 1
    if (index > allocation_size) return
    if (present(u)) new_int_array%data(index) = u
    index = index + 1
    if (index > allocation_size) return
    if (present(v)) new_int_array%data(index) = v
    index = index + 1
    if (index > allocation_size) return
    if (present(w)) new_int_array%data(index) = w
    index = index + 1
    if (index > allocation_size) return
    if (present(x)) new_int_array%data(index) = x
    index = index + 1
    if (index > allocation_size) return
    if (present(y)) new_int_array%data(index) = y
    index = index + 1
    if (index > allocation_size) return
    if (present(z)) new_int_array%data(index) = z
  end function constructor_int


  function constructor_int64(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z) result(new_int64_array)
    implicit none

    integer(c_int64_t), intent(in), optional :: a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z
    type(int64_array) :: new_int64_array
    integer(c_int) :: index, allocation_size

    allocation_size = ex(a)+ex(b)+ex(c)+ex(d)+ex(e)+ex(f)+ex(g)+ex(h)+ex(i)+ex(j)+ex(k)+ex(l)+ex(m)+ex(n)+ex(o)+ex(p)+ex(q)+ex(r)+ex(s)+ex(t)+ex(u)+ex(v)+ex(w)+ex(x)+ex(y)+ex(z)

    allocate(new_int64_array%data(allocation_size))

    index = 1

    if (index > allocation_size) return
    if (present(a)) new_int64_array%data(index) = a
    index = index + 1
    if (index > allocation_size) return
    if (present(b)) new_int64_array%data(index) = b
    index = index + 1
    if (index > allocation_size) return
    if (present(c)) new_int64_array%data(index) = c
    index = index + 1
    if (index > allocation_size) return
    if (present(d)) new_int64_array%data(index) = d
    index = index + 1
    if (index > allocation_size) return
    if (present(e)) new_int64_array%data(index) = e
    index = index + 1
    if (index > allocation_size) return
    if (present(f)) new_int64_array%data(index) = f
    index = index + 1
    if (index > allocation_size) return
    if (present(g)) new_int64_array%data(index) = g
    index = index + 1
    if (index > allocation_size) return
    if (present(h)) new_int64_array%data(index) = h
    index = index + 1
    if (index > allocation_size) return
    if (present(i)) new_int64_array%data(index) = i
    index = index + 1
    if (index > allocation_size) return
    if (present(j)) new_int64_array%data(index) = j
    index = index + 1
    if (index > allocation_size) return
    if (present(k)) new_int64_array%data(index) = k
    index = index + 1
    if (index > allocation_size) return
    if (present(l)) new_int64_array%data(index) = l
    index = index + 1
    if (index > allocation_size) return
    if (present(m)) new_int64_array%data(index) = m
    index = index + 1
    if (index > allocation_size) return
    if (present(n)) new_int64_array%data(index) = n
    index = index + 1
    if (index > allocation_size) return
    if (present(o)) new_int64_array%data(index) = o
    index = index + 1
    if (index > allocation_size) return
    if (present(p)) new_int64_array%data(index) = p
    index = index + 1
    if (index > allocation_size) return
    if (present(q)) new_int64_array%data(index) = q
    index = index + 1
    if (index > allocation_size) return
    if (present(r)) new_int64_array%data(index) = r
    index = index + 1
    if (index > allocation_size) return
    if (present(s)) new_int64_array%data(index) = s
    index = index + 1
    if (index > allocation_size) return
    if (present(t)) new_int64_array%data(index) = t
    index = index + 1
    if (index > allocation_size) return
    if (present(u)) new_int64_array%data(index) = u
    index = index + 1
    if (index > allocation_size) return
    if (present(v)) new_int64_array%data(index) = v
    index = index + 1
    if (index > allocation_size) return
    if (present(w)) new_int64_array%data(index) = w
    index = index + 1
    if (index > allocation_size) return
    if (present(x)) new_int64_array%data(index) = x
    index = index + 1
    if (index > allocation_size) return
    if (present(y)) new_int64_array%data(index) = y
    index = index + 1
    if (index > allocation_size) return
    if (present(z)) new_int64_array%data(index) = z
  end function constructor_int64


  function constructor_float(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z) result(new_float_array)
    implicit none

    real(c_float), intent(in), optional :: a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z
    type(float_array) :: new_float_array
    integer(c_int) :: index, allocation_size

    allocation_size = ex(a)+ex(b)+ex(c)+ex(d)+ex(e)+ex(f)+ex(g)+ex(h)+ex(i)+ex(j)+ex(k)+ex(l)+ex(m)+ex(n)+ex(o)+ex(p)+ex(q)+ex(r)+ex(s)+ex(t)+ex(u)+ex(v)+ex(w)+ex(x)+ex(y)+ex(z)

    allocate(new_float_array%data(allocation_size))

    index = 1

    if (index > allocation_size) return
    if (present(a)) new_float_array%data(index) = a
    index = index + 1
    if (index > allocation_size) return
    if (present(b)) new_float_array%data(index) = b
    index = index + 1
    if (index > allocation_size) return
    if (present(c)) new_float_array%data(index) = c
    index = index + 1
    if (index > allocation_size) return
    if (present(d)) new_float_array%data(index) = d
    index = index + 1
    if (index > allocation_size) return
    if (present(e)) new_float_array%data(index) = e
    index = index + 1
    if (index > allocation_size) return
    if (present(f)) new_float_array%data(index) = f
    index = index + 1
    if (index > allocation_size) return
    if (present(g)) new_float_array%data(index) = g
    index = index + 1
    if (index > allocation_size) return
    if (present(h)) new_float_array%data(index) = h
    index = index + 1
    if (index > allocation_size) return
    if (present(i)) new_float_array%data(index) = i
    index = index + 1
    if (index > allocation_size) return
    if (present(j)) new_float_array%data(index) = j
    index = index + 1
    if (index > allocation_size) return
    if (present(k)) new_float_array%data(index) = k
    index = index + 1
    if (index > allocation_size) return
    if (present(l)) new_float_array%data(index) = l
    index = index + 1
    if (index > allocation_size) return
    if (present(m)) new_float_array%data(index) = m
    index = index + 1
    if (index > allocation_size) return
    if (present(n)) new_float_array%data(index) = n
    index = index + 1
    if (index > allocation_size) return
    if (present(o)) new_float_array%data(index) = o
    index = index + 1
    if (index > allocation_size) return
    if (present(p)) new_float_array%data(index) = p
    index = index + 1
    if (index > allocation_size) return
    if (present(q)) new_float_array%data(index) = q
    index = index + 1
    if (index > allocation_size) return
    if (present(r)) new_float_array%data(index) = r
    index = index + 1
    if (index > allocation_size) return
    if (present(s)) new_float_array%data(index) = s
    index = index + 1
    if (index > allocation_size) return
    if (present(t)) new_float_array%data(index) = t
    index = index + 1
    if (index > allocation_size) return
    if (present(u)) new_float_array%data(index) = u
    index = index + 1
    if (index > allocation_size) return
    if (present(v)) new_float_array%data(index) = v
    index = index + 1
    if (index > allocation_size) return
    if (present(w)) new_float_array%data(index) = w
    index = index + 1
    if (index > allocation_size) return
    if (present(x)) new_float_array%data(index) = x
    index = index + 1
    if (index > allocation_size) return
    if (present(y)) new_float_array%data(index) = y
    index = index + 1
    if (index > allocation_size) return
    if (present(z)) new_float_array%data(index) = z
  end function constructor_float


  function constructor_double(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z) result(new_double_array)
    implicit none

    real(c_double), intent(in), optional :: a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z
    type(double_array) :: new_double_array
    integer(c_int) :: index, allocation_size

    allocation_size = ex(a)+ex(b)+ex(c)+ex(d)+ex(e)+ex(f)+ex(g)+ex(h)+ex(i)+ex(j)+ex(k)+ex(l)+ex(m)+ex(n)+ex(o)+ex(p)+ex(q)+ex(r)+ex(s)+ex(t)+ex(u)+ex(v)+ex(w)+ex(x)+ex(y)+ex(z)

    allocate(new_double_array%data(allocation_size))

    index = 1

    if (index > allocation_size) return
    if (present(a)) new_double_array%data(index) = a
    index = index + 1
    if (index > allocation_size) return
    if (present(b)) new_double_array%data(index) = b
    index = index + 1
    if (index > allocation_size) return
    if (present(c)) new_double_array%data(index) = c
    index = index + 1
    if (index > allocation_size) return
    if (present(d)) new_double_array%data(index) = d
    index = index + 1
    if (index > allocation_size) return
    if (present(e)) new_double_array%data(index) = e
    index = index + 1
    if (index > allocation_size) return
    if (present(f)) new_double_array%data(index) = f
    index = index + 1
    if (index > allocation_size) return
    if (present(g)) new_double_array%data(index) = g
    index = index + 1
    if (index > allocation_size) return
    if (present(h)) new_double_array%data(index) = h
    index = index + 1
    if (index > allocation_size) return
    if (present(i)) new_double_array%data(index) = i
    index = index + 1
    if (index > allocation_size) return
    if (present(j)) new_double_array%data(index) = j
    index = index + 1
    if (index > allocation_size) return
    if (present(k)) new_double_array%data(index) = k
    index = index + 1
    if (index > allocation_size) return
    if (present(l)) new_double_array%data(index) = l
    index = index + 1
    if (index > allocation_size) return
    if (present(m)) new_double_array%data(index) = m
    index = index + 1
    if (index > allocation_size) return
    if (present(n)) new_double_array%data(index) = n
    index = index + 1
    if (index > allocation_size) return
    if (present(o)) new_double_array%data(index) = o
    index = index + 1
    if (index > allocation_size) return
    if (present(p)) new_double_array%data(index) = p
    index = index + 1
    if (index > allocation_size) return
    if (present(q)) new_double_array%data(index) = q
    index = index + 1
    if (index > allocation_size) return
    if (present(r)) new_double_array%data(index) = r
    index = index + 1
    if (index > allocation_size) return
    if (present(s)) new_double_array%data(index) = s
    index = index + 1
    if (index > allocation_size) return
    if (present(t)) new_double_array%data(index) = t
    index = index + 1
    if (index > allocation_size) return
    if (present(u)) new_double_array%data(index) = u
    index = index + 1
    if (index > allocation_size) return
    if (present(v)) new_double_array%data(index) = v
    index = index + 1
    if (index > allocation_size) return
    if (present(w)) new_double_array%data(index) = w
    index = index + 1
    if (index > allocation_size) return
    if (present(x)) new_double_array%data(index) = x
    index = index + 1
    if (index > allocation_size) return
    if (present(y)) new_double_array%data(index) = y
    index = index + 1
    if (index > allocation_size) return
    if (present(z)) new_double_array%data(index) = z
  end function constructor_double


  function constructor_string(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z) result(new_string_array)
    implicit none

    character(len = *, kind = c_char), intent(in), optional :: a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z
    type(string_array) :: new_string_array
    integer(c_int) :: index, allocation_size

    allocation_size = ex(a)+ex(b)+ex(c)+ex(d)+ex(e)+ex(f)+ex(g)+ex(h)+ex(i)+ex(j)+ex(k)+ex(l)+ex(m)+ex(n)+ex(o)+ex(p)+ex(q)+ex(r)+ex(s)+ex(t)+ex(u)+ex(v)+ex(w)+ex(x)+ex(y)+ex(z)

    allocate(new_string_array%data(allocation_size))

    index = 1

    if (index > allocation_size) return
    if (present(a)) new_string_array%data(index) = a
    index = index + 1
    if (index > allocation_size) return
    if (present(b)) new_string_array%data(index) = b
    index = index + 1
    if (index > allocation_size) return
    if (present(c)) new_string_array%data(index) = c
    index = index + 1
    if (index > allocation_size) return
    if (present(d)) new_string_array%data(index) = d
    index = index + 1
    if (index > allocation_size) return
    if (present(e)) new_string_array%data(index) = e
    index = index + 1
    if (index > allocation_size) return
    if (present(f)) new_string_array%data(index) = f
    index = index + 1
    if (index > allocation_size) return
    if (present(g)) new_string_array%data(index) = g
    index = index + 1
    if (index > allocation_size) return
    if (present(h)) new_string_array%data(index) = h
    index = index + 1
    if (index > allocation_size) return
    if (present(i)) new_string_array%data(index) = i
    index = index + 1
    if (index > allocation_size) return
    if (present(j)) new_string_array%data(index) = j
    index = index + 1
    if (index > allocation_size) return
    if (present(k)) new_string_array%data(index) = k
    index = index + 1
    if (index > allocation_size) return
    if (present(l)) new_string_array%data(index) = l
    index = index + 1
    if (index > allocation_size) return
    if (present(m)) new_string_array%data(index) = m
    index = index + 1
    if (index > allocation_size) return
    if (present(n)) new_string_array%data(index) = n
    index = index + 1
    if (index > allocation_size) return
    if (present(o)) new_string_array%data(index) = o
    index = index + 1
    if (index > allocation_size) return
    if (present(p)) new_string_array%data(index) = p
    index = index + 1
    if (index > allocation_size) return
    if (present(q)) new_string_array%data(index) = q
    index = index + 1
    if (index > allocation_size) return
    if (present(r)) new_string_array%data(index) = r
    index = index + 1
    if (index > allocation_size) return
    if (present(s)) new_string_array%data(index) = s
    index = index + 1
    if (index > allocation_size) return
    if (present(t)) new_string_array%data(index) = t
    index = index + 1
    if (index > allocation_size) return
    if (present(u)) new_string_array%data(index) = u
    index = index + 1
    if (index > allocation_size) return
    if (present(v)) new_string_array%data(index) = v
    index = index + 1
    if (index > allocation_size) return
    if (present(w)) new_string_array%data(index) = w
    index = index + 1
    if (index > allocation_size) return
    if (present(x)) new_string_array%data(index) = x
    index = index + 1
    if (index > allocation_size) return
    if (present(y)) new_string_array%data(index) = y
    index = index + 1
    if (index > allocation_size) return
    if (present(z)) new_string_array%data(index) = z
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
  function array_i32_insert(input, new_value) result(output)
    implicit none

    integer(c_int), dimension(:), intent(in) :: input
    integer(c_int), intent(in), value :: new_value
    integer(c_int), dimension(:), allocatable :: output
    integer(c_int) :: old_size, i

    old_size = size(input)

    allocate(output(old_size + 1))

    do i = 1,old_size
      output(i) = input(i)
    end do

    output(old_size + 1) = new_value
  end function array_i32_insert


  !* Insert a value at the end of a heap string array.
  function array_string_insert(input, new_value) result(output)
    implicit none

    type(heap_string), dimension(:), intent(in) :: input
    type(heap_string), intent(in), value :: new_value
    type(heap_string), dimension(:), allocatable :: output
    integer(c_int) :: old_size, i

    old_size = size(input)

    allocate(output(old_size + 1))

    do i = 1,old_size
      output(i) = input(i)
    end do

    output(old_size + 1) = new_value
  end function array_string_insert


  !* Insert a value at the end of a memory texture array.
  function array_memory_texture_insert(input, new_value) result(output)
    use :: memory_texture_module
    implicit none

    type(memory_texture), dimension(:), intent(in) :: input
    type(memory_texture), intent(in), value :: new_value
    type(memory_texture), dimension(:), allocatable :: output
    integer(c_int) :: old_size, i

    old_size = size(input)

    allocate(output(old_size + 1))

    do i = 1,old_size
      output(i) = input(i)
    end do

    output(old_size + 1) = new_value
  end function array_memory_texture_insert


end module array
