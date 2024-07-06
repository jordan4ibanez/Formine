module f_string
  use, intrinsic :: iso_c_binding
  implicit none

  private

  public :: string_from_c
  public :: int_to_string

contains

  ! Dump a raw Fortran string pointer into a string.
  subroutine copy_string_pointer(length, input_pointer, output_string)
    use, intrinsic :: iso_c_binding
    implicit none
    character(c_char), pointer :: input_pointer(:)
    character(:), allocatable :: output_string
    integer(4) :: i
    ! Start off with the pointer width.
    integer(4) :: length

    ! Now allocate what is needed into the output string.
    allocate(character(len=length) :: output_string)

    ! Now copy over each character.
    ! print*,length
    do i = 1, length
      output_string(i:i) = input_pointer(i)
    end do
  end subroutine copy_string_pointer

  ! Use this to convert C strings stored in a (character, pointer) into Fortran strings.
  function string_from_c(c_string, size) result(fortran_string)
    use, intrinsic :: iso_c_binding
    implicit none
    ! On the C side. The view is great.
    type(c_ptr), intent(in), value :: c_string
    ! On the Fortran side.
    character(c_char), pointer :: fortran_raw_string(:)
    character(:), allocatable :: fortran_string

    integer :: size
    ! 4 BYTES, aka, 32 bit.
    ! If there is a string bigger than this, we have a problem.
    integer(4) :: i
    integer(4) :: input_length
    ! Starts off as 0
    integer(4) :: length = 0

    ! We must ensure that we are not converting a null pointer
    ! as this can lead to SERIOUS UB.
    if (.not. c_associated(c_string)) then
      !? So we will choose to return a blank string instead of halting.
      !? This comment is left here as a backup and retroactive development documentation.
      ! error stop "string_from_c: NULL POINTER IN C STRING"
      fortran_string = ""
    else
      !? It seems that everything is okay, we will proceed.
      call c_f_pointer(c_string, fortran_raw_string, [size])

      ! Get the size of the character pointer.
      ! This is so we do not go out of bounds.
      ! Manually cast this to 32 bit.
      input_length = int(sizeof(fortran_raw_string))

      ! Let's find the null terminator.
      do i = 1, input_length
        if (fortran_raw_string(i) == achar(0)) then
          length = i - 1
          exit
        end if
      end do

      ! If the length is 0, we literally cannot do anything, so give up.
      if (length > 0) then
        call copy_string_pointer(length, fortran_raw_string, fortran_string)
      else
        fortran_string = ""
      end if
    end if
  end function string_from_c

  ! Convert an integer into an allocated string.
  function int_to_string(i) result(output)
    implicit none
    integer :: i
    character(:), allocatable :: output

    ! If the number is any bigger than this, wat.
    allocate(character(128) :: output)
    write(output, "(i128)") i

    ! Now we shift the whole thing left and trim it to fit.
    output = trim(adjustl(output))

    !? This is simply debug.
    ! print*,"-----"
    ! print*,output//"."
    ! print*,"len: ", len(output)
    ! print*,"-----"

  end function int_to_string

end module f_string
