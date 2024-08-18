module string
  use, intrinsic :: iso_c_binding
  use :: h_string
  implicit none


  private


  public :: string_from_c

  public :: int_to_string
  public :: long_to_string
  public :: into_c_string
  public :: bool_to_string
  public :: string_to_int
  public :: string_to_int64

  public :: get_file_name_from_string
  public :: string_remove_file_name_from_path
  public :: string_remove_file_extension
  public :: string_get_file_extension
  public :: string_cut_first

  public :: string_get_non_space_characters
  public :: string_starts_with
  public :: string_ends_with
  public :: string_contains_character

  public :: string_trim_white_space
  public :: string_get_right_of_character
  public :: string_get_left_of_character

  !? Pass through the type.
  public :: heap_string


contains


  !* Dump a raw Fortran string pointer into a string.
  !* Returns the new string.
  function convert_c_string_pointer_to_string(length, input_pointer) result(output_string)
    use, intrinsic :: iso_c_binding
    implicit none

    character(c_char), pointer :: input_pointer(:)
    character(:), allocatable :: output_string
    ! Start off with the pointer width.
    integer(c_int) :: i, length

    ! Now allocate what is needed into the output string.
    allocate(character(len = length, kind = c_char) :: output_string)

    ! Now copy over each character.
    do i = 1, length
      output_string(i:i) = input_pointer(i)
    end do
  end function convert_c_string_pointer_to_string


  !* Use this to convert C strings stored in a (character, pointer) into Fortran strings.
  function string_from_c(c_string_pointer, string_length) result(fortran_string)
    use, intrinsic :: iso_c_binding
    implicit none

    ! On the C side. The view is great.
    type(c_ptr), intent(in), value :: c_string_pointer
    ! On the Fortran side.
    character(kind = c_char), dimension(:), pointer :: fortran_string_pointer
    character(len = :, kind = c_char), allocatable :: fortran_string
    integer :: string_length, found_string_length
    ! 4 BYTES, aka, 32 bit.
    ! If there is a string bigger than this, we have a problem.
    integer(c_int) :: i

    ! Starts off as 0
    found_string_length = 0

    ! We must ensure that we are not converting a null pointer
    ! as this can lead to SERIOUS UB.
    if (.not. c_associated(c_string_pointer)) then
      !? So we will choose to return a blank string instead of halting.
      !? This comment is left here as a backup and retroactive development documentation.
      ! error stop "string_from_c: NULL POINTER IN C STRING"
      fortran_string = ""
    else
      !? It seems that everything is okay, we will proceed.
      call c_f_pointer(c_string_pointer, fortran_string_pointer, [string_length])

      fortran_string = convert_c_string_pointer_to_string(string_length, fortran_string_pointer)

      !? This will crash, it automatically gets deallocated.
      !? I have this here in case I audit my code in the future.
      ! deallocate(fortran_string_pointer)

      ! Force a null terminator to be applied.
      !? This prevents strange behavior when C misbehaves.
      fortran_string(string_length:string_length) = achar(0)

      ! Let's find the null terminator.
      do i = 1,string_length
        ! print*,fortran_raw_string(i)
        if (fortran_string(i:i) == achar(0)) then
          found_string_length = i - 1
          exit
        end if
      end do

      ! If the length is 0, we literally cannot do anything, so give up.
      if (found_string_length > 0) then
        ! Trim the string.
        fortran_string = fortran_string(1:found_string_length)
      else
        fortran_string = ""
      end if
    end if
  end function string_from_c


  ! Convert a regular Fortran string into a null terminated C string.
  !* Allocated, remember to deallocate!
  function into_c_string(input) result(output)
    implicit none

    character(len = *, kind = c_char) :: input
    character(len = :, kind = c_char), allocatable :: output

    ! Simply shove that string into the allocated string and null terminate it.
    !? This seems to automatically allocate so don't allocate for no reason.
    output = input//achar(0)
  end function into_c_string


  ! Convert an integer into an allocated string.
  !* Allocatable will deallocate once the memory goes out of scope.
  function int_to_string(i) result(output)
    implicit none

    integer(c_int) :: i
    character(:, kind = c_char), allocatable :: output

    ! If the number is any bigger than this, wat.
    allocate(character(128) :: output)
    write(output, "(i128)") i

    ! Now we shift the whole thing left and trim it to fit.
    output = trim(adjustl(output))

    !? This is simply debug.
    ! print"(A)","-----"
    ! print"(A)","["//output//"]"
    ! print"(A)","len: ", len(output)
    ! print"(A)","-----"
  end function int_to_string


  ! Convert an integer into an allocated string.
  !* Allocatable will deallocate once the memory goes out of scope.
  function long_to_string(i) result(output)
    implicit none

    integer(c_int64_t) :: i
    character(:, kind = c_char), allocatable :: output

    ! If the number is any bigger than this, wat.
    allocate(character(128) :: output)
    write(output, "(i128)") i

    ! Now we shift the whole thing left and trim it to fit.
    output = trim(adjustl(output))

    !? This is simply debug.
    ! print"(A)","-----"
    ! print*,i
    ! print"(A)","["//output//"]"
    ! print"(A)","len: "//int_to_string(len(output))
    ! print"(A)","-----"
  end function long_to_string


  ! Convert a logical into an allocated string.
  !* Allocatable will deallocate once the memory goes out of scope.
  function bool_to_string(bool) result(output)
    implicit none

    logical :: bool
    character(len = :), allocatable :: output

    allocate(character(5) :: output)

    ! Simply write true or false into the string.
    if (bool) then
      write(output, "(A)") "true"
    else
      write(output, "(A)") "false"
    end if

    ! Now we shift the whole thing left and trim it to fit.
    output = trim(adjustl(output))
  end function bool_to_string


  !* Convert a string to an int.
  function string_to_int(input_string) result(int)
    implicit none

    character(len = *, kind = c_char), intent(in) :: input_string
    integer(kind = c_int) :: int

    int = 0

    ! Don't parse empty strings.
    if (input_string == "") then
      return
    end if

    read(input_string, *) int
  end function string_to_int


  !* Convert a string to an int.
  function string_to_int64(input_string) result(int64)
    implicit none

    character(len = *, kind = c_char), intent(in) :: input_string
    integer(kind = c_int64_t) :: int64

    int64 = 0

    ! Don't parse empty strings.
    if (input_string == "") then
      return
    end if

    read(input_string, *) int64
  end function string_to_int64



  !* Get a file name string from a string that is a path.
  function get_file_name_from_string(input_string) result(resulting_name_of_file)
    use, intrinsic :: iso_c_binding, only: c_char
    implicit none

    character(len = *, kind = c_char), intent(in) :: input_string
    character(len = :, kind = c_char), allocatable :: resulting_name_of_file
    integer :: i, length_of_string

    i = index(input_string, "/", back = .true.)

    ! This probably isn't a path.
    if (i == 0) then
      print"(A)", achar(27)//"[38;2;255;128;0m[String] Warning: Could not extract file name from directory."//achar(27)//"[m"
      resulting_name_of_file = ""
      return
    end if

    length_of_string = len(input_string)

    ! This is a folder.
    if (i == length_of_string) then
      print"(A)", achar(27)//"[38;2;255;128;0m[String] Warning: Tried to get file name of folder."//achar(27)//"[m"
      resulting_name_of_file = ""
      return
    end if

    ! So this is a file. Let's now get it
    resulting_name_of_file = input_string(i + 1:length_of_string)
  end function get_file_name_from_string


  !* Can convert [./test/cool.png] into [./test/]
  function string_remove_file_name_from_path(input_file_name) result(file_name_without_extension)
    implicit none

    character(len = *), intent(in) :: input_file_name
    character(len = :), allocatable :: file_name_without_extension
    integer(c_int) :: i

    i = index(input_file_name, "/", back = .true.)

    if (i == 0) then
      print"(A)", achar(27)//"[38;2;255;128;0m[String] Warning: Tried to remove file name off string that's not a file path."//achar(27)//"[m"
      file_name_without_extension = ""
      return
    end if

    file_name_without_extension = input_file_name(1:i)
  end function string_remove_file_name_from_path


  !* Convert something like "test.png" into "test"
  function string_remove_file_extension(input_file_name) result(file_name_without_extension)
    implicit none

    character(len = *), intent(in) :: input_file_name
    character(len = :), allocatable :: file_name_without_extension
    integer(c_int) :: i

    i = index(input_file_name, ".", back = .true.)

    if (i == 0) then
      print"(A)", achar(27)//"[38;2;255;128;0m[String] Warning: Tried to remove file extension off string that's not a file name."//achar(27)//"[m"
      file_name_without_extension = ""
      return
    end if

    file_name_without_extension = input_file_name(1:i - 1)
  end function string_remove_file_extension


  !* Get a file extension from a string.
  !* If it has no extension, this returns "".
  function string_get_file_extension(input_file_name) result(extension)
    implicit none

    character(len = *, kind = c_char), intent(in) :: input_file_name
    character(len = :, kind = c_char), allocatable :: extension
    integer(c_int) :: i, string_length

    i = index(input_file_name, ".", back = .true.)

    ! Has no extension.
    if (i == 0) then
      extension = ""
      return
    end if

    ! Move the index past the [.]
    i = i + 1

    string_length = len(input_file_name)

    ! Probably a sentence?
    if (i >= string_length) then
      extension = ""
      return
    end if

    extension = input_file_name(i:string_length)
  end function string_get_file_extension


  !* Cut the first instance of a substring out of a string. Left to right.
  function string_cut_first(input_string, sub_string) result(cut_string)
    implicit none

    character(len = *, kind = c_char), intent(in) :: input_string, sub_string
    character(len = :, kind = c_char), allocatable :: cut_string
    integer :: found_index, sub_string_width, inner_left, inner_right, outer_left, outer_right
    

    ! Starts off as the input.
    cut_string = input_string

    ! If blank, return.
    if (input_string == "") then
      return
    end if

    ! If a character, there's no way to really work with that.

    found_index = index(input_string, sub_string)

    ! Not found.
    if (found_index == 0) then
      return
    end if

    sub_string_width = len(sub_string)

    ! Left side of the target.
    inner_left = 1
    inner_right = found_index - 1

    ! Right side of the target.
    outer_left = found_index + sub_string_width
    outer_right = len(input_string)

    ! Now we just concatenate the beginning and ending together.
    cut_string = input_string(inner_left:inner_right)//input_string(outer_left:outer_right)
  end function string_cut_first


  !* Get the count of non space characters in a string.
  !* So "a b c" is a count of 3.
  function string_get_non_space_characters(input_string) result(character_count)
    implicit none

    character(len = *), intent(in) :: input_string
    integer(c_int) :: character_count, i

    character_count = 0

    ! Yeah, we're literally just counting the non space characters.
    do i = 1,len(input_string)
      if (input_string(i:i) == " ") then
        cycle
      end if
      character_count = character_count + 1
    end do
  end function string_get_non_space_characters


  !* Check if a string starts with a sub string.
  function string_starts_with(input_string, sub_string) result(starts_with)
    implicit none

    character(len = *, kind = c_char), intent(in) :: input_string, sub_string
    logical :: starts_with

    starts_with = .false.

    ! Blank.
    if (sub_string == "" .or. input_string == "") then
      return
    end if

    starts_with = index(input_string, sub_string) == 1
  end function string_starts_with


  !* Check if a string ends with a sub string.
  function string_ends_with(input_string, sub_string) result(ends_with)
    implicit none

    character(len = *, kind = c_char), intent(in) :: input_string, sub_string
    logical :: ends_with
    integer(c_int) :: input_length, sub_string_length, found_index

    ends_with = .false.

    ! Blank.
    if (sub_string == "" .or. input_string == "") then
      return
    end if

    found_index = index(input_string, sub_string, back = .true.)

    ! Not found.
    if (found_index == 0) then
      return
    end if

    input_length = len(input_string)
    sub_string_length = len(sub_string)

    ! We can simply check if adding the found index with the size
    ! of the substring matches the input length.
    ! Subtract 1 to push it back to 0 index.
    ! [hi there] check [re]
    ! found at [7], sub length: [2], in length: [8]
    ! [[7 + 2] - 1] == 8 == .true.
    ends_with = (found_index + sub_string_length) - 1 == input_length
  end function string_ends_with


  !* Check if a string has a character.
  function string_contains_character(input_string, char) result(has_char)
    implicit none

    character(len = *, kind = c_char), intent(in) :: input_string
    character(len = 1, kind = c_char), intent(in) :: char
    logical :: has_char

    has_char = index(input_string, char) /= 0
  end function string_contains_character


  !* Strip leading and trailing white space off a string.
  function string_trim_white_space(input_string) result(output_string)
    implicit none

    character(len = *, kind = c_char), intent(in) :: input_string
    character(len = :, kind = c_char), allocatable :: output_string

    ! This is kind of like how you remove bits in a bit shift, but for strings.
    output_string = trim(adjustl(input_string))
  end function string_trim_white_space


  !* This helper function is mainly made for parsing conf files.
  !* It will remove all surrounding space.
  !* Will return "" if can't parse.
  !* Example:
  !* test = blah
  !* return: [blah]
  function string_get_right_of_character(input_string, char) result(output_string)
    implicit none

    character(len = *, kind = c_char), intent(in) :: input_string
    character(len = 1, kind = c_char), intent(in) :: char
    character(len = :, kind = c_char), allocatable :: output_string
    integer(c_int) :: found_index, input_length

    found_index = index(input_string, char)

    ! No character found.
    if (found_index == 0) then
      output_string = ""
      return
    end if

    input_length = len(input_string)

    ! Shift it to the right of the found character index.
    found_index = found_index + 1

    ! Out of bounds.
    if (found_index > input_length) then
      output_string = ""
      return
    end if

    ! Then process it.
    output_string = input_string(found_index:input_length)
    output_string = string_trim_white_space(output_string)
  end function string_get_right_of_character


  !* This helper function is mainly made for parsing conf files.
  !* It will remove all surrounding space.
  !* Will return "" if can't parse.
  !* Example:
  !* test = blah
  !* return: [test]
  function string_get_left_of_character(input_string, char) result(output_string)
    implicit none

    character(len = *, kind = c_char), intent(in) :: input_string
    character(len = 1, kind = c_char), intent(in) :: char
    character(len = :, kind = c_char), allocatable :: output_string
    integer(c_int) :: found_index

    found_index = index(input_string, char)

    ! No character found.
    if (found_index == 0) then
      output_string = ""
      return
    end if

    ! Shift it to the left of the found character index.
    found_index = found_index - 1

    ! Out of bounds.
    if (found_index <= 0) then
      output_string = ""
      return
    end if

    ! Then process it.
    output_string = input_string(1:found_index)
    output_string = string_trim_white_space(output_string)
  end function string_get_left_of_character


end module string
