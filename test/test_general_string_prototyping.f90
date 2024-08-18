module test_suite_string_prototyping
  use :: string
  use :: testament
  use, intrinsic :: iso_c_binding
  implicit none


contains

  subroutine starts_with()
    implicit none

    character(len = :, kind = c_char), allocatable :: unit_1, unit_2, unit_3

    print*, "BEGIN TESTING STRING STARTS WITH."

    unit_1 = "hi there"

    call assert_true(string_starts_with(unit_1, "hi"))
    call assert_false(string_starts_with(unit_1, "i"))
    call assert_true(string_starts_with(unit_1, "hi "))
    call assert_true(string_starts_with(unit_1, "hi t"))
    call assert_false(string_starts_with(unit_1, ""))
    call assert_false(string_starts_with(unit_1, " "))

    unit_2 = "[test = 1]"

    call assert_true(string_starts_with(unit_2, "["))
    call assert_false(string_starts_with(unit_2, "[t "))
    call assert_true(string_starts_with(unit_2, "[tes"))
    call assert_true(string_starts_with(unit_2, "[test"))
    call assert_false(string_starts_with(unit_2, ""))
    call assert_false(string_starts_with(unit_2, " "))

    unit_3 = ""

    call assert_false(string_starts_with(unit_3, "["))
    call assert_false(string_starts_with(unit_3, "[t "))
    call assert_false(string_starts_with(unit_3, "[tes"))
    call assert_false(string_starts_with(unit_3, "[test"))
    call assert_false(string_starts_with(unit_3, ""))
    call assert_false(string_starts_with(unit_3, " "))
  end subroutine starts_with


  subroutine ends_with()
    implicit none

    character(len = :, kind = c_char), allocatable :: unit_1, unit_2, unit_3

    print*, "BEGIN TESTING STRING ENDS WITH."

    unit_1 = "hi there"

    call assert_true(string_ends_with(unit_1, "there"))
    call assert_false(string_ends_with(unit_1, "er"))
    call assert_true(string_ends_with(unit_1, " there"))
    call assert_true(string_ends_with(unit_1, "here"))
    call assert_false(string_ends_with(unit_1, "1"))
    call assert_false(string_ends_with(unit_1, ""))
    call assert_false(string_ends_with(unit_1, " "))

    unit_2 = "[test = 1]"

    call assert_true(string_ends_with(unit_2, "]"))
    call assert_false(string_ends_with(unit_2, "1] "))
    call assert_true(string_ends_with(unit_2, " 1]"))
    call assert_true(string_ends_with(unit_2, " = 1]"))
    call assert_false(string_ends_with(unit_2, ""))
    call assert_false(string_ends_with(unit_2, " "))

    unit_3 = ""

    call assert_false(string_starts_with(unit_3, "["))
    call assert_false(string_starts_with(unit_3, "[t "))
    call assert_false(string_starts_with(unit_3, "[tes"))
    call assert_false(string_starts_with(unit_3, "[test"))
    call assert_false(string_starts_with(unit_3, ""))
    call assert_false(string_starts_with(unit_3, " "))
  end subroutine ends_with


end module test_suite_string_prototyping

program test_string_prototyping
  use :: test_suite_string_prototyping
  implicit none

  call starts_with()

  call ends_with()

end program test_string_prototyping
