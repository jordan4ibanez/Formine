module test_suite_string
  use string
  use testament
  implicit none

  !! Not private.


contains


  subroutine assign_test()
    implicit none

    type(heap_string) :: unit_1

    unit_1 = "hi this is a test"

    call assert_str_equal(unit_1%get(), "hi this is a test")
  end subroutine assign_test


  subroutine equality_test()
    implicit none

    type(heap_string) :: unit_1
    type(heap_string) :: unit_2
    character(len = :), allocatable :: comparitor_1

    unit_1 = "testing"
    unit_2 = "not testing"

    comparitor_1 = "testing"

    ! Preliminary run up.
    call assert_str_equal(unit_1%get(), comparitor_1)
    call assert_str_not_equal(unit_2%get(), comparitor_1)
    call assert_str_not_equal(unit_1%get(), unit_2%get())

    ! Now test the operator.
    call assert_true(unit_1 == comparitor_1)
    call assert_false(unit_2 == comparitor_1)
    call assert_false(unit_1 == unit_2)
  end subroutine equality_test


  subroutine is_allocated_test()
    implicit none

    type(heap_string) :: unit_1

    call assert_false(unit_1%is_allocated())

    unit_1 = "allocation test"

    call assert_true(unit_1%is_allocated())
  end subroutine is_allocated_test


  subroutine get_test()
    implicit none

    type(heap_string) :: unit_1

    call assert_str_equal(unit_1%get(), "")

    unit_1 = "test"

    call assert_str_equal(unit_1%get(), "test")
  end subroutine get_test


  subroutine append_test()
    implicit none

    type(heap_string) :: unit_1

    unit_1 = "hi"

    call unit_1%append(" ")

    call assert_str_equal(unit_1%get(), "hi ")

    call unit_1%append("there")

    call assert_str_equal(unit_1%get(), "hi there")
  end subroutine append_test


  subroutine prepend_test()
    implicit none

    type(heap_string) :: unit_1

    unit_1 = "there"

    call unit_1%prepend(" ")

    call assert_str_equal(unit_1%get(), " there")

    call unit_1%prepend("hi")

    call assert_str_equal(unit_1%get(), "hi there")
  end subroutine prepend_test


  subroutine strip_test()
    implicit none

    type(heap_string) :: unit_1

    unit_1 = "   test   "

    call unit_1%strip()

    call assert_str_equal(unit_1%get(), "test")
  end subroutine strip_test


  subroutine cut_test()
    implicit none

    type(heap_string) :: unit_1
    type(heap_string) :: unit_2
    type(heap_string) :: unit_3
    type(heap_string) :: unit_4


    ! Middle.
    unit_1 = "test/hello/there"

    call unit_1%cut("hello")

    call assert_str_equal(unit_1%get(), "test//there")


    ! Inner.
    unit_2 = "blorf=cool"

    call unit_2%cut("blorf")

    call assert_str_equal(unit_2%get(), "=cool")


    ! Outer.
    unit_3 = "shmecin!@#^Testing"

    call unit_3%cut("@#^Testing")

    call assert_str_equal(unit_3%get(), "shmecin!")


    ! Entire.
    unit_4 = "oh man"

    call unit_4%cut("oh man")

    call assert_str_equal(unit_4%get(), "")
  end subroutine cut_test


  subroutine cut_last_test()
    implicit none

    type(heap_string) :: unit_1

    unit_1 = "hi there person, there is a thing!"

    call unit_1%cut_last("there")

    call assert_str_equal(unit_1%get(), "hi there person,  is a thing!")
  end subroutine cut_last_test


  subroutine cut_all_test()
    implicit none

    !? This is tested like this because it should behave exactly like looping the cut method.

    type(heap_string) :: unit_1
    type(heap_string) :: unit_2
    type(heap_string) :: unit_3

    ! This is insanely nonsensical on purpose.
    unit_1 = "hi/bye_hi/flar!phi/hi%hi%hi%hi!hi>hi!byehi"

    call unit_1%cut_all("hi")

    call assert_str_equal(unit_1%get(), "/bye_/flar!p/%%%!>!bye")


    ! This one is more simple.
    unit_2 = "https://www.google.com"

    call unit_2%cut_all("http://www.")

    ! This should miss, it's HTTP not HTTPS.
    call assert_str_not_equal(unit_2%get(), "google.com")

    call unit_2%cut_all("https://www.")

    ! This should hit, it's now stripping out HTTPS.
    call assert_str_equal(unit_2%get(), "google.com")


    ! This is more realistic. We want to remove a "file name" from the string to get the directory.
    ! This can also go horribly wrong if for some reason the os allows extension folder names. :D
    ! The only reason this exists in this test is to ensure that we aren't removing random stuff before
    ! the target substring.
    unit_3 = "/home/user/Desktop/my_data.obj"

    call unit_3%cut_all("my_data.obj")

    call assert_str_equal(unit_3%get(), "/home/user/Desktop/")
  end subroutine cut_all_test


  subroutine contains_test()
    implicit none

    type(heap_string) :: unit_1
    type(heap_string) :: unit_2


    unit_1 = "sarcastic"

    call assert_true(unit_1%contains("cast"))

    call assert_false(unit_1%contains("automobile"))


    unit_2 = "https://www.google.com"

    call assert_true(unit_2%contains("https://www."))

    call assert_true(unit_2%contains(".com"))

    call assert_false(unit_2%contains("http://www."))
  end subroutine contains_test


end module test_suite_string


program test_heap_string
  use test_suite_string
  implicit none

  call assign_test()

  call equality_test()

  call is_allocated_test()

  call get_test()

  call append_test()

  call prepend_test()

  call strip_test()

  call cut_test()

  call cut_last_test()

  call cut_all_test()

  call contains_test()
end program test_heap_string
