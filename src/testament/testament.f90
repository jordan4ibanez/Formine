!** Testament is a simple testing library I slapped together.
!? Who's going to write the unit test for the unit test library?
module testament
  implicit none

  ! Whole thing is public, purely functional library.

contains


  subroutine assert_str_equal(a,b, error_message)
    implicit none

    character(len = *), intent(in) :: a
    character(len = *), intent(in) :: b
    character(len = *), intent(in), optional :: error_message

    if (a /= b) then
      if (present(error_message)) then
        error stop "[Testament] Expected: "//a//" | Received: "//b//" | "//error_message
      else
        error stop "[Testament] Expected: "//a//" | received: "//b
      end if
    end if
  end subroutine assert_str_equal


  subroutine assert_str_not_equal(a,b, error_message)
    implicit none

    character(len = *), intent(in) :: a
    character(len = *), intent(in) :: b
    character(len = *), intent(in), optional :: error_message

    if (a == b) then
      if (present(error_message)) then
        ! Wow, I would have never guessed.
        error stop "[Testament] Expected <NOT>: "//a//" | Received: "//b//" | "//error_message
      else
        error stop "[Testament] Expected <NOT>: "//a//" | received: "//b
      end if
    end if
  end subroutine assert_str_not_equal


end module testament
