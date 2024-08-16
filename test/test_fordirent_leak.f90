module test_suite_forderint
  use :: directory
  implicit none

contains

  subroutine test_memory_leak()
    implicit none

    type(directory_reader) :: reader

    print*, "begin testing forderint memory leak."

    do while (.true.)

      call reader%deallocate_memory()

      call reader%read_directory("./")

    end do
  end subroutine test_memory_leak



end module test_suite_forderint

program test_forderint
  use test_suite_forderint
  implicit none

  ! This is nice and steady at the time of writing this. (use git blame)
  ! Held at 131.1 kb :D
  ! call test_memory_leak()

end program test_forderint
