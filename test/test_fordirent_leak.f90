module test_suite_forderint
  use :: directory
  implicit none

contains

  subroutine test_memory_leak()
    implicit none

    type(directory_reader) :: reader
    ! integer(c_int) :: i

    ! i = 1
    print*, "begin testing forderint memory leak."

    do while (.true.)
      ! print*,"loop: ", i

      call reader%destroy()

      call reader%read_directory("./mods/formine/")

      ! i = i + 1

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
