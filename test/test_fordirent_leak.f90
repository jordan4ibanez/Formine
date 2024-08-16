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

  call test_memory_leak()

end program test_forderint
