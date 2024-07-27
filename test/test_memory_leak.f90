module test_suite_test_memory_leak
  use :: vector_3f
  implicit none


contains

  subroutine test_leak()
    use, intrinsic :: iso_c_binding, only: c_long
    use :: string, only: int_to_string
    implicit none

    type(vec3f) :: unit_1
    integer(c_long) :: i
    integer :: gigabyte_count

    !? I was using this to see if direct pointer access would leak. It doesn't! :D
    ! if (.true.) then
    !   return
    ! end if

    gigabyte_count = 0

    print"(A)", "[BEGIN]: Testing memory leak."

    do i = 1, 9223372036854775806_8
      unit_1%x() = unit_1%x() + 1
      unit_1%y() = unit_1%y() + 1
      unit_1%z() = unit_1%z() + 1

      print*,unit_1%x()

      !* This will notate how much memory would be leaking.
      if (modulo(i * 4 * 3, 1073741824) == 0) then
        gigabyte_count = gigabyte_count + 1
        print"(A)", "Gigabytes: "//int_to_string(gigabyte_count)
      end if
    end do

    print"(A)", "[END]: Testing memory leak."

  end subroutine test_leak

end module test_suite_test_memory_leak


program test_memory_leak
  use :: test_suite_test_memory_leak
  implicit none

  call test_leak()

end program test_memory_leak
