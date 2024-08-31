module test_fast_noise_suite
  use :: fast_noise_lite
  use :: iso_c_binding
  implicit none


contains


  subroutine test_perlin()
    use :: string
    implicit none

    type(fnl_state) :: noise_state
    real(c_float) :: test_data
    integer(c_int) :: x, y

    noise_state = fnl_state()
    noise_state%noise_type = FNL_NOISE_PERLIN

    print*,"BEGIN NOISE TEST"
    do x = -10000, 10000
      do y = -10000, 10000
        test_data = fnl_get_noise_2d(noise_state, real(x, c_float), real(y, c_float))
        print"(f0.5)", test_data
        if (test_data < -1.00001 .or. test_data > 1.00001) then
          ! print"(f0.5)",test_data
          error stop "walked out of bounds"
        end if
      end do
    end do
    print*,"END NOISE TEST"

  end subroutine test_perlin


end module test_fast_noise_suite

program test_fast_noise
  use :: test_fast_noise_suite
  implicit none

  call test_perlin()

end program test_fast_noise
