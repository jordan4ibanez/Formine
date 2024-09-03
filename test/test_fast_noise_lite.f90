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
        test_data = fnl_get_noise_3d(noise_state, real(x, c_float), real(y, c_float), 0.0)
        ! print"(f0.5)", test_data
        if (test_data < -1.00001 .or. test_data > 1.00001) then
          ! print"(f0.5)",test_data
          error stop "walked out of bounds"
        end if
      end do
    end do
    print*,"END NOISE TEST"

  end subroutine test_perlin


  subroutine fast_noise_pr_debug()
    implicit none

    type(fnl_state) :: noise_state
    real(c_float), dimension(128,128) :: noise_data
    integer(c_int) :: x, y

    noise_state = fnl_state()

    do y = 1,128
      do x = 1,128
        noise_data(x,y) = fnl_get_noise_2d(noise_state, real(x, c_float), real(y, c_float))
      end do
    end do
  end subroutine fast_noise_pr_debug
end module test_fast_noise_suite

program test_fast_noise
  use :: fast_noise_lite
  use, intrinsic :: iso_c_binding
  implicit none

  type(fnl_state) :: noise_state
  real(c_float), dimension(128,128) :: noise_data
  integer(c_int) :: x, y

  noise_state = fnl_state()
  noise_state%noise_type = FNL_NOISE_OPENSIMPLEX2S

  do y = 1,128
    do x = 1,128
      noise_data(x,y) = fnl_get_noise_3d(noise_state, real(x, c_float), real(y, c_float), 0.0)
    end do
  end do

  ! print*,noise_data
end program test_fast_noise
