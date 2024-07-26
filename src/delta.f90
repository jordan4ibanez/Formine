!* Delta time module for doing engine things that involve time.
module delta
  use, intrinsic :: iso_c_binding, only: c_long, c_double, c_float
  implicit none

  private

  public :: delta_tick
  public :: get_delta_f64
  public :: get_delta_f32

  integer(c_long) :: old_delta_integral = 0
  real(c_double) :: delta_time = 0.0d0

contains

  subroutine delta_tick()
    implicit none

    ! This only works on Windows, Linux, and POSIX platforms.
    ! If for whatever reason you're porting this, you might want to set this to:
    ! integer(c_float)
    ! But doing this will make it so after 24 days, it WILL wrap around!
    ! You will also run into extreme precision problems on high refresh rate devices.
    integer(c_long) :: count, count_rate, count_max, new_delta_integral

    call system_clock(count, count_rate, count_max)

    ! Move the calculation into milliseconds
    new_delta_integral = count

    ! Now set it.
    delta_time = real(new_delta_integral - old_delta_integral, kind = c_double)

    delta_time = delta_time / real(count_rate, kind = c_double)

    print"(f00.10)",delta_time
    ! Finally, save it.
    old_delta_integral = new_delta_integral
  end subroutine delta_tick


  real(c_double) function get_delta_f64() result(current_delta)
    implicit none

    current_delta = delta_time
  end function get_delta_f64


  real(c_float) function get_delta_f32() result(current_delta)
    implicit none

    ! Basically a precision chop.
    current_delta = real(delta_time, kind = c_float)
  end function get_delta_f32

end module delta
