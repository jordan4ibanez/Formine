!* Delta time module for doing engine things that involve time.
module delta_time
  use, intrinsic :: iso_c_binding, only: c_int64_t, c_double, c_float, c_int
  implicit none

  private

  public :: delta_initialize
  public :: delta_tick
  public :: delta_get_f32
  public :: delta_get_f64
  public :: get_fps

  integer(c_int64_t) :: old_delta_integral = 0
  real(c_double) :: delta = 0.0d0

  real(c_double) :: fps_delta_accumulator = 0.0d0
  integer(c_int) :: fps_accumulator = 0
  integer(c_int) :: current_fps = 0

contains


  !* This sets up the delta calculator so it doesn't blow up.
  subroutine delta_initialize()
    implicit none

    integer(c_int64_t) :: count

    call system_clock(count)

    old_delta_integral = count
  end subroutine delta_initialize


  subroutine delta_tick()
    implicit none

    ! This only works on Windows, Linux, and POSIX platforms.
    ! These platforms seem to implement the monotonic clock.
    ! If for whatever reason you're porting this, you might want to set this to:
    ! integer(c_float)
    ! But doing this will make it so after 24 days, it WILL wrap around!
    ! You will also run into extreme precision problems on high refresh rate devices.
    integer(c_int64_t) :: count, count_rate, count_max, new_delta_integral

    call system_clock(count, count_rate, count_max)

    ! Move the calculation into milliseconds
    new_delta_integral = count

    ! Now set it.
    delta = real(new_delta_integral - old_delta_integral, kind = c_double)

    delta = delta / real(count_rate, kind = c_double)

    ! Finally, save it.
    old_delta_integral = new_delta_integral

    ! Bolt on FPS calculation.
    call fps_calculation()
  end subroutine delta_tick


  subroutine fps_calculation()
    use :: string_f90, only: int_to_string
    implicit none

    fps_delta_accumulator = fps_delta_accumulator + delta
    fps_accumulator = fps_accumulator + 1

    if (fps_delta_accumulator >= 1.0d0) then
      current_fps = fps_accumulator
      print*,"fps: "//int_to_string(current_fps)
      fps_accumulator = 0
      fps_delta_accumulator = fps_delta_accumulator - 1.0d0
    end if
  end subroutine fps_calculation


  real(c_float) function delta_get_f32() result(current_delta)
    implicit none

    ! Basically a precision chop.
    current_delta = real(delta, kind = c_float)
  end function delta_get_f32


  real(c_double) function delta_get_f64() result(current_delta)
    implicit none

    current_delta = delta
  end function delta_get_f64


  integer(c_int) function get_fps() result(fps)
    implicit none

    fps = current_fps
  end function get_fps


end module delta_time
