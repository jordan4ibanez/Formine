module factory
  implicit none
  private

  public :: mob
  public :: new_mob

  public :: yep

  integer, save :: yep

  type mob
    integer :: id = 0
    integer :: hp = 0
  contains
    procedure :: yell
  end type mob

contains

  ! Construct a mobly mob. Count how many we have.
  function new_mob() result(a_mob)
    use, intrinsic :: iso_fortran_env
    use :: string
    implicit none

    type(mob) :: a_mob
    integer, save :: total_mobs = 0
    real :: randomness = 0.0

    ! Seed the random generator.
    if (total_mobs == 0) then
      call random_seed()
    end if

    call random_number(randomness)

    ! 0 - 100.0
    randomness = randomness * 100.0

    ! Mob has random hp
    a_mob%hp = floor(randomness)
    a_mob%id = total_mobs

    ! Tick up mob count.
    total_mobs = total_mobs + 1
  end

  subroutine yell(this)
    use :: string
    implicit none

    class(mob) :: this

    print"(A)","HELLO, I AM MOB ["//int_to_string(this%id)//"] WITH HP ["//int_to_string(this%hp)//"]"
  end subroutine yell

end module factory
