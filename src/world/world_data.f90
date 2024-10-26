module world_data
  use, intrinsic :: iso_c_binding
  implicit none


  private


  integer(c_int) :: world_seed


contains


  subroutine world_data_set_world_seed(new_seed)
    implicit none

    integer(c_int), intent(in), value :: new_seed

    world_seed = new_seed
  end subroutine world_data_set_world_seed


  function world_data_get_world_seed() result(seed)
    implicit none

    integer(c_int) :: seed

    seed = world_seed
  end function world_data_get_world_seed


end module world_data
