module thread_handler
  use :: thread
  implicit none

  !* This is simply an easy way to put all the threading handling in one spot.
  !* Nothing but main should be using this as an import.

contains


  subroutine thread_handler_run()
    implicit none

    call thread_process_detached_thread_queue()


  end subroutine thread_handler_run


end module thread_handler
