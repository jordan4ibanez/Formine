module thread_handler
  use :: thread
  use :: chunk_generator
  use :: chunk_mesh
  implicit none

  !* This is simply an easy way to put all the threading handling in one spot.
  !* Nothing but main should be using this as an import.

contains

  !* This is called at the beginning of the program to initialize needed components.
  subroutine thread_handler_intialization()
    implicit none

    !? 8 additional threads.
    !! TODO: THIS NEEDS TO BE A SETTINGS MENU OPTION!
    call thread_initialize(8)

    call chunk_generator_initialize()

    call chunk_mesh_initialize()
  end subroutine thread_handler_intialization


  !* This is called in the main loop of the program to handle thread things.
  subroutine thread_handler_run()
    implicit none

    call thread_process_thread_queue()

    call chunk_generator_process_output_queue()

    call chunk_mesh_process_output_queue()
  end subroutine thread_handler_run


end module thread_handler
