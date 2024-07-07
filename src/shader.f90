module shader
  implicit none

  private

contains

  function create_shader(shader_name, vertex_code_location, fragment_code_location) result(success)
    implicit none

    character(len = *) :: shader_name
    character(len = *) :: vertex_code_location
    character(len = *) :: fragment_code_location
    logical :: success

    


  end function create_shader

end module shader
