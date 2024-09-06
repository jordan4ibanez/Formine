module version_info
  use :: raw_data_version_info
  use, intrinsic :: iso_c_binding
  implicit none

  public :: FORMINE_IS_RELEASE
  public :: FORMINE_VERSION_MAJOR
  public :: FORMINE_VERSION_MINOR
  public :: FORMINE_VERSION_PATCH

end module version_info
