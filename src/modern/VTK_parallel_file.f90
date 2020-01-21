module vtk_parallel_file
  use vtk_serial_file, only : vtk_file_dt
  implicit none
  type(vtk_file_dt), allocatable :: parallel_file  !! parallel vtk file
end module
