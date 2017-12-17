# Unit tests
include_directories(${CMAKE_BINARY_DIR}/mod)

# attributes
add_executable(attributes_unit attributes_unit.f90 Analyze.f90)
target_link_libraries(attributes_unit vtkmofo)

# cells
add_executable(cells_unit cells_unit.f90 Analyze.f90)
target_link_libraries(cells_unit vtkmofo)

