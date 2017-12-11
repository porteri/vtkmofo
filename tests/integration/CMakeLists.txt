include_directories(${CMAKE_BINARY_DIR}/mod)

# Integration tests
add_executable(Driver_unit Driver_unit.f90)
target_link_libraries(Driver_unit vtkmofo)
