include_directories(${CMAKE_BINARY_DIR}/mod)

# Unit tests
add_executable(attributes_scalar attributes_scalar_unit.f90 Analyze.f90)
target_link_libraries(attributes_scalar vtkmofo)
