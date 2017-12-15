include_directories(${CMAKE_BINARY_DIR}/mod)

# Integration tests
add_executable(Driver Driver.f90)
target_link_libraries(Driver vtkmofo)
