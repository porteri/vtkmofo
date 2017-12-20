# Integration tests
include_directories(${CMAKE_BINARY_DIR}/mod)

# Cube geometry
add_executable(cube Cube.f90)
target_link_libraries(cube vtkmofo)

# Cylindrical geometry
add_executable(cylinder Cylinder.f90)
target_link_libraries(cylinder vtkmofo)
