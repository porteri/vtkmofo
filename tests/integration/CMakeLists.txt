# Integration tests
include_directories(${CMAKE_BINARY_DIR}/mod)

# Cube geometry
add_executable(cube Cube.f90)
target_link_libraries(cube vtkmofo)

# Cylindrical geometry
add_executable(cylinder Cylinder.f90)
target_link_libraries(cylinder vtkmofo)

# Rectangular prism geometry
add_executable(rectangular_prism Rectangular_prism.f90)
target_link_libraries(rectangular_prism vtkmofo)

# Pyramid geometry
add_executable(pyramid Pyramid.f90)
target_link_libraries(pyramid vtkmofo)

# T_shape geometry
add_executable(t_shape T_shape.f90)
target_link_libraries(t_shape vtkmofo)