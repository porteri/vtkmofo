include_directories(${CMAKE_BINARY_DIR}/mod)

# Unit tests
file (COPY attributes_scalar.vtk  DESTINATION "${CMAKE_CURRENT_BINARY_DIR}")
file (COPY attributes_vector.vtk  DESTINATION "${CMAKE_CURRENT_BINARY_DIR}")
file (COPY attributes_normal.vtk  DESTINATION "${CMAKE_CURRENT_BINARY_DIR}")
file (COPY attributes_texture.vtk DESTINATION "${CMAKE_CURRENT_BINARY_DIR}")
file (COPY attributes_tensor.vtk  DESTINATION "${CMAKE_CURRENT_BINARY_DIR}")
#file (COPY attributes_field.vtk   DESTINATION "${CMAKE_CURRENT_BINARY_DIR}")
add_executable(attributes_scalar_unit attributes_scalar_unit.f90 Analyze.f90)
target_link_libraries(attributes_scalar_unit vtkmofo)
