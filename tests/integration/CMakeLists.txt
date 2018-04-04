function(add_VTKmofo_integration_test test_name)
  # Assumes source file is ${test_name}.f90
  add_executable(${test_name} ${test_name}.f90)
  target_link_libraries(${test_name} vtkmofo)
  set_property(TARGET ${test_name}
    PROPERTY
    FOLDER "VTKmofo-Integration-Tests")
  source_group("VTKmofo\\IntegrationTests" FILES ${test_name}.f90)
endfunction(add_VTKmofo_integration_test)

foreach(test
    Cube
    Cylinder
    Rectangular_prism
    Pyramid
    T_shape)
  add_VTKmofo_integration_test(${test})
  list(APPEND VTKmofo_integration_test_list "${test}")
endforeach()

# Export integration test list so that we don't have to keep track if it twice
set(VTKmofo_integration_test_list "${VTKmofo_integration_test_list}" PARENT_SCOPE)
