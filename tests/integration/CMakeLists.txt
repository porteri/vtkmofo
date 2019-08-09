function(add_VTKmofo_integration_test test_name folder)
  # Assumes source file is ${test_name}.f90
  add_executable(${folder}_${test_name} ${folder}/${test_name}.f90)

  if(VTKmofo_USE_OpenCoarrays)
    target_link_libraries(${folder}_${test_name}
      PUBLIC OpenCoarrays::caf_mpi_static)
  endif()
  target_link_libraries(${folder}_${test_name}
    PUBLIC vtkmofo)
  set_property(TARGET ${folder}_${test_name}
    PROPERTY
    FOLDER "VTKmofo-Integration-Tests")
  source_group("VTKmofo\\IntegrationTests" FILES ${test_name}.f90)
endfunction(add_VTKmofo_integration_test)

# legacy tests
foreach(test
    Append
    DTIO_T_shape
    Polygonal_data
    Rectilinear_grid
    Structured_grid
    Structured_points
    Unstructured_grid)
  add_VTKmofo_integration_test(${test} "legacy")
  list(APPEND VTKmofo_integration_test_list "legacy_${test}")
endforeach()

# serial modern tests
foreach(test
    Image_Data
    Rectilinear_grid
    Structured_grid
    Unstructured_grid)
  add_VTKmofo_integration_test(${test} "serial")
  list(APPEND VTKmofo_integration_test_list "serial_${test}")
endforeach()

# Export integration test list so that we don't have to keep track if it twice
set(VTKmofo_integration_test_list "${VTKmofo_integration_test_list}" PARENT_SCOPE)
