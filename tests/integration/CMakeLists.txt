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
    FOLDER "tests/vtkmofo_integration_tests")
  source_group("VTKmofo\\IntegrationTests" FILES ${test_name}.f90)
endfunction(add_VTKmofo_integration_test)

# legacy tests
foreach(test
    Append
    Polygonal_data
    Rectilinear_grid
    Structured_grid
    Structured_points
    Unstructured_grid)
  add_VTKmofo_integration_test(${test} "legacy")
  list(APPEND VTKmofo_integration_test_list "legacy_${test}")
endforeach()

# Currently ignore the DTIO test b/c Intel 18.x does not properly support DTIO
if ("${CMAKE_Fortran_COMPILER_ID}" MATCHES "Intel")

else()
    add_VTKmofo_integration_test(DTIO_T_shape "legacy")
    list(APPEND VTKmofo_integration_test_list "legacy_DTIO_T_shape")
endif()

# serial modern tests
foreach(test
    Rectilinear_grid
    Structured_grid
    Structured_points
    Unstructured_grid)
  add_VTKmofo_integration_test(${test} "serial")
  list(APPEND VTKmofo_integration_test_list "serial_${test}")
endforeach()

# Currently ignore the DTIO test b/c Intel 18.x does not properly support DTIO
if ("${CMAKE_Fortran_COMPILER_ID}" MATCHES "Intel")

else()
    add_VTKmofo_integration_test(DTIO_T_shape "serial")
    list(APPEND VTKmofo_integration_test_list "serial_DTIO_T_shape")
endif()

# parallel modern tests
foreach(test
    Structured_grid
    Unstructured_grid)
  add_VTKmofo_integration_test(${test} "parallel")
  list(APPEND VTKmofo_integration_test_list "parallel_${test}")
endforeach()

# Export integration test list so that we don't have to keep track if it twice
set(VTKmofo_integration_test_list "${VTKmofo_integration_test_list}" PARENT_SCOPE)
