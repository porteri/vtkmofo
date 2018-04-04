# Unit tests
include_directories(${CMAKE_BINARY_DIR}/mod)

# Add object library to work around CMake parallel build w/ module quirk
add_library(VTKmofoPassFail OBJECT VTKmofoAnalyze.f90)

function(add_VTKmofo_test test_name)
  # Assumes source file is ${test_name}.f90
  add_executable(${test_name} ${test_name}.f90 $<TARGET_OBJECTS:VTKmofoPassFail>)
  target_link_libraries(${test_name} vtkmofo)
  set_property(TARGET ${test_name}
    PROPERTY
    FOLDER "VTKmofo-Unit-Tests")
  source_group("VTKmofo\\UnitTests" FILES ${test_name}.f90)
endfunction(add_VTKmofo_test)

# attributes
add_VTKmofo_test(attributes_unit)

# cells
add_VTKmofo_test(cells_unit)

# datasets
add_VTKmofo_test(datasets_unit)
