# Unit tests
# Add object library to work around CMake parallel build race conditions
# (Sources defining modules should *NOT* be listed in multiple targets!
# Use an object lib and link against that instead.)
add_library(VTKmofoPassFail OBJECT VTKmofoAnalyze.f90)
target_include_directories(VTKmofoPassFail PUBLIC "${VTKmofo_mod_dir}")
set_property(TARGET VTKmofoPassFail
  PROPERTY
  FOLDER "VTKmofo-Unit-Tests")

function(add_VTKmofo_unit_test test_name)
  # Assumes source file is ${test_name}.f90
  add_executable(${test_name} ${test_name}.f90 $<TARGET_OBJECTS:VTKmofoPassFail>)
  target_link_libraries(${test_name} vtkmofo)
  set_property(TARGET ${test_name}
    PROPERTY
    FOLDER "VTKmofo-Unit-Tests")
  source_group("VTKmofo\\UnitTests" FILES ${test_name}.f90 VTKmofoAnalyze.f90)
endfunction(add_VTKmofo_unit_test)

foreach(test
    attributes_unit
    cells_unit
    datasets_unit)
  add_VTKmofo_unit_test(${test})
  list(APPEND VTKmofo_unit_test_list ${test})
endforeach()

# Set the VTKmofo_unit_test_list in parent scope
set(VTKmofo_unit_test_list ${VTKmofo_unit_test_list} PARENT_SCOPE)
