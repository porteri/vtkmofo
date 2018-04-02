# Unit tests
include_directories(${CMAKE_BINARY_DIR}/mod)

# Add object library to work around CMake parallel build w/ module quirk
add_library(PassFail OBJECT Analyze.f90)

# attributes
add_executable(attributes_unit attributes_unit.f90 $<TARGET_OBJECTS:PassFail>)
target_link_libraries(attributes_unit vtkmofo)

# cells
add_executable(cells_unit cells_unit.f90 $<TARGET_OBJECTS:PassFail>)
target_link_libraries(cells_unit vtkmofo)

# datasets
add_executable(datasets_unit datasets_unit.f90 $<TARGET_OBJECTS:PassFail>)
target_link_libraries(datasets_unit vtkmofo)
