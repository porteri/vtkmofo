# CMAKE minimum version
cmake_minimum_required(VERSION 3.14.0)

set(CMAKE_BUILD_TYPE_INIT Debug) # Default to a debug build type

option(VTKmofo_USE_OpenCoarrays
  "Build vtkmofo with support for linking against OpenCoarray programs" OFF)

# vtkmofo project version
set(VTKMOFO_VERSION_MAJOR 1)
set(VTKMOFO_VERSION_MINOR 200)

# Project name
project(vtkmofo VERSION "${VTKMOFO_VERSION_MAJOR}.${VTKMOFO_VERSION_MINOR}" LANGUAGES Fortran)

# Set the type/configuration of build to perform
set(ALLOWED_BUILD_TYPES "Debug" "Release")
set(CMAKE_BUILD_TYPE "Debug"
  CACHE STRING "Select which configuration to build.")
set_property (CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS ${ALLOWED_BUILD_TYPES})

# Custom modules
list(APPEND CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake/modules")
include(Platform_Compiler_Options) # Performs abstraction for OS and compiler options
include(AddLibFunction)

# Print project logo, version and tag line
file(READ ${CMAKE_CURRENT_LIST_DIR}/cmake/VTKMOFO-logo.txt VTKMOFO_LOGO)
string(JOIN "\n         " VTKMOFO_GREETING "${VTKMOFO_LOGO}"
  "VTKMOFO, v${VTKMOFO_VERSION_MAJOR}.${VTKMOFO_VERSION_MINOR}: VTK in MOdern FOrtran")
if(NOT isMultiConfig)
  string(APPEND VTKMOFO_GREETING "\n         Build type: ${CMAKE_BUILD_TYPE}")
endif()
string(APPEND VTKMOFO_GREETING "\n")
string(PREPEND VTKMOFO_GREETING "\n")
message(STATUS "${VTKMOFO_GREETING}")

# Turn on the ability to create folders to organize projects (.vcproj)
# It creates "CMakePredefinedTargets" folder by default and adds CMake
# defined projects like INSTALL.vcproj and ZERO_CHECK.vcproj
set_property(GLOBAL PROPERTY USE_FOLDERS ON)

if(VTKmofo_USE_OpenCoarrays)
  find_package(OpenCoarrays)
endif()

##################################################
# Begin vtkmofo specific targets and configuration
##################################################

# Where to put vtkmofo module files
set(VTKmofo_mod_dir ${CMAKE_CURRENT_BINARY_DIR}/mod)

# Utilities
set(vtkmofo_utilities
  utilities/File_utility.f90
  utilities/File_utility_procedures.f90
#  utilities/Linked_list.f90
#  utilities/Linked_list_procedures.f90
  utilities/Misc.f90
  utilities/Misc_procedures.f90
  utilities/Precision.f90
  utilities/XML.f90
  utilities/XML_procedures.F90
  )

# Legacy vtk files
set(vtkmofo_legacy
  legacy/VTK_io.f90
  legacy/VTK_datasets.f90
  legacy/VTK_cells.f90
  legacy/VTK_attributes.f90
  legacy/VTK_vars.f90
  legacy/VTK_attributes_procedures.f90
  legacy/VTK_cells_procedures.f90
  legacy/VTK_datasets_procedures.f90
  legacy/VTK.f90
  legacy/VTK_io_procedures.f90
  )

# Modern vtk serial format files
set(vtkmofo_modern
  modern/VTK_DataArray_procedures.f90
  modern/VTK_element_procedures.f90
  modern/VTK_piece_element_procedures.F90
  modern/VTK_XML_file_procedures.f90
  modern/VTK_XML_Grid_procedures.f90
  modern/VTK_DataArray.f90
  modern/VTK_element.f90
  modern/VTK_formats_types.f90
  modern/VTK_piece_element.f90
  modern/VTK_XML_file.f90
  modern/VTK_XML_Grid.f90
  )

# Compile the utilities library first
foreach(item ${vtkmofo_utilities})
  list(APPEND vtkmofo_utilities_srcs "${CMAKE_CURRENT_SOURCE_DIR}/src/${item}")
endforeach()
add_library(vtkmofo_utilities OBJECT ${vtkmofo_utilities_srcs})
# Tell CMake where to put vtkmofo .mod files generated with libvtkmofo
set_property(TARGET vtkmofo_utilities
  PROPERTY Fortran_MODULE_DIRECTORY ${VTKmofo_mod_dir})
# Visual Studio Organization
set_property(TARGET vtkmofo_utilities
  PROPERTY FOLDER "source")
set_target_properties(vtkmofo_utilities
  PROPERTIES PROJECT_LABEL "utilities")
# Link against OpenCoarrays (if needed)
if(VTKmofo_USE_OpenCoarrays)
  target_link_libraries(vtkmofo_utilities
    PRIVATE OpenCoarrays::caf_mpi_static)
endif()

# Add any object files from the interfaces to the main library build
foreach(item ${vtkmofo_legacy})
  list(APPEND vtkmofo_srcs "${CMAKE_CURRENT_SOURCE_DIR}/src/${item}")
endforeach()
foreach(item ${vtkmofo_modern})
  list(APPEND vtkmofo_srcs "${CMAKE_CURRENT_SOURCE_DIR}/src/${item}")
endforeach()

add_library(vtkmofo
  STATIC ${vtkmofo_srcs} $<TARGET_OBJECTS:vtkmofo_utilities>)
# Link against OpenCoarrays (if needed)
if(VTKmofo_USE_OpenCoarrays)
  target_link_libraries(vtkmofo
    PRIVATE OpenCoarrays::caf_mpi_static)
endif()

# Work around for GFortran bug
if("${CMAKE_Fortran_COMPILER_ID}" MATCHES "GNU")
  set_property( SOURCE
    src/utilities/XML_procedures.f90
    src/modern/VTK_piece_element_procedures.f90
    src/legacy/VTK_cells_procedures.f90
    src/legacy/VTK_datasets_procedures.f90
    APPEND
    PROPERTY
    COMPILE_OPTIONS "-O0"
    )
endif()

# Tell CMake where to put vtkmofo .mod files generated with libvtkmofo
set_property(TARGET vtkmofo
  PROPERTY
  Fortran_MODULE_DIRECTORY ${VTKmofo_mod_dir})

# Tell consumers where to find .mod files
target_include_directories(vtkmofo PUBLIC
  $<BUILD_INTERFACE:${VTKmofo_mod_dir}>
  $<INSTALL_INTERFACE:include/vtkmofo>
  )

# Organize things in Visual Studio
source_group("VTKmofoLib" FILES ${VTKmofo_sources})
set_property(TARGET vtkmofo
  PROPERTY
  FOLDER "source")
install(TARGETS vtkmofo DESTINATION lib EXPORT vtkmofo-targets)
# Breaking this code out into a function would be nice
set(MOD_DIR_TO_INSTALL "${VTKmofo_mod_dir}")
set(MOD_DESTINATION_DIR include/vtkmofo)
install(
  CODE "file(GLOB_RECURSE MODULE_FILES \"${MOD_DIR_TO_INSTALL}/*.mod\")"
  CODE "file(GLOB_RECURSE SUBMOD_FILES \"${MOD_DIR_TO_INSTALL}/*.smod\")"
  CODE "file(INSTALL \${MODULE_FILES} DESTINATION \"\${CMAKE_INSTALL_PREFIX}/${MOD_DESTINATION_DIR}\")"
  CODE "file(INSTALL \${SUBMOD_FILES} DESTINATION \"\${CMAKE_INSTALL_PREFIX}/${MOD_DESTINATION_DIR}\")"
  )
install(EXPORT vtkmofo-targets DESTINATION lib/vtkmofo)

######################
# Add test directories
######################

enable_testing()

# Specify all the subdirectories containing test executable/library sources:
set(subdirectories_list tests/unit tests/integration)
foreach(subdirectory ${subdirectories_list})
  add_subdirectory("${CMAKE_CURRENT_SOURCE_DIR}/${subdirectory}")
endforeach()

# Unit Tests exported from tests/unit subdirectory in ${VTKmofo_unit_test_list}
# Integration Tests exported from tests/integration in ${VTKmofo_integration_test_list}

# Add unit tests and define the string that is used to signal success
foreach(unit_test ${VTKmofo_unit_test_list})
  add_test(NAME "VTKmofo_${unit_test}_test" COMMAND ${CMAKE_CURRENT_BINARY_DIR}/tests/unit/${unit_test} WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/tests/unit)
  set_property(TEST "VTKmofo_${unit_test}_test" PROPERTY PASS_REGULAR_EXPRESSION "Test Passed")
  set_property(TEST "VTKmofo_${unit_test}_test" PROPERTY LABELS "vtkmofo" "unit-test")
endforeach()

# Add integration tests and define the string that is used to signal success
foreach(integration_test ${VTKmofo_integration_test_list})
  add_test(NAME "VTKmofo_${integration_test}_test" COMMAND ${CMAKE_CURRENT_BINARY_DIR}/tests/integration/${integration_test} WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/tests/integration)
  set_property(TEST "VTKmofo_${integration_test}_test" PROPERTY PASS_REGULAR_EXPRESSION "Finished")
  set_property(TEST "VTKmofo_${integration_test}_test" PROPERTY LABELS "vtkmofo" "integration-test")
  set_property(TEST "VTKmofo_${integration_test}_test" PROPERTY FOLDER "VTKMOFO_Integration_Tests")
endforeach()

add_custom_target(RUN_ALL_VTKmofo_TESTS
  COMMAND ${CMAKE_CTEST_COMMAND} -C $<CONFIG> --output-on-failure -L "vtkmofo")
set_property(TARGET RUN_ALL_VTKmofo_TESTS PROPERTY FOLDER "All-Tests")
