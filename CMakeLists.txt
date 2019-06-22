# CMAKE minimum version
cmake_minimum_required(VERSION 3.12.2)

option(VTKmofo_USE_OpenCoarrays
  "Build VTKmofo with support for linking against OpenCoarray programs" OFF)

# Project name
project(vtkmofo LANGUAGES Fortran)

# vtkmofo project version
set (VTKMOFO_VERSION_MAJOR 1)
set (VTKMOFO_VERSION_MINOR 001)

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

# System options
if ( "${CMAKE_SYSTEM_NAME}" MATCHES "Windows")
  set(prefix "/")
  set(infix ":")
  set(Qf "Q")
  set(Q "Q")
  set(eq ":")
  set(colon ":")
  set(colon_ ":")
  set(libs_static "/libs:static")
  set(dgblibs "/dbglibs")
else()
  set(prefix "-")
  set( infix " ")
  set( Qf "f")
  set( Q "")
  set( eq "=")
  set( colon "")
  set( colon_ " ")
  set( libs_static "")
  set( dgblibs "")
endif()

# Compiler options
if ("${CMAKE_Fortran_COMPILER_ID}" MATCHES "Intel")
  set(intel_compiler true)
  if ( "${CMAKE_SYSTEM_NAME}" MATCHES "Windows")
    # Full debug, Fortran 2008 standard, free format (due to automatic build to .f file for testing), Check bounds
    set(CMAKE_Fortran_FLAGS
      "${prefix}nologo ${prefix}debug${infix}full ${prefix}MP ${prefix}Od ${prefix}standard-semantics ${prefix}warn${infix}errors ${prefix}stand${infix}f15 ${prefix}debug-parameters${infix}all ${prefix}warn${infix}declarations ${prefix}warn${infix}unused ${prefix}warn${infix}interfaces ${prefix}${Qf}trapuv ${prefix}${Q}init${eq}snan ${prefix}${Q}init${eq}arrays ${prefix}fpe${colon}0 ${prefix}traceback ${prefix}check${colon_}bounds ${prefix}check${colon_}stack ${libs_static} ${prefix}threads ${dbglibs} ${prefix}free"
       )
  else()
    # Full debug, Fortran 2008 standard, free format (due to automatic build to .f file for testing), Check bounds
    set(CMAKE_Fortran_FLAGS
      "${prefix}nologo ${prefix}debug${infix}full ${prefix}multiple-processes ${prefix}O0 ${prefix}standard-semantics ${prefix}warn${infix}errors ${prefix}stand${infix}f15 ${prefix}debug-parameters${infix}all ${prefix}warn${infix}declarations ${prefix}warn${infix}unused ${prefix}warn${infix}interfaces ${prefix}${Qf}trapuv ${prefix}${Q}init${eq}snan ${prefix}${Q}init${eq}arrays ${prefix}fpe${colon}0 ${prefix}traceback ${prefix}check${colon_}bounds ${prefix}check${colon_}stack ${libs_static} ${prefix}threads ${dbglibs} ${prefix}free"
	   )
  endif()
elseif("${CMAKE_Fortran_COMPILER_ID}" MATCHES "GNU")
  set(gnu_compiler true)
  # Fortran 2008 standard, free format (due to automatic build to .f file for testing)
  set(CMAKE_Fortran_FLAGS "${prefix}std=f2008 ${prefix}fcheck=all ${prefix}Wall ${prefix}Wno-unused-dummy-argument ${prefix}ffree-form")
else()
  message(WARNING
    "\n"
    "Attempting to build with untested Fortran compiler: ${CMAKE_Fortran_COMPILER_ID}. "
    "Please report any failures through the vtkmofo Git issues\n\n"
  )
endif()

if(VTKmofo_USE_OpenCoarrays)
  find_package(OpenCoarrays)
endif()

##################################################
# Begin VTKmofo specific targets and configuration
##################################################

# Where to put VTKmofo module files
set(VTKmofo_mod_dir ${CMAKE_CURRENT_BINARY_DIR}/mod)

# Avoid compilation cascades:
#    put interfaces and submodules in separate libraries
# Specify VTKmofo interfaces
set(VTKmofo_interfaces
  Precision.f90
  Misc_interface.f90
  VTK_io_interface.f90
  VTK_datasets_interface.f90
  VTK_cells_interface.f90
  VTK_attributes_interface.f90
  VTK_vars.f90)

# Specify VTKmofo implementations
set(VTKmofo_implementations
  Misc_implementation.f90
  VTK_attributes_implementation.f90
  VTK_cells_implementation.f90
  VTK_datasets_implementation.f90
  VTK_interface.f90
  VTK_io_implementation.f90
  )

# Compile all the interfaces first
foreach(item ${VTKmofo_interfaces})
  list(APPEND VTKmofo_interface_srcs "${CMAKE_CURRENT_SOURCE_DIR}/src/${item}")
endforeach()
add_library(vtkmofo_interfaces OBJECT ${VTKmofo_interface_srcs})
# Tell CMake where to put vtkmofo .mod files generated with libvtkmofo
set_property(TARGET vtkmofo_interfaces
  PROPERTY
  Fortran_MODULE_DIRECTORY ${VTKmofo_mod_dir})
if(VTKmofo_USE_OpenCoarrays)
  target_link_libraries(vtkmofo_interfaces
    PRIVATE OpenCoarrays::caf_mpi_static)
endif()

# Add any object files from the interfaces to the main library build
foreach(item ${VTKmofo_implementations})
  list(APPEND VTKmofo_implementation_srcs "${CMAKE_CURRENT_SOURCE_DIR}/src/${item}")
endforeach()
add_library(vtkmofo
  STATIC ${VTKmofo_implementation_srcs} $<TARGET_OBJECTS:vtkmofo_interfaces>)
if(VTKmofo_USE_OpenCoarrays)
  target_link_libraries(vtkmofo
    PRIVATE OpenCoarrays::caf_mpi_static)
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
  FOLDER "VTKmofo")
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
  set_property(TEST "VTKmofo_${unit_test}_test" PROPERTY PASS_REGULAR_EXPRESSION "Test passed")
  set_property(TEST "VTKmofo_${unit_test}_test" PROPERTY LABELS "VTKmofo" "unit-test")
endforeach()

# Add integration tests and define the string that is used to signal success
foreach(integration_test ${VTKmofo_integration_test_list})
  add_test(NAME "VTKmofo_${integration_test}_test" COMMAND ${CMAKE_CURRENT_BINARY_DIR}/tests/integration/${integration_test} WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/tests/integration)
  set_property(TEST "VTKmofo_${integration_test}_test" PROPERTY PASS_REGULAR_EXPRESSION "Finished")
  set_property(TEST "VTKmofo_${integration_test}_test" PROPERTY LABELS "VTKmofo" "integration-test")
endforeach()

add_custom_target(RUN_ALL_VTKmofo_TESTS
  COMMAND ${CMAKE_CTEST_COMMAND} -C $<CONFIG> --output-on-failure -L "VTKmofo")
set_property(TARGET RUN_ALL_VTKmofo_TESTS PROPERTY FOLDER "All-Tests")
