# CMAKE minimum version
cmake_minimum_required(VERSION 3.2.2)

# Project name
project(vtkmofo LANGUAGES Fortran)

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
      "${prefix}nologo ${prefix}debug${infix}full ${prefix}MP ${prefix}Od ${prefix}standard-semantics ${prefix}warn${infix}errors ${prefix}stand${infix}f15 ${prefix}debug-parameters${infix}all ${prefix}warn${infix}declarations ${prefix}warn${infix}unused ${prefix}warn${infix}interfaces ${prefix}${Qf}trapuv ${prefix}${Q}init${eq}snan ${prefix}${Q}init${eq}arrays ${prefix}fpe${colon}0 ${prefix}traceback ${prefix}check${colon_}bounds ${prefix}check${colon_}stack ${libs_static} ${prefix}threads ${dbglibs} ${prefix}free ${prefix}assume${infix}nounderscore"
       )
  else()
    # Full debug, Fortran 2008 standard, free format (due to automatic build to .f file for testing), Check bounds
    set(CMAKE_Fortran_FLAGS
      "${prefix}nologo ${prefix}debug${infix}full ${prefix}multiple-processes ${prefix}O0 ${prefix}standard-semantics ${prefix}warn${infix}errors ${prefix}stand${infix}f15 ${prefix}debug-parameters${infix}all ${prefix}warn${infix}declarations ${prefix}warn${infix}unused ${prefix}warn${infix}interfaces ${prefix}${Qf}trapuv ${prefix}${Q}init${eq}snan ${prefix}${Q}init${eq}arrays ${prefix}fpe${colon}0 ${prefix}traceback ${prefix}check${colon_}bounds ${prefix}check${colon_}stack ${libs_static} ${prefix}threads ${dbglibs} ${prefix}free ${prefix}assume${infix}nounderscore"
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


# Check if using OpenCoarrays caf wrapper
if(CMAKE_Fortran_COMPILER MATCHES "caf$")
  set(USING_CAF_COMPILER_WRAPPER TRUE)
  set(DEFAULT_NUM_IMAGES 1)
endif()


##################################################
# Begin VTKmofo specific targets and configuration
##################################################

# Where to put VTKmofo module files
set(VTKmofo_mod_dir ${CMAKE_CURRENT_BINARY_DIR}/mod)

# Specify VTKmofo sources
set(VTKmofo_files
  "Precision.f90"
  "Misc.f90"
  "VTK_attributes.f90"
  "VTK_cells.f90"
  "VTK_datasets.f90"
  "VTK_interface.f90"
  "VTK_vars.f90"
  )
foreach(item ${VTKmofo_files})
  list(APPEND VTKmofo_sources "${CMAKE_CURRENT_SOURCE_DIR}/src/${item}")
endforeach()

# Make VTKmofo static library
add_library(vtkmofo STATIC ${VTKmofo_sources})

# Tell CMake where to put vtkmofo .mod files generated with libvtkmofo
set_property(TARGET vtkmofo
  PROPERTY
  Fortran_MODULE_DIRECTORY ${VTKmofo_mod_dir})

# Tell consumers where to find .mod files
target_include_directories(vtkmofo PUBLIC ${VTKmofo_mod_dir})

# Organize things in Visual Studio
source_group("VTKmofoLib" FILES ${VTKmofo_sources})
set_property(TARGET vtkmofo
  PROPERTY
  FOLDER "VTKmofo")
install(TARGETS vtkmofo DESTINATION lib)
install(DIRECTORY "${VTKmofo_mod_dir}" DESTINATION mod)


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
  if(USING_CAF_COMPILER_WRAPPER)
    add_caf_test("VTKmofo_${unit_test}_test" ${DEFAULT_NUM_IMAGES} ${CMAKE_CURRENT_BINARY_DIR}/tests/unit/${unit_test})
  else()
    add_test(NAME "VTKmofo_${unit_test}_test" COMMAND ${CMAKE_CURRENT_BINARY_DIR}/tests/unit/${unit_test} WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/tests/unit)
  endif()
  set_property(TEST "VTKmofo_${unit_test}_test" PROPERTY PASS_REGULAR_EXPRESSION "Test passed")
  set_property(TEST "VTKmofo_${unit_test}_test" PROPERTY LABELS "VTKmofo" "unit-test")
endforeach()

# Add integration tests and define the string that is used to signal success
foreach(integration_test ${VTKmofo_integration_test_list})
  if(USING_CAF_COMPILER_WRAPPER)
    add_caf_test("VTKmofo_${integration_test}_test" ${DEFAULT_NUM_IMAGES} ${CMAKE_CURRENT_BINARY_DIR}/tests/integration/${integration_test})
  else()
    add_test(NAME "VTKmofo_${integration_test}_test" COMMAND ${CMAKE_CURRENT_BINARY_DIR}/tests/integration/${integration_test} WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/tests/integration)
  endif()
  set_property(TEST "VTKmofo_${integration_test}_test" PROPERTY PASS_REGULAR_EXPRESSION "Finished")
  set_property(TEST "VTKmofo_${integration_test}_test" PROPERTY LABELS "VTKmofo" "integration-test")
endforeach()

add_custom_target(RUN_ALL_VTKmofo_TESTS
  COMMAND ${CMAKE_CTEST_COMMAND} --output-on-failure -L "VTKmofo")
add_dependencies(RUN_ALL_VTKmofo_TESTS ${VTKmofo_unit_test_list} ${VTKmofo_integration_test_list})
set_property(TARGET RUN_ALL_VTKmofo_TESTS PROPERTY FOLDER "All-Tests")
