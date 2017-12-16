# CMAKE minimum version
cmake_minimum_required(VERSION 3.2.2)

# Project name
project(vtkmofo LANGUAGES Fortran)

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
  # Full debug, Fortran 2008 standard, free format (due to automatic build to .f file for testing), Check bounds
  set(CMAKE_Fortran_FLAGS
    "${prefix}nologo ${prefix}debug${infix}full ${prefix}multiple-processes ${prefix}O0 ${prefix}standard-semantics ${prefix}warn${infix}errors ${prefix}stand${infix}f15 ${prefix}debug-parameters${infix}all ${prefix}warn${infix}declarations ${prefix}warn${infix}unused ${prefix}warn${infix}interfaces ${prefix}${Qf}trapuv ${prefix}${Q}init${eq}snan ${prefix}${Q}init${eq}arrays ${prefix}fpe${colon}0 ${prefix}traceback ${prefix}check${colon_}bounds ${prefix}check${colon_}stack ${libs_static} ${prefix}threads ${dbglibs} ${prefix}free"
  )
elseif("${CMAKE_Fortran_COMPILER_ID}" MATCHES "GNU")
  set(gnu_compiler true)
  # Fortran 2008 standard, free format (due to automatic build to .f file for testing)
  set(CMAKE_Fortran_FLAGS "${prefix}std=f2008 ${prefix}fcheck=all ${prefix}Wall ${prefix}Wno-unused-dummy-argument ${prefix}ffree-form ${prefix}fprofile-arcs ${prefix}ftest-coverage" )
else()
  message(WARNING
    "\n"
    "Attempting to build with untested Fortran compiler: ${CMAKE_Fortran_COMPILER_ID}. "
    "Please report any failures through the vtkmofo Git issues\n\n"
  )
endif()

set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/mod)

# Specify directories
set(src
"src/Kinds.f90"
"src/Misc.f90"
"src/VTK_attributes.f90"
"src/VTK_cells.f90"
"src/VTK_datasets.f90"
"src/VTK_output.f90"
)

# Make as a static library
include_directories(${CMAKE_BINARY_DIR}/mod)
add_library(vtkmofo ${src})
install(TARGETS vtkmofo DESTINATION lib)
install(DIRECTORY "${CMAKE_BINARY_DIR}/mod/" DESTINATION mod)

#Specify all the subdirectories containing codes that produce executable files or libraries:
set(subdirectories_list tests/unit tests/integration)
foreach(subdirectory ${subdirectories_list})
  add_subdirectory(${subdirectory})
endforeach()

enable_testing()

# Unit Tests
set(unit_test_list attributes cells
                   )

# Integration Tests
set(integration_test_list Driver
                          )
# Add unit tests and define the string that is used to signal success
foreach(unit_test ${unit_test_list})
  add_test(NAME "${unit_test}_test" COMMAND ${CMAKE_BINARY_DIR}/tests/unit/${unit_test}_unit WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/tests/unit/)
  set_property(TEST "${unit_test}_test" PROPERTY PASS_REGULAR_EXPRESSION "Test passed")
endforeach()

# Add integration tests and define the string that is used to signal success
foreach(integration_test ${integration_test_list})
  add_test(NAME "${integration_test}_test" COMMAND ${CMAKE_BINARY_DIR}/tests/integration/${integration_test} WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/tests/integration/)
  set_property(TEST "${integration_test}_test" PROPERTY PASS_REGULAR_EXPRESSION "Finished")
endforeach()
