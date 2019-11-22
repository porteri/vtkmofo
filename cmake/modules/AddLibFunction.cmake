# Functions to abstract common tasks for creating and manipulating targets & tests
include_guard(DIRECTORY) # Check to see if the file has previously been processed
# Create a function to generate libraries from lists of source files
# given relative to the current CMakeLists.txt
#
# Mandatory arguments:
#
#   Positional argument 1: lib_name
#       Name of library to create (without leading "lib")
#   Keyword Argument SOURCES:
#       A list of source files relative to ${CMAKE_CURRENT_SOURCE_DIR} to compile
#
# Optional keyword arguments:
#    LINKAGE: Allowed values are STATIC, SHARED, MODULE. Not passing a value
#             defaults to CMake's default behavior. (BUILD_SHARED_LIBS can override
#             CMakes default behavior but LINKAGE specified here takes precedence.
#             set LIB_FORCE_LINKAGE=(STATIC | SHARED | MODULE) to override.)
#    INSTALL_DEST: Subdirectory install location. Defaults to "lib".

function(add_lib lib_name)
  # Parse arguments
  set(options "")
  set(oneValueArgs LINKAGE INSTALL_DEST)
  set(multiValueArgs SOURCES)
  cmake_parse_arguments(add_lib "${options}" "${oneValueArgs}" "${multiValueArgs}"
    ${ARGN})
  if(NOT add_lib_SOURCES)
    message( FATAL_ERROR "Argument 'SOURCES' to add_lib() CMake function is mandatory!
Please pass the source file names (relative to the ${CMAKE_CURRENT_SOURCE_DIR}).")
  endif()

  # Add each source file (with path) to list
  foreach(src IN LISTS add_lib_SOURCES)
    list(APPEND ${lib_name}Sources "${CMAKE_CURRENT_SOURCE_DIR}/${src}")
  endforeach()

  # Determine linkage if specified; default to system default behavior
  # See CMake documentation for BUILD_SHARED_LIBS variable
  set(ALLOWED_LINKAGE STATIC SHARED MODULE)
  if(add_lib_LINKAGE)
    if(NOT ${add_lib_LINKAGE} IN_LIST ALLOWED_LINKAGE)
      message( FATAL_ERROR "Argument 'LINKAGE' passed to add_lib() must be one of:
    STATIC, SHARED, or MODULE to match valid options passed to add_library()")
    else()
      set(LINKAGE ${add_lib_LINKAGE})
    endif()
  endif()

  if(LIB_FORCE_LINKAGE)
    if(NOT ${LIB_FORCE_LINKAGE} IN_LIST ALLOWED_LINKAGE)
      message( FATAL_ERROR "'LIB_FORCE_LINKAGE' must be one of:
    STATIC, SHARED, or MODULE to match valid options passed to add_library()")
    else()
      set(LINKAGE ${LIB_FORCE_LINKAGE})
    endif()
  endif()

  # Add library target using info processed so far
  add_library(${lib_name} ${LINKAGE}
    ${${lib_name}Sources})

  get_target_property(LNK_LANG ${lib_name} LINKER_LANGUAGE)
  if(NOT ${LINKER_LANGUAGE} STREQUAL C)
    target_compile_options(${lib_name} BEFORE
      PRIVATE ${prefix}warn${infix}errors)
    if(TARGET OpenCoarrays::caf_mpi_static)
      target_link_libraries(${lib_name}
	PUBLIC OpenCoarrays::caf_mpi_static)
    endif()
  endif()

  # Tell CMake where to install it
  set(_INSTALL_DEST "lib")
  if(add_lib_INSTALL_DEST)
    set(_INSTALL_DEST ${add_lib_INSTALL_DEST})
  endif()
endfunction(add_lib)
