# This function handles the compiler and platform options
# for the following compilers:
# 1) gfortran
# 2) intel (not implemented / testted as part of this project)
#
set(CMAKE_VERBOSE_MAKEFILE OFF)
include_guard(DIRECTORY) # Check to see if the file has previously been processed

# Compiler options
if (CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
  # Intel platform specific settings
  add_definitions(-DINTEL_COMPILER)
  if (WIN32) # Windows options
    set(prefix "/")
    set(infix ":")
    set(Qf "Q")
    set(Q "Q")
    set(eq ":")
    set(colon ":")
    set(colon_ ":")
    set(libs_static "/libs:static")
    set(dbglibs "/dbglibs")
  else() # *nix options
    set(prefix "-")
    set(infix " ")
    set(Qf "f")
    set(Q "")
    set(eq "=")
    set(colon "")
    set(colon_ " ")
    set(libs_static "")
    set(dbglibs "")
  endif()

  set(Intel_Fortran_FLAGS_Release "${prefix}check${colon_}none ${prefix}O3 ${prefix}fpp ${prefix}MP")
  set(Intel_Fortran_FLAGS_Debug "${prefix}check${colon_}all ${prefix}Od ${prefix}fpp ${prefix}MP ${prefix}warn${colon_}all ${prefix}stand${colon_}f15 ${prefix}fpe${colon_}0")

#  "${prefix}nologo ${prefix}debug${infix}full ${prefix}MP ${prefix}Od ${prefix}standard-semantics ${prefix}warn${infix}errors ${prefix}stand${infix}f15 ${prefix}debug-parameters${infix}all ${prefix}warn${infix}unused ${prefix}warn${infix}interfaces ${prefix}${Qf}trapuv ${prefix}${Q}init${eq}snan ${prefix}${Q}init${eq}arrays ${prefix}fpe${colon}0 ${prefix}traceback ${prefix}check${colon_}bounds ${prefix}check${colon_}stack ${libs_static} ${prefix}threads ${dbglibs} ${prefix}free"
#  "${prefix}nologo ${prefix}debug${infix}full ${prefix}multiple-processes ${prefix}O0 ${prefix}standard-semantics ${prefix}warn${infix}errors ${prefix}stand${infix}f15 ${prefix}debug-parameters${infix}all ${prefix}warn${infix}declarations ${prefix}warn${infix}unused ${prefix}warn${infix}interfaces ${prefix}${Qf}trapuv ${prefix}${Q}init${eq}snan ${prefix}${Q}init${eq}arrays ${prefix}fpe${colon}0 ${prefix}traceback ${prefix}check${colon_}bounds ${prefix}check${colon_}stack ${libs_static} ${prefix}threads ${dbglibs} ${prefix}free"

  set(Intel_EXE_LINKER_FLAGS "${prefix}traceback ${prefix}stand${colon_}f15 ${prefix}${Q}coarray${colon_}distributed ${prefix}fpp")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
  # GFortran build configs
  if (CMAKE_Fortran_COMPILER_VERSION VERSION_LESS 8)
    set(Fortran_standard "f2008")
  else()
    set(Fortran_standard "f2018")
  endif()
  set(GNU_Fortran_FLAGS_Release "-fbacktrace -std=${Fortran_standard} -ffree-form -fcheck=all -fcoarray=single")
  set(GNU_Fortran_FLAGS_Debug "-fbacktrace -std=${Fortran_standard} -ffree-form -fcoarray=single")
else()
  message(WARNING
    "\n"
    "Attempting to build with untested Fortran compiler: ${CMAKE_Fortran_COMPILER_ID}. "
    "Please report any failures through the vtkmofo Git issues\n\n"
  )
endif()

list(JOIN ${CMAKE_Fortran_COMPILER_ID}_Fortran_FLAGS_${CMAKE_BUILD_TYPE} " " CMAKE_Fortran_FLAGS)
