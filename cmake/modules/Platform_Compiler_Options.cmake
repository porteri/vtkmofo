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

  set(Intel_Fortran_FLAGS_Release "${prefix}check${colon_}none ${prefix}O3")
  set(Intel_Fortran_FLAGS_Debug "${prefix}check${colon_}all ${prefix}O0")
  
#  "${prefix}nologo ${prefix}debug${infix}full ${prefix}MP ${prefix}Od ${prefix}standard-semantics ${prefix}warn${infix}errors ${prefix}stand${infix}f15 ${prefix}debug-parameters${infix}all ${prefix}warn${infix}declarations ${prefix}warn${infix}unused ${prefix}warn${infix}interfaces ${prefix}${Qf}trapuv ${prefix}${Q}init${eq}snan ${prefix}${Q}init${eq}arrays ${prefix}fpe${colon}0 ${prefix}traceback ${prefix}check${colon_}bounds ${prefix}check${colon_}stack ${libs_static} ${prefix}threads ${dbglibs} ${prefix}free"
#  "${prefix}nologo ${prefix}debug${infix}full ${prefix}multiple-processes ${prefix}O0 ${prefix}standard-semantics ${prefix}warn${infix}errors ${prefix}stand${infix}f15 ${prefix}debug-parameters${infix}all ${prefix}warn${infix}declarations ${prefix}warn${infix}unused ${prefix}warn${infix}interfaces ${prefix}${Qf}trapuv ${prefix}${Q}init${eq}snan ${prefix}${Q}init${eq}arrays ${prefix}fpe${colon}0 ${prefix}traceback ${prefix}check${colon_}bounds ${prefix}check${colon_}stack ${libs_static} ${prefix}threads ${dbglibs} ${prefix}free"
  
  set(Intel_EXE_LINKER_FLAGS "${prefix}traceback ${prefix}stand${colon_}f15 ${prefix}${Q}coarray${colon_}distributed")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
  # GFortran build configs
  set(GNU_Fortran_FLAGS_Release "-fbacktrace -std=f2018 -ffree-form -fcheck=all")
  set(GNU_Fortran_FLAGS_Debug "-fbacktrace -std=f2018 -ffree-form")
else()
  message(WARNING
    "\n"
    "Attempting to build with untested Fortran compiler: ${CMAKE_Fortran_COMPILER_ID}. "
    "Please report any failures through the vtkmofo Git issues\n\n"
  )
endif()

list(JOIN ${CMAKE_Fortran_COMPILER_ID}_Fortran_FLAGS_${CMAKE_BUILD_TYPE} " " CMAKE_Fortran_FLAGS)
