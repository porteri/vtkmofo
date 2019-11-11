MODULE vtk_vars
    USE Precision, ONLY : i4k
    USE XML,       ONLY : ascii, binary, file_format
    IMPLICIT NONE
    !! author: Ian Porter
    !! date: 12/20/2017
    !!
    !! This module contains basic information needed for reading/writing to the vtk file
    !!
    PRIVATE
    PUBLIC :: vtkunit, version, default_title, default_fn, vtkfilename, vtktitle, fcnt, vtk_extension
    PUBLIC :: ascii, binary, file_format
                                              !! Selected file type
    INTEGER(i4k) :: vtkunit  = 20_i4k                                               !! Default VTK unit #
    INTEGER(i4k) :: fcnt     = 0_i4k                                                !! File counter for time-dependent output files
    INTEGER(i4k), PARAMETER :: bit=0_i4k, unsigned_char=1_i4k, char=2_i4k, unsigned_short=3_i4k, short=4_i4k, &
      &                        unsigned_int=5_i4k, int=6_i4k, unsigned_long=7_i4k, long=8_i4k, float=9_i4k,   &
      &                        double=10_i4k                                        !! Types of data
    CHARACTER(LEN=*), PARAMETER   :: version       = '# vtk DataFile Version 3.0'   !! VTK datafile version
    CHARACTER(LEN=*), PARAMETER   :: default_title = 'Version 3.0 VTK file'         !! Title card
    CHARACTER(LEN=*), PARAMETER   :: vtk_extension = '.vtk'                         !! File extension
    CHARACTER(LEN=*), PARAMETER   :: default_fn    = 'out' // vtk_extension         !! Default filename
    CHARACTER(LEN=:), ALLOCATABLE :: vtkfilename                                    !! Supplied filename
    CHARACTER(LEN=:), ALLOCATABLE :: vtktitle                                       !! Supplied title

END MODULE vtk_vars
