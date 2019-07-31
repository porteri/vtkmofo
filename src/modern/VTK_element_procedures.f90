SUBMODULE (VTK_element) VTK_element_procedures
    USE Precision, ONLY : i4k
    IMPLICIT NONE
    !! author: Ian Porter
    !! date: 05/06/2019
    !!
    !! This is the basic file for a serial VTK file
    !!

    CONTAINS

        MODULE PROCEDURE vtk_element_setup
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 05/06/2019
        !!
        !! This writes the header for a VTK file
        !!
        !! Example:
        !! <VTKFile type=”ImageData” version=”0.1” byte_order=”LittleEndian”>
        CHARACTER(LEN=*), PARAMETER   :: name = 'VTKFile'
        CHARACTER(LEN=:), ALLOCATABLE :: string
        CHARACTER(LEN=:), ALLOCATABLE :: type_string
        CHARACTER(LEN=*), PARAMETER   :: version_string = ' version="' // def_version // '"'
        CHARACTER(LEN=:), ALLOCATABLE :: byte_order_string
        CHARACTER(LEN=:), ALLOCATABLE :: compression_string

        IF (ALLOCATED(me%type)) THEN
            ALLOCATE(type_string,source=' type="' // me%type // '"')
        ELSE
            ERROR STOP "Error. Can't create VTK file without a known type. Terminated in vtk_element_setup"
        END IF
        IF (ALLOCATED(me%byte_order)) THEN
            ALLOCATE(byte_order_string,source=' byte_order="' // me%byte_order // '"')
        ELSE
            ALLOCATE(byte_order_string,source=' byte_order="' // def_byte_order // '"')
        END IF
        IF (ALLOCATED(me%compression)) THEN
            ALLOCATE(compression_string,source=' compression="' // me%compression // '"')
        ELSE
            ALLOCATE(compression_string,source='')
        END IF

        ALLOCATE(string, source=type_string // version_string // byte_order_string // compression_string)

        CALL me%setup(name=name,string=string,offset=4)

        END PROCEDURE vtk_element_setup

        MODULE PROCEDURE initialize
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 05/07/2019
        !!
        !! This is an external interface to allow the user or other routines to set the internal variables
        !!

        IF (PRESENT(type))           ALLOCATE(me%type,source=type)
        IF (PRESENT(byte_order))     ALLOCATE(me%byte_order,source=byte_order)
        IF (PRESENT(compression))    ALLOCATE(me%compression,source=compression)
        IF (PRESENT(file_extension)) ALLOCATE(me%file_extension,source=file_extension)

        CALL me%vtk_element_setup()

        END PROCEDURE initialize

        MODULE PROCEDURE finalize
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 05/07/2019
        !!
        !!
        !!
        IF (ALLOCATED(me%vtk_element)) THEN
            CALL me%vtk_element%finalize()
            CALL me%add(me%vtk_element)
        END IF

        END PROCEDURE finalize

        MODULE PROCEDURE gcc_bug_workaround_deallocate_vtk_element_single
        IMPLICIT NONE
        !! gcc Work-around for deallocating a multi-dimension derived type w/ allocatable character strings

        IF (ALLOCATED(foo%type))           DEALLOCATE(foo%type)
        IF (ALLOCATED(foo%version))        DEALLOCATE(foo%version)
        IF (ALLOCATED(foo%byte_order))     DEALLOCATE(foo%byte_order)
        IF (ALLOCATED(foo%compression))    DEALLOCATE(foo%compression)
        IF (ALLOCATED(foo%file_extension)) DEALLOCATE(foo%file_extension)
        IF (ALLOCATED(foo%filename))       DEALLOCATE(foo%filename)
        !CALL foo%piece%deallocate_piece_dt()
        IF (ALLOCATED(foo%vtk_element)) THEN
            CALL foo%vtk_element%me_deallocate()
            CALL foo%vtk_element%deallocate()
        END IF
        CALL foo%deallocate()

        END PROCEDURE gcc_bug_workaround_deallocate_vtk_element_single

END SUBMODULE VTK_element_procedures
