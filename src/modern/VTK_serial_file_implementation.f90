SUBMODULE (VTK_Serial_file) VTK_Serial_file_implementation
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
        CHARACTER(LEN=*), PARAMETER   :: version_string = 'version="' // def_version // '"'
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
            ALLOCATE(byte_order_string,source='')
        END IF
        IF (ALLOCATED(me%compression)) THEN
            ALLOCATE(compression_string,source=' compression="' // me%compression // '"')
        ELSE
            ALLOCATE(compression_string,source='')
        END IF

        ALLOCATE(string, source=type_string // version_string // byte_order_string // compression_string)

        CALL me%setup(name=name,string=string)

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
        !! This writes the end of the file
        !!

!        WRITE(me%unit,'(a)') '</VTKFile>'

        END PROCEDURE finalize

END SUBMODULE VTK_Serial_file_implementation
