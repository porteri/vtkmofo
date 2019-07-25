SUBMODULE (VTK_Serial_file) VTK_Serial_file_implementation
    IMPLICIT NONE
    !! author: Ian Porter
    !! date: 05/06/2019
    !!
    !! This is the basic file for a serial VTK file
    !!

    CONTAINS

        MODULE PROCEDURE deallocate_VTK_file_dt
        IMPLICIT NONE
        !! gcc Work-around for deallocating a multi-dimension derived type w/ allocatable character strings

        IF (ALLOCATED(foo%VTK_data)) CALL foo%VTK_data%deallocate_vtk()

        CALL foo%deallocate() !! Deallocate the xml data

        END PROCEDURE deallocate_VTK_file_dt

END SUBMODULE VTK_Serial_file_implementation
