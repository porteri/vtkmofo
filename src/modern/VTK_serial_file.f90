MODULE VTK_Serial_file
    USE XML,               ONLY : xml_file_dt
    USE VTK_serial_grid,   ONLY : VTK_dataset_dt
    IMPLICIT NONE
    !! author: Ian Porter
    !! date: 05/06/2019
    !!
    !! This is the basic file for a serial VTK file
    !!

    PRIVATE
    PUBLIC :: serial_file, VTK_dataset_dt

    TYPE, EXTENDS(xml_file_dt) :: VTK_file_dt
        !! VTK file type derived type
        CLASS(VTK_dataset_dt), ALLOCATABLE :: VTK_dataset
    CONTAINS
        PROCEDURE, PRIVATE :: deallocate_VTK_file_dt
        GENERIC, PUBLIC :: me_deallocate => deallocate_VTK_file_dt
    END TYPE VTK_file_dt

    TYPE(VTK_file_dt), ALLOCATABLE :: serial_file    !! Serial VTK file
!   TYPE(VTK_file_dt), ALLOCATABLE :: parallel_file  !! Parallel VTK file
                                                     !! Parallel file is a TODO for future work

    INTERFACE

        RECURSIVE MODULE SUBROUTINE deallocate_VTK_file_dt (foo)
        IMPLICIT NONE
        !! gcc Work-around for deallocating a multi-dimension derived type w/ allocatable character strings
        CLASS(VTK_file_dt), INTENT(INOUT) :: foo

        END SUBROUTINE deallocate_VTK_file_dt

    END INTERFACE

END MODULE VTK_Serial_file
