MODULE VTK_Serial_file
    USE XML,            ONLY : xml_file_dt, xml_element_dt
    USE vtk_attributes, ONLY : attribute, attributes
    USE vtk_datasets,   ONLY : dataset
    USE VTK_piece_element, ONLY : piece_dt
    USE VTK_element,    ONLY : VTK_element_dt
    IMPLICIT NONE
    !! author: Ian Porter
    !! date: 05/06/2019
    !!
    !! This is the basic file for a serial VTK file
    !!

    PRIVATE
    PUBLIC :: serial_file, VTK_dataset_dt

    TYPE, EXTENDS(VTK_element_dt) :: VTK_dataset_dt
!        CLASS(VTK_element_dt), ALLOCATABLE :: file_header
        TYPE(piece_dt), ALLOCATABLE :: piece
        CHARACTER(LEN=:), ALLOCATABLE :: WholeExtent
        CHARACTER(LEN=:), ALLOCATABLE :: grid_type
    CONTAINS
        PROCEDURE :: set_grid
        PROCEDURE :: vtk_dataset_deallocate
        PROCEDURE :: finalize
    END TYPE VTK_dataset_dt

    TYPE, EXTENDS(xml_file_dt) :: VTK_file_dt
        CLASS(VTK_dataset_dt), ALLOCATABLE :: VTK_dataset
    CONTAINS
        PROCEDURE, PRIVATE :: deallocate_VTK_file_dt
        GENERIC, PUBLIC :: me_deallocate => deallocate_VTK_file_dt
    END TYPE VTK_file_dt

    TYPE(VTK_file_dt), ALLOCATABLE :: serial_file    !! Serial VTK file
!   TYPE(VTK_file_dt), ALLOCATABLE :: parallel_file  !! Parallel VTK file
                                                     !! Parallel file is a TODO for future work

    INTERFACE

        MODULE SUBROUTINE set_grid (me, geometry)
        IMPLICIT NONE
        !1 author: Ian Porter
        !! date: 07/09/2019
        !!
        !! Initializes a piece dt with the geometry information
        !!
        CLASS(VTK_dataset_dt), INTENT(INOUT) :: me
        CLASS(dataset),        INTENT(IN)    :: geometry

        END SUBROUTINE set_grid

        MODULE SUBROUTINE finalize (me)
        IMPLICIT NONE
        !! Writes data inside of itself
        CLASS(VTK_dataset_dt), INTENT(INOUT) :: me

        END SUBROUTINE finalize

        RECURSIVE MODULE SUBROUTINE vtk_dataset_deallocate (foo)
        IMPLICIT NONE
        !! gcc Work-around for deallocating a multi-dimension derived type w/ allocatable character strings
        CLASS(VTK_dataset_dt), INTENT(INOUT) :: foo

        END SUBROUTINE vtk_dataset_deallocate

        RECURSIVE MODULE SUBROUTINE deallocate_VTK_file_dt (foo)
        IMPLICIT NONE
        !! gcc Work-around for deallocating a multi-dimension derived type w/ allocatable character strings
        CLASS(VTK_file_dt), INTENT(INOUT) :: foo

        END SUBROUTINE deallocate_VTK_file_dt

    END INTERFACE

END MODULE VTK_Serial_file
