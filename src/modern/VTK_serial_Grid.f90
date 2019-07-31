MODULE VTK_serial_Grid
    USE XML,               ONLY : xml_file_dt, xml_element_dt
    USE vtk_datasets,      ONLY : dataset
    USE VTK_piece_element, ONLY : piece_dt
    USE VTK_element,       ONLY : VTK_element_dt
    IMPLICIT NONE
    !! author: Ian Porter
    !! date: 07/28/2019
    !!
    !! This contains the DTs for the different serial geometry types
    !!

    PRIVATE
    PUBLIC :: VTK_dataset_dt
    PUBLIC :: VTK_serial_RectilinearGrid_dt
    PUBLIC :: VTK_serial_StructuredGrid_dt
    PUBLIC :: VTK_serial_UnstructuredGrid_dt

    TYPE, EXTENDS(VTK_element_dt), ABSTRACT :: VTK_dataset_dt
        !! VTK dataset derived type
        CHARACTER(LEN=:), ALLOCATABLE :: WholeExtent  !! String for the whole extent of the range
        CHARACTER(LEN=:), ALLOCATABLE :: grid_type    !! Name of the grid type
        TYPE(piece_dt),   ALLOCATABLE :: piece        !! Piece DT (Currently only supporting one piece)
    CONTAINS
        PROCEDURE(abs_set_grid), DEFERRED :: set_grid
        PROCEDURE :: vtk_dataset_deallocate
        PROCEDURE :: finalize
    END TYPE VTK_dataset_dt

    TYPE, EXTENDS(VTK_dataset_dt) :: VTK_serial_RectilinearGrid_dt
        !! Serial file Rectilinear Grid
        PRIVATE
    CONTAINS
        PROCEDURE :: set_grid => Rectilineargrid_set_grid
    END TYPE VTK_serial_RectilinearGrid_dt

    TYPE, EXTENDS(VTK_dataset_dt) :: VTK_serial_StructuredGrid_dt
        !! Serial file Structured Grid
        PRIVATE
    CONTAINS
        PROCEDURE :: set_grid => Structuredgrid_set_grid
    END TYPE VTK_serial_StructuredGrid_dt

    TYPE, EXTENDS(VTK_dataset_dt) :: VTK_serial_UnstructuredGrid_dt
        !! Serial file Structured Grid
        PRIVATE
    CONTAINS
        PROCEDURE :: set_grid => Unstructuredgrid_set_grid
    END TYPE VTK_serial_UnstructuredGrid_dt

    INTERFACE

        MODULE SUBROUTINE abs_set_grid (me, geometry)
        IMPLICIT NONE
        !1 author: Ian Porter
        !! date: 07/09/2019
        !!
        !! Initializes a piece dt with the geometry information
        !!
        CLASS(VTK_dataset_dt), INTENT(INOUT) :: me
        CLASS(dataset),        INTENT(IN)    :: geometry

        END SUBROUTINE abs_set_grid

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

        MODULE SUBROUTINE Rectilineargrid_set_grid (me, geometry)
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 07/28/2019
        !!
        !! This writes the grid information for a rectilinear grid
        !!
        CLASS(VTK_serial_RectilinearGrid_dt), INTENT(INOUT) :: me         !! Serial geometry DT
        CLASS(dataset),                       INTENT(IN)    :: geometry   !! DT of geometry information

        END SUBROUTINE Rectilineargrid_set_grid

        MODULE SUBROUTINE Structuredgrid_set_grid (me, geometry)
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 07/28/2019
        !!
        !! This writes the grid information for a structured grid
        !!
        CLASS(VTK_serial_StructuredGrid_dt), INTENT(INOUT) :: me         !! Serial geometry DT
        CLASS(dataset),                      INTENT(IN)    :: geometry   !! DT of geometry information

        END SUBROUTINE Structuredgrid_set_grid

        MODULE SUBROUTINE Unstructuredgrid_set_grid (me, geometry)
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 07/29/2019
        !!
        !! This writes the grid information for an unstructured grid
        !!
        CLASS(VTK_serial_UnstructuredGrid_dt), INTENT(INOUT) :: me         !! Serial geometry DT
        CLASS(dataset),                        INTENT(IN)    :: geometry   !! DT of geometry information

        END SUBROUTINE Unstructuredgrid_set_grid

    END INTERFACE

END MODULE VTK_serial_Grid
