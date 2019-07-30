MODULE VTK_serial_Grid
    USE VTK_element,     ONLY : VTK_element_dt
    USE vtk_datasets,    ONLY : dataset
    USE VTK_serial_file, ONLY : VTK_dataset_dt
    IMPLICIT NONE
    !! author: Ian Porter
    !! date: 07/28/2019
    !!
    !! This contains the DTs for the different serial geometry types
    !!

    PRIVATE
    PUBLIC :: VTK_serial_RectilinearGrid_dt, VTK_serial_StructuredGrid_dt

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

    INTERFACE

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

    END INTERFACE

END MODULE VTK_serial_Grid
