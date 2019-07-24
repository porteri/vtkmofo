MODULE VTK_serial_RectilinearGrid
    USE VTK_element,    ONLY : VTK_element_dt
    USE vtk_attributes, ONLY : attribute, attributes
    USE vtk_datasets,   ONLY : dataset
    IMPLICIT NONE
    !!
    !!
    !!
    !!
    !!

    PRIVATE
    PUBLIC :: VTK_serial_RectilinearGrid_dt

    TYPE, EXTENDS(VTK_element_dt) :: VTK_serial_RectilinearGrid_dt
        CLASS(VTK_element_dt), ALLOCATABLE :: file_header
    CONTAINS
        PROCEDURE :: set_grid
    END TYPE VTK_serial_RectilinearGrid_dt

    INTERFACE

        MODULE SUBROUTINE set_grid (me, geometry)
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 05/06/2019
        !!
        !! This writes the body of a rectilinear grid
        !!
        CLASS(VTK_serial_RectilinearGrid_dt), INTENT(INOUT) :: me
        CLASS(dataset),                       INTENT(IN)    :: geometry   !! DT of geometry to be printed

        END SUBROUTINE set_grid

    END INTERFACE

END MODULE VTK_serial_RectilinearGrid
