MODULE VTK_serial_RectilinearGrid
    USE VTK_Serial_file, ONLY : VTK_element_dt
    USE vtk_attributes,  ONLY : attribute, attributes
    USE vtk_datasets,    ONLY : dataset
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
        PROCEDURE :: set_grid_data
    END TYPE VTK_serial_RectilinearGrid_dt

    INTERFACE

        MODULE SUBROUTINE set_grid_data (me, geometry, celldata, pointdata, celldatasets, pointdatasets)
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 05/06/2019
        !!
        !! This writes the body of a rectilinear grid
        !!
        CLASS(VTK_serial_RectilinearGrid_dt), INTENT(INOUT) :: me
        CLASS(dataset),    INTENT(IN)           :: geometry   !! DT of geometry to be printed
        CLASS(attribute),  INTENT(IN), OPTIONAL :: celldata   !!
        CLASS(attribute),  INTENT(IN), OPTIONAL :: pointdata  !!
        CLASS(attributes), DIMENSION(:), INTENT(IN), OPTIONAL :: celldatasets  !!
        CLASS(attributes), DIMENSION(:), INTENT(IN), OPTIONAL :: pointdatasets !!
        END SUBROUTINE set_grid_data

        MODULE SUBROUTINE rectilinear_grid_write (me)
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 05/06/2019
        !!
        !! This writes the body of a rectilinear grid into its data structure
        !!
        CLASS(VTK_serial_RectilinearGrid_dt), INTENT(INOUT) :: me
        END SUBROUTINE rectilinear_grid_write

    END INTERFACE

END MODULE VTK_serial_RectilinearGrid
