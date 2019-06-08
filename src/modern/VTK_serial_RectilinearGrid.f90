MODULE VTK_serial_RectilinearGrid
    USE VTK_Serial_file, ONLY : VTK_element_dt, xml_file_dt
    IMPLICIT NONE
    !!
    !!
    !!
    !!
    !!

    PRIVATE
    PUBLIC :: VTK_serial_RectilinearGrid_dt

    TYPE, EXTENDS(VTK_element_dt) :: VTK_serial_RectilinearGrid_dt
        CLASS(VTK_element_dt) :: file_header
    CONTAINS
        PROCEDURE :: set_grid_data
    END TYPE VTK_serial_RectilinearGrid_dt

    INTERFACE

        MODULE SUBROUTINE set_grid_data (me)
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 05/06/2019
        !!
        !! This writes the body of a rectilinear grid
        !!
        CLASS(VTK_serial_RectilinearGrid_dt), INTENT(INOUT) :: me
        END SUBROUTINE set_grid_data

    END INTERFACE

END MODULE VTK_serial_RectilinearGrid
