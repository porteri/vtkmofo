SUBMODULE (VTK_serial_RectilinearGrid) RectilinearGrid_sub
    !! author: Ian Porter
    !! date: 05/06/2019
    !!
    !! This submodule implements the procedures for a serial Rectilinear Grid
    !!

    CHARACTER(LEN=*), PARAMETER :: file_extension = ".vtr"
    CHARACTER(LEN=*), PARAMETER :: grid_type = "RectilinearGrid"


    CONTAINS

        MODULE PROCEDURE set_grid_data
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 05/06/2019
        !!
        !! This sets parameters specific to the DT
        !!

        CALL me%initialize(type=grid_type,file_extension=file_extension)

        END PROCEDURE set_grid_data

END SUBMODULE RectilinearGrid_sub
