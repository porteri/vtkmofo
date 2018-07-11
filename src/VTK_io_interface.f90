MODULE vtk_io
    USE Precision
    USE vtk_attributes
    USE vtk_cells
    USE vtk_datasets
    USE vtk_vars
    IMPLICIT NONE
    !>@brief
    !> This module contains the output file to write to VTK format
    !>@author
    !> Ian Porter
    !>@date
    !> 12/1/2017

    PRIVATE
    PUBLIC :: vtk_legacy_write

    INTERFACE

        MODULE SUBROUTINE vtk_legacy_write (unit, geometry, celldata, pointdata, celldatasets, pointdatasets, &
          &                                 filename, multiple_io, data_type, title)
        !>@brief
        !> This subroutines writes the legacy vtk output file
        !>@author
        !> Ian Porter
        !>@date
        !> 12/1/2017
        !
        ! Input
        !
        ! unit        - File unit #
        ! vtk         - Geometry to be printed
        ! celldata    -
        ! pointdata   -
        ! filename    - Name of .vtk file to write to
        ! multiple_io - Identifier as to whether there will be a multiple files written (i.e., time-dependent output)
        ! data_type   - Identifier to write in ascii or Binary
        ! title       - Title for vtk output file line #2
        !

        CLASS(dataset),    INTENT(IN)           :: geometry
        CLASS(attribute),  INTENT(IN), OPTIONAL :: celldata, pointdata
        CLASS(attributes), DIMENSION(:), INTENT(IN), OPTIONAL :: celldatasets, pointdatasets
        INTEGER(i4k),      INTENT(IN)           :: unit
        INTEGER(i4k),      INTENT(IN), OPTIONAL :: data_type
        LOGICAL,           INTENT(IN), OPTIONAL :: multiple_io
        CHARACTER(LEN=*),  INTENT(IN), OPTIONAL :: filename, title

        END SUBROUTINE vtk_legacy_write

        MODULE SUBROUTINE vtk_legacy_read (unit, geometry, celldata, pointdata, celldatasets, pointdatasets, filename, &
          &                                data_type, title)
        !>@brief
        !> This subroutines reads the legacy vtk output file
        !>@author
        !> Ian Porter
        !>@date
        !> 12/20/2017
        !
        ! Input
        !
        ! unit      - File unit #
        ! filename  - Name of .vtk file to read from
        !
        ! Output
        !
        ! vtk       - Geometry to be read
        ! data_type - Identifier to read in ascii or Binary
        ! title     - Title for vtk output file line #2
        !

        CLASS(dataset),    INTENT(INOUT)        :: geometry
        CLASS(attribute),  INTENT(INOUT), OPTIONAL :: celldata, pointdata
        CLASS(attributes), DIMENSION(:), INTENT(INOUT), OPTIONAL :: celldatasets, pointdatasets
        INTEGER(i4k),      INTENT(IN)           :: unit
        INTEGER(i4k),      INTENT(OUT)          :: data_type
        CHARACTER(LEN=*),  INTENT(IN)           :: filename
        CHARACTER(LEN=*),  INTENT(OUT)          :: title

        END SUBROUTINE vtk_legacy_read

    END INTERFACE

END MODULE vtk_io
