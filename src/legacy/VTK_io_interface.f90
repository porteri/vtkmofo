MODULE vtk_io
    USE Precision, ONLY : i4k, r8k
    USE vtk_attributes
    USE vtk_cells
    USE vtk_datasets
    USE vtk_vars
    IMPLICIT NONE
    !! author: Ian Porter
    !! date: 12/1/2017
    !!
    !! This module contains the output file to write to VTK format
    !!
    PRIVATE
    PUBLIC :: vtk_legacy_write, vtk_serial_write

    INTERFACE vtk_legacy_write
        PROCEDURE :: vtk_legacy_full_write
        PROCEDURE :: vtk_legacy_append
        PROCEDURE :: vtk_legacy_finalize
    END INTERFACE vtk_legacy_write

    INTERFACE vtk_serial_write
        PROCEDURE :: vtk_serial_full_write
        PROCEDURE :: vtk_serial_append
        PROCEDURE :: vtk_serial_finalize
    END INTERFACE vtk_serial_write

    INTERFACE

        MODULE SUBROUTINE vtk_legacy_full_write (geometry, celldata, pointdata, celldatasets, pointdatasets, &
          &                                      unit, filename, multiple_io, data_type, title)
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 12/1/2017
        !!
        !! This subroutine writes the legacy vtk output file
        !!
        CLASS(dataset),    INTENT(IN)           :: geometry   !! DT of geometry to be printed
        CLASS(attribute),  INTENT(IN), OPTIONAL :: celldata   !!
        CLASS(attribute),  INTENT(IN), OPTIONAL :: pointdata  !!
        TYPE(attributes), DIMENSION(:), INTENT(IN), OPTIONAL :: celldatasets  !!
        TYPE(attributes), DIMENSION(:), INTENT(IN), OPTIONAL :: pointdatasets !!
        INTEGER(i4k),      INTENT(IN), OPTIONAL :: unit        !! VTK file unit
        INTEGER(i4k),      INTENT(IN), OPTIONAL :: data_type   !! Identifier to write in ascii or Binary
        LOGICAL,           INTENT(IN), OPTIONAL :: multiple_io !! Identifier as to whether there will be multiple files written
                                                               !! (i.e., time-dependent output)
        CHARACTER(LEN=*),  INTENT(IN), OPTIONAL :: filename    !! VTK filename
        CHARACTER(LEN=*),  INTENT(IN), OPTIONAL :: title       !! Title to be written on title line (#2) in output file

        END SUBROUTINE vtk_legacy_full_write

        MODULE SUBROUTINE vtk_legacy_append (celldata, pointdata, celldatasets, pointdatasets)
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 12/1/2017
        !!
        !! This subroutine appends data to the legacy vtk output file
        !!
        CLASS(attribute),  INTENT(IN), OPTIONAL :: celldata   !!
        CLASS(attribute),  INTENT(IN), OPTIONAL :: pointdata  !!
        TYPE(attributes), DIMENSION(:), INTENT(IN), OPTIONAL :: celldatasets  !!
        TYPE(attributes), DIMENSION(:), INTENT(IN), OPTIONAL :: pointdatasets !!

        END SUBROUTINE vtk_legacy_append

        MODULE SUBROUTINE vtk_legacy_finalize (finished)
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 06/03/2019
        !!
        !! This subroutine is a finalizer for the legacy vtk file write
        !!
        LOGICAL, INTENT(IN) :: finished  !! Finished flag

        END SUBROUTINE vtk_legacy_finalize

        MODULE SUBROUTINE vtk_legacy_read (unit, geometry, celldata, pointdata, celldatasets, pointdatasets, &
          &                                filename, data_type, title)
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 12/20/2017
        !!
        !! This subroutine reads the legacy vtk output file
        !!
        CLASS(dataset),    INTENT(INOUT)           :: geometry   !! DT of geometry to be printed
        CLASS(attribute),  INTENT(INOUT), OPTIONAL :: celldata   !!
        CLASS(attribute),  INTENT(INOUT), OPTIONAL :: pointdata  !!
        TYPE(attributes), DIMENSION(:), INTENT(INOUT), OPTIONAL :: celldatasets  !!
        TYPE(attributes), DIMENSION(:), INTENT(INOUT), OPTIONAL :: pointdatasets !!
        INTEGER(i4k),      INTENT(IN)           :: unit          !! VTK file unit
        INTEGER(i4k),      INTENT(OUT), OPTIONAL :: data_type    !! Identifier as to whether VTK file is ascii or Binary
        CHARACTER(LEN=*),  INTENT(IN),  OPTIONAL :: filename     !! VTK filename
        CHARACTER(LEN=*),  INTENT(OUT), OPTIONAL :: title        !! Title to be written on title line (#2) in output file

        END SUBROUTINE vtk_legacy_read

        MODULE SUBROUTINE vtk_serial_full_write (geometry, celldata, pointdata, celldatasets, pointdatasets, &
          &                                      unit, filename, multiple_io, data_type, title)
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 5/08/2019
        !!
        !! This subroutine writes the modern serial vtk output file
        !!
        CLASS(dataset),    INTENT(IN)           :: geometry   !! DT of geometry to be printed
        CLASS(attribute),  INTENT(IN), OPTIONAL :: celldata   !!
        CLASS(attribute),  INTENT(IN), OPTIONAL :: pointdata  !!
        TYPE(attributes), DIMENSION(:), INTENT(IN), OPTIONAL :: celldatasets  !!
        TYPE(attributes), DIMENSION(:), INTENT(IN), OPTIONAL :: pointdatasets !!
        INTEGER(i4k),      INTENT(IN), OPTIONAL :: unit        !! VTK file unit
        INTEGER(i4k),      INTENT(IN), OPTIONAL :: data_type   !! Identifier to write in ascii or Binary
        LOGICAL,           INTENT(IN), OPTIONAL :: multiple_io !! Identifier as to whether there will be multiple files written
                                                               !! (i.e., time-dependent output)
        CHARACTER(LEN=*),  INTENT(IN), OPTIONAL :: filename    !! VTK filename
        CHARACTER(LEN=*),  INTENT(IN), OPTIONAL :: title       !! Title to be written on title line (#2) in output file

        END SUBROUTINE vtk_serial_full_write

        MODULE SUBROUTINE vtk_serial_append (celldata, pointdata, celldatasets, pointdatasets)
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 06/24/2019
        !!
        !! This subroutine appends data to the legacy vtk output file
        !!
        CLASS(attribute),  INTENT(IN), OPTIONAL :: celldata   !!
        CLASS(attribute),  INTENT(IN), OPTIONAL :: pointdata  !!
        TYPE(attributes), DIMENSION(:), INTENT(IN), OPTIONAL :: celldatasets  !!
        TYPE(attributes), DIMENSION(:), INTENT(IN), OPTIONAL :: pointdatasets !!

        END SUBROUTINE vtk_serial_append

        MODULE SUBROUTINE vtk_serial_finalize (finished)
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 06/24/2019
        !!
        !! This subroutine is a finalizer for the legacy vtk file write
        !!
        LOGICAL, INTENT(IN) :: finished  !! Finished flag

        END SUBROUTINE vtk_serial_finalize

    END INTERFACE

END MODULE vtk_io
