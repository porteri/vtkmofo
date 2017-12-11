MODULE vtk
    USE Kinds
    USE vtk_types
    IMPLICIT NONE
    !>@brief
    !> This module contains the output file to write to VTK format
    !>@author
    !> Ian Porter
    !>@date
    !> 12/1/2017

    PRIVATE
    PUBLIC :: vtk_legacy_init

    CONTAINS
        SUBROUTINE vtk_legacy_init (vtk, data_type, filename)
        USE Kinds
        !>@brief
        !> This subroutines writes the legacy vtk output file
        !>@author
        !> Ian Porter
        !>@date
        !> 12/1/2017
        !
        ! Input
        !
        ! vtk       - Geometry to be printed
        ! data_type - Identifier to write in ascii or Binary
        ! filename  - Name of .vtk file to write to

        CLASS(vtk_datastruct), INTENT(IN) :: vtk
        INTEGER(i4k),     INTENT(IN), OPTIONAL :: data_type
        CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: filename
        INTEGER(i4k) :: inputstat
        LOGICAL      :: file_is_open
        INTEGER(i4k), DIMENSION(3) :: dims
        REAL(r8k),    DIMENSION(3) :: origin, spacing

        IF (PRESENT(data_type)) filetype    = data_type
        IF (PRESENT(filename))  vtkfilename = filename

        INQUIRE(unit = vtk%unit, opened = file_is_open)
        IF (.NOT. file_is_open) THEN
            SELECT CASE (filetype)
            CASE (ascii)
                OPEN(unit=vtk%unit, file=vtkfilename, iostat=inputstat, status='unknown', form='formatted')
            CASE (binary)
                OPEN(unit=vtk%unit, file=vtkfilename, iostat=inputstat, status='unknown', form='unformatted')
            END SELECT
        END IF

        WRITE(vtk%unit,'(a)') version
        WRITE(vtk%unit,'(a)') title
        SELECT CASE (filetype)
        CASE (ascii)
            WRITE(vtk%unit,'(a)') 'ASCII'
        CASE (binary)
            WRITE(vtk%unit,'(a)') 'BINARY'
        END SELECT

        CALL vtk%write()

        CLOSE(unit=vtkunit)

        END SUBROUTINE vtk_legacy_init
END MODULE vtk