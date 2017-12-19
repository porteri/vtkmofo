MODULE vtk
    USE Kinds
    USE vtk_datasets
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
        SUBROUTINE vtk_legacy_init (vtk, unit, data_type, filename, title)
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
        ! unit      - File unit #
        ! data_type - Identifier to write in ascii or Binary
        ! filename  - Name of .vtk file to write to
        ! title     - Title for vtk output file line #2

        CLASS(dataset),   INTENT(IN) :: vtk
        INTEGER(i4k),     INTENT(IN) :: unit
        INTEGER(i4k),     INTENT(IN), OPTIONAL :: data_type
        CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: filename, title
        INTEGER(i4k) :: inputstat
        LOGICAL      :: file_is_open
        CHARACTER(LEN=:), ALLOCATABLE :: form, filetype_text

        IF (PRESENT(data_type)) filetype    = data_type    !! Calling program provided what file type to use for vtk file
        IF (PRESENT(filename)) THEN
            vtkfilename = filename                         !! Calling program provided a filename
        ELSE
            vtkfilename = default_fn                       !! Calling program did not provide a filename. Use default
        END IF

        IF (PRESENT(title)) THEN
            vtktitle    = title                            !! Calling program provided a title
        ELSE
            vtktitle    = default_title                    !! Calling program did not provide a title. Use default
        END IF

        INQUIRE(unit = unit, opened = file_is_open)        !! Check to see if file is already open
        IF (.NOT. file_is_open) THEN                       !! File is not yet open. Determine format to open file
            SELECT CASE (filetype)
            CASE (ascii)
                form='formatted'
            CASE (binary)
                form = 'unformatted'
            END SELECT
            OPEN(unit=unit, file=vtkfilename, iostat=inputstat, status='unknown', form=form)
                                                           !! Open the VTK file
        END IF

        WRITE(unit,'(a)') version                          !! VTK version (currently, 3.0)
        WRITE(unit,'(a)') vtktitle                         !! VTK title card
        SELECT CASE (filetype)
        CASE (ascii)
            filetype_text = 'ASCII'
        CASE (binary)
            filetype_text = 'BINARY'
        END SELECT
        WRITE(unit,'(a)') filetype_text                    !! VTK file type

        CALL vtk%write(unit)                               !! Write the information

        CLOSE(unit=vtkunit)                                !! Close the VTK file

        END SUBROUTINE vtk_legacy_init
END MODULE vtk