SUBMODULE (vtk) vtk_implementation
    USE Precision
    USE vtk_attributes
    USE vtk_cells
    USE vtk_datasets
    USE vtk_vars
    IMPLICIT NONE

    CONTAINS

        MODULE PROCEDURE vtk_legacy_write
        USE Precision
        USE Misc,           ONLY : to_uppercase
        USE vtk_datasets,   ONLY : dataset
        USE vtk_attributes, ONLY : attribute, attributes
        USE vtk_vars,       ONLY : default_fn, default_title, filetype, vtkfilename, vtktitle, ascii, binary, &
          &                        version, fcnt, file_extension
        INTEGER(i4k)     :: i, inputstat
        LOGICAL          :: file_is_open
        CHARACTER(LEN=8) :: fcnt_char = ''
        CHARACTER(LEN=:), ALLOCATABLE :: form, filetype_text

        IF (PRESENT(data_type)) filetype    = data_type    !! Calling program provided what file type to use for vtk file
        IF (PRESENT(filename)) THEN
            vtkfilename = filename                         !! Calling program provided a filename
        ELSE
            vtkfilename = default_fn                       !! Calling program did not provide a filename. Use default
        END IF
        IF (PRESENT(multiple_io)) THEN
            IF (multiple_io) THEN
                WRITE (fcnt_char,FMT='(i8)') fcnt
                vtkfilename = vtkfilename(1:INDEX(to_uppercase(vtkfilename),to_uppercase(file_extension))-1) // "_" // &
                  &           TRIM(ADJUSTL(fcnt_char)) // file_extension
                fcnt = fcnt + 1
            END IF
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
                form          = 'formatted'
                filetype_text = 'ASCII'
            CASE (binary)
                form          = 'unformatted'
                filetype_text = 'BINARY'
            CASE DEFAULT
                WRITE(*,*) 'Warning: filetype is incorrectly defined. Will default to ASCII'
                form          = 'formatted'
                filetype_text = 'ASCII'
            END SELECT
            OPEN(unit=unit, file=vtkfilename, iostat=inputstat, status='unknown', form=form)
                                                           !! Open the VTK file
        END IF

        WRITE(unit,100) version                            !! VTK version (currently, 3.0)
        WRITE(unit,100) vtktitle                           !! VTK title card
        WRITE(unit,100) filetype_text                      !! VTK file type

        CALL geometry%write(unit)                          !! Write the geometry information
        IF (PRESENT(celldatasets)) THEN
            WRITE(unit,101) celldatasets(1)%n
            DO i = 1, SIZE(celldatasets)
                CALL celldatasets(i)%attribute%write(unit) !! Write the cell data values
            END DO
        ELSE IF (PRESENT(celldata)) THEN
            WRITE(unit,101) celldatasets(1)%n
            CALL celldata%write(unit)                      !! Write the cell data values
        END IF
        IF (PRESENT(pointdatasets)) THEN
            WRITE(unit,102) pointdatasets(1)%n
            DO I = 1, SIZE(pointdatasets)
                CALL pointdatasets(i)%attribute%write(unit)
            END DO
        ELSE IF (PRESENT(pointdata)) THEN
            WRITE(unit,102) pointdatasets(1)%n
            CALL pointdata%write(unit)                     !! Write the point data values
        END IF

        CLOSE(unit)                                        !! Close the VTK file
100     FORMAT(a)
101     FORMAT('CELL_DATA ',i0)
102     FORMAT('POINT_DATA ',i0)
        END PROCEDURE vtk_legacy_write

        MODULE PROCEDURE vtk_legacy_read
        USE Precision
        USE vtk_datasets,   ONLY : dataset
        USE vtk_attributes, ONLY : attribute, attributes
        USE vtk_vars,       ONLY : default_fn, default_title, filetype, vtkfilename, vtktitle, ascii, binary, version
        INTEGER(i4k) :: i, inputstat
        LOGICAL      :: file_is_open
        CHARACTER(LEN=:), ALLOCATABLE :: form, filetype_text, vtk_version, line

        INQUIRE(unit = unit, opened = file_is_open)        !! Check to see if file is already open
        IF (.NOT. file_is_open) THEN                       !! File is not yet open. Determine format to open file
            OPEN(unit=unit, file=filename, iostat=inputstat, status='old')
                                                           !! Open the VTK file
        END IF

        READ(unit,100) vtk_version                         !! VTK version (currently, 3.0)
        IF (vtk_version /= version) THEN
                                                           !! If the file is not version 3.0, abort read
            ERROR STOP 'ERROR: vtkmofo only works with version 3.0'
        END IF

        READ(unit,100) title                               !! VTK title card
        READ(unit,100) filetype_text                       !! VTK file type

        CLOSE(unit)                                        !! Close the file to re-open it in the proper format

        SELECT CASE (filetype_text)
        CASE ('ASCII')
            data_type = ascii
            form      = 'formatted'
        CASE ('BINARY')
            data_type = binary
            form      = 'unformatted'
        END SELECT

        OPEN(unit=unit, file=filename, iostat=inputstat, status='old', form=form)
                                                           !! Open the VTK file in the proper format
        READ(unit,100) line                                !! Skip over this line
        READ(unit,100) line                                !! Skip over this line
        READ(unit,100) line                                !! Skip over this line

        CALL geometry%read(unit)                           !! Read the information from the file

        IF (PRESENT(celldatasets)) THEN
            READ(unit,*)
            DO i = 1, SIZE(celldatasets)
                CALL celldatasets(i)%attribute%read(unit)  !! Write the cell data values
            END DO
        ELSE IF (PRESENT(celldata)) THEN
            READ(unit,*)
            CALL celldata%write(unit)                      !! Write the cell data values
        END IF
        IF (PRESENT(pointdatasets)) THEN
            READ(unit,*)
            DO I = 1, SIZE(pointdatasets)
                CALL pointdatasets(i)%attribute%read(unit)
            END DO
        ELSE IF (PRESENT(pointdata)) THEN
            READ(unit,*)
            CALL pointdata%write(unit)                     !! Write the point data values
        END IF

        CLOSE(unit)                                        !! Close the VTK file

        vtkfilename = filename                             !! Save the filename for future internal use
        vtktitle    = title                                !! Save the title for future internal use
        filetype    = data_type                            !! Save the file type for future internal use

100     FORMAT(a)
        END PROCEDURE vtk_legacy_read

END SUBMODULE vtk_implementation
