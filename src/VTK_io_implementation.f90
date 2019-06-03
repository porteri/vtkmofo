SUBMODULE (vtk_io) vtk_io_implementation
    !! author: Ian Porter
    !! date: 12/1/2017
    !!
    !! This module implements the ability to read/write VTK formatted files
    !!
    CONTAINS

        MODULE PROCEDURE vtk_legacy_full_write
        USE Misc,     ONLY : to_uppercase
        USE vtk_vars, ONLY : default_fn, default_title, filetype, vtkfilename, vtktitle, ascii, binary, &
          &                  version, fcnt, file_extension
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 12/1/2017
        !!
        !! This subroutines writes the legacy vtk output file
        !!
        INTEGER(i4k)  :: i, inputstat, newunit
        LOGICAL, SAVE :: file_was_already_open = .FALSE.
        CHARACTER(LEN=:), ALLOCATABLE :: form, filetype_text
        CHARACTER(LEN=11) :: fm

        IF (PRESENT(data_type)) filetype = data_type            !! Calling program provided what file type to use for vtk file
        IF (ALLOCATED(vtkfilename)) DEALLOCATE(vtkfilename)
        IF (PRESENT(filename)) THEN
            ALLOCATE(vtkfilename, source=filename)              !! Calling program provided a filename
        ELSE
            ALLOCATE(vtkfilename, source=default_fn)            !! Calling program did not provide a filename. Use default
        END IF
        IF (PRESENT(multiple_io)) THEN
            IF (multiple_io) THEN
                mio_filename: BLOCK
                    CHARACTER(LEN=8) :: fcnt_char = ''          !! File count character
                    CHARACTER(LEN=:), ALLOCATABLE :: base_fn    !! Base file name
                    WRITE (fcnt_char,FMT='(i8)') fcnt
                    ALLOCATE(base_fn, source=vtkfilename(1:INDEX(to_uppercase(vtkfilename),to_uppercase(file_extension))-1))
                    DEALLOCATE(vtkfilename)
                    ALLOCATE(vtkfilename, source=base_fn // "_" // TRIM(ADJUSTL(fcnt_char)) // file_extension)
                    fcnt = fcnt + 1                             !! Increase timestep file counter by 1
                END BLOCK mio_filename
            END IF
        END IF
        IF (PRESENT(title)) THEN
            ALLOCATE(vtktitle, source=title)                    !! Calling program provided a title
        ELSE
            ALLOCATE(vtktitle, source=default_title)            !! Calling program did not provide a title. Use default
        END IF

        IF (PRESENT(unit)) THEN
            newunit = unit
            INQUIRE(unit=newunit, opened=file_was_already_open) !! Check to see if file is already open
            IF (.NOT. file_was_already_open) THEN               !! File is not yet open. Determine format from filetype
                SELECT CASE (filetype)
                CASE (ascii)
                    ALLOCATE(form, source='formatted')
                    ALLOCATE(filetype_text, source='ASCII')
                CASE (binary)
                    ALLOCATE(form, source='unformatted')
                    ALLOCATE(filetype_text, source='BINARY')
                CASE DEFAULT
                    WRITE(*,*) 'Warning: filetype is incorrectly defined. Will default to ASCII'
                    ALLOCATE(form, source='formatted')
                    ALLOCATE(filetype_text, source='ASCII')
                END SELECT
                OPEN(unit=newunit, file=vtkfilename, iostat=inputstat, status='REPLACE', form=form)
                                                                !! Open the VTK file
            ELSE                                                !! File is already open. Determine format based on file format
                INQUIRE(unit=newunit,form=fm)
                SELECT CASE (TO_UPPERCASE(TRIM(fm)))
                CASE ('FORMATTED')
                    ALLOCATE(filetype_text, source='ASCII')
                CASE DEFAULT
                    ALLOCATE(filetype_text, source='BINARY')
                END SELECT
            END IF
        ELSE
            !! No unit # provided. Make determination by value set for filetype
            SELECT CASE (filetype)
            CASE (ascii)
                ALLOCATE(form, source='formatted')
                ALLOCATE(filetype_text, source='ASCII')
            CASE (binary)
                ALLOCATE(form, source='unformatted')
                ALLOCATE(filetype_text, source='BINARY')
            CASE DEFAULT
                WRITE(*,*) 'Warning: filetype is incorrectly defined. Will default to ASCII'
                ALLOCATE(form, source='formatted')
                ALLOCATE(filetype_text, source='ASCII')
            END SELECT
            OPEN(newunit=newunit, file=vtkfilename, iostat=inputstat, status='REPLACE', form=form)
                                                                !! Open the VTK file
        END IF

        WRITE(newunit,100) version                              !! VTK version (currently, 3.0)
        WRITE(newunit,100) vtktitle                             !! VTK title card
        WRITE(newunit,100) filetype_text                        !! VTK file type

        CALL geometry%write(newunit)                            !! Write the geometry information

        IF (PRESENT(celldatasets)) THEN
            WRITE(newunit,101) celldatasets(1)%n
            DO i = 1, SIZE(celldatasets)
                CALL celldatasets(i)%attribute%write(newunit)   !! Write the cell data values
            END DO
        ELSE IF (PRESENT(celldata)) THEN
            WRITE(newunit,101) celldatasets(1)%n
            CALL celldata%write(newunit)                        !! Write the cell data values
        END IF

        IF (PRESENT(pointdatasets)) THEN
            WRITE(newunit,102) pointdatasets(1)%n
            DO I = 1, SIZE(pointdatasets)
                CALL pointdatasets(i)%attribute%write(newunit)
            END DO
        ELSE IF (PRESENT(pointdata)) THEN
            WRITE(newunit,102) pointdatasets(1)%n
            CALL pointdata%write(newunit)                       !! Write the point data values
        END IF

        IF (.NOT. file_was_already_open) THEN
            CLOSE(newunit)                                      !! Close the VTK file if file was not open prior to calling vtkmofo
        END IF

100     FORMAT(a)
101     FORMAT('CELL_DATA ',i0)
102     FORMAT('POINT_DATA ',i0)

        END PROCEDURE vtk_legacy_full_write

        MODULE PROCEDURE vtk_legacy_append
        USE Misc,     ONLY : to_uppercase
        USE vtk_vars, ONLY : default_fn, default_title, filetype, vtkfilename, vtktitle, ascii, binary, &
          &                  version, fcnt, file_extension
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 12/1/2017
        !!
        !! This subroutines writes the legacy vtk output file
        !!
        INTEGER(i4k)  :: i, inputstat, newunit
        LOGICAL, SAVE :: file_was_already_open = .FALSE.
        CHARACTER(LEN=:), ALLOCATABLE :: form, filetype_text
        CHARACTER(LEN=11) :: fm

        IF (PRESENT(celldatasets)) THEN
            WRITE(newunit,101) celldatasets(1)%n
            DO i = 1, SIZE(celldatasets)
                CALL celldatasets(i)%attribute%write(newunit)   !! Write the cell data values
            END DO
        ELSE IF (PRESENT(celldata)) THEN
            WRITE(newunit,101) celldatasets(1)%n
            CALL celldata%write(newunit)                        !! Write the cell data values
        END IF

        IF (PRESENT(pointdatasets)) THEN
            WRITE(newunit,102) pointdatasets(1)%n
            DO I = 1, SIZE(pointdatasets)
                CALL pointdatasets(i)%attribute%write(newunit)
            END DO
        ELSE IF (PRESENT(pointdata)) THEN
            WRITE(newunit,102) pointdatasets(1)%n
            CALL pointdata%write(newunit)                       !! Write the point data values
        END IF

        IF (.NOT. file_was_already_open) THEN
            CLOSE(newunit)                                      !! Close the VTK file if file was not open prior to calling vtkmofo
        END IF

101     FORMAT('CELL_DATA ',i0)
102     FORMAT('POINT_DATA ',i0)

        END PROCEDURE vtk_legacy_append

        MODULE PROCEDURE vtk_legacy_read
        USE Misc,     ONLY : def_len
        USE vtk_vars, ONLY : default_fn, default_title, filetype, vtkfilename, vtktitle, ascii, binary, version
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 12/20/2017
        !!
        !! This subroutines reads the legacy vtk output file
        !!
        INTEGER(i4k) :: i, inputstat
        LOGICAL      :: file_is_open
        CHARACTER(LEN=:), ALLOCATABLE :: form, filetype_text, vtk_version
        CHARACTER(LEN=def_len) :: line

        INQUIRE(unit = unit, opened = file_is_open)        !! Check to see if file is already open
        IF (.NOT. file_is_open) THEN                       !! File is not yet open. Determine format to open file
            OPEN(unit=unit, file=filename, iostat=inputstat, status='old')
                                                           !! Open the VTK file
        END IF

        READ(unit,100) line                                !! VTK version (currently, 3.0)
        ALLOCATE(vtk_version, source=TRIM(line))
        line = ''
        IF (vtk_version /= version) THEN
                                                           !! If the file is not version 3.0, abort read
            ERROR STOP 'ERROR: vtkmofo only works with version 3.0'
        END IF

        READ(unit,100) title                               !! VTK title card
        READ(unit,100) line                                !! VTK file type
        ALLOCATE(filetype_text, source=TRIM(line))
        line = ''

        CLOSE(unit)                                        !! Close the file to re-open it in the proper format

        SELECT CASE (filetype_text)
        CASE ('ASCII')
            data_type = ascii
            ALLOCATE(form, source='formatted')
        CASE ('BINARY')
            data_type = binary
            ALLOCATE(form, source='unformatted')
        CASE DEFAULT
            ERROR STOP 'Unsupported file type. Must be ASCII or BINARY. Terminated in vtk_legacy_read'
        END SELECT

        OPEN(unit=unit, file=filename, iostat=inputstat, status='old', form=form)
                                                           !! Open the VTK file in the proper format
        READ(unit,*)                                       !! Skip over this line
        READ(unit,*)                                       !! Skip over this line
        READ(unit,*)                                       !! Skip over this line

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

        ALLOCATE(vtkfilename, source=filename)             !! Save the filename for future internal use
        ALLOCATE(vtktitle, source=title)                   !! Save the title for future internal use
        filetype = data_type                               !! Save the file type for future internal use

100     FORMAT(a)

        END PROCEDURE vtk_legacy_read

END SUBMODULE vtk_io_implementation
