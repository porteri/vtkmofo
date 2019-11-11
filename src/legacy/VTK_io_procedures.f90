SUBMODULE (vtk_io) vtk_io_implementation
    !! author: Ian Porter
    !! date: 12/1/2017
    !!
    !! This module implements the ability to read/write VTK formatted files
    !!

    LOGICAL,      SAVE :: file_was_already_open     = .FALSE.
    LOGICAL,      SAVE :: printed_cell_data_header  = .FALSE.
    LOGICAL,      SAVE :: printed_point_data_header = .FALSE.
    INTEGER(i4k), SAVE :: newunit
    CHARACTER(LEN=:), ALLOCATABLE, SAVE :: form

    CONTAINS

        MODULE PROCEDURE vtk_legacy_full_write
        USE Misc,     ONLY : to_uppercase, trim_from_string
        USE vtk_vars, ONLY : default_fn, default_title, vtkfilename, vtktitle, version, fcnt, vtk_extension
        USE XML,      ONLY : convert_format_to_string, file_format_text, file_format, ascii, binary, format_ascii, format_binary
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 12/1/2017
        !!
        !! This subroutine writes the legacy vtk output file
        !!
        INTEGER(i4k) :: i, inputstat
        CHARACTER(LEN=11) :: fm

        ! Clear out any pre-existing data
        IF (ALLOCATED(vtkfilename))      DEALLOCATE(vtkfilename)
        IF (ALLOCATED(form))             DEALLOCATE(form)
        IF (ALLOCATED(vtktitle))         DEALLOCATE(vtktitle)
        IF (ALLOCATED(file_format_text)) DEALLOCATE(file_format_text)

        IF (PRESENT(format)) THEN
            file_format = format                                !! Calling program provided what file type to use for vtk file
        ELSE
            file_format = ascii                                 !! Default to ascii
        END IF
        IF (PRESENT(filename)) THEN
            ALLOCATE(vtkfilename, source=trim_from_string(filename,vtk_extension) // vtk_extension)
                                                                !! Calling program provided a filename
        ELSE
            ALLOCATE(vtkfilename, source=default_fn)            !! Calling program did not provide a filename. Use default
        END IF
        IF (PRESENT(multiple_io)) THEN
            IF (multiple_io) THEN
                mio_filename: BLOCK
                    CHARACTER(LEN=8) :: fcnt_char = ''          !! File count character
                    CHARACTER(LEN=:), ALLOCATABLE :: base_fn    !! Base file name
                    WRITE (fcnt_char,FMT='(i8)') fcnt
                    !ALLOCATE(base_fn, source=vtkfilename(1:INDEX(to_uppercase(vtkfilename),to_uppercase(file_extension))-1))
                    ALLOCATE(base_fn, source=trim_from_string(vtkfilename,vtk_extension))
                    DEALLOCATE(vtkfilename)
                    ALLOCATE(vtkfilename, source=base_fn // "_" // TRIM(ADJUSTL(fcnt_char)) // vtk_extension)
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
            IF (.NOT. file_was_already_open) THEN               !! File is not yet open. Determine format from file_format
                SELECT CASE (file_format)
                CASE (ascii)
                    ALLOCATE(form, source='formatted')
                CASE (binary)
                    ALLOCATE(form, source='unformatted')
                CASE DEFAULT
                    ERROR STOP 'Error: file_format is incorrectly defined in vtk_legacy_full_write'
                END SELECT
                ALLOCATE(file_format_text, source=convert_format_to_string(file_format))
                OPEN(unit=newunit, file=vtkfilename, iostat=inputstat, status='REPLACE', form=form)
                                                                !! Open the VTK file
            ELSE                                                !! File is already open. Determine format based on file format
                INQUIRE(unit=newunit,form=fm)
                SELECT CASE (TO_UPPERCASE(TRIM(fm)))
                CASE ('FORMATTED')
                    ALLOCATE(file_format_text, source=format_ascii)
                CASE DEFAULT
                    ALLOCATE(file_format_text, source=format_binary)
                END SELECT
            END IF
        ELSE
            !! No unit # provided. Make determination by value set for file_format
            SELECT CASE (file_format)
            CASE (ascii)
                ALLOCATE(form, source='formatted')
            CASE (binary)
                ALLOCATE(form, source='unformatted')
            CASE DEFAULT
                WRITE(*,*) 'Warning: file_format is incorrectly defined. Will default to ASCII'
                ALLOCATE(form, source='formatted')
            END SELECT
            ALLOCATE(file_format_text, source=convert_format_to_string(file_format))
            OPEN(newunit=newunit, file=vtkfilename, iostat=inputstat, status='REPLACE', form=form)
                                                                !! Open the VTK file
        END IF

        WRITE(newunit,100) version                              !! VTK version (currently, 3.0)
        WRITE(newunit,100) vtktitle                             !! VTK title card
        WRITE(newunit,100) file_format_text                     !! VTK file type

        CALL geometry%write(newunit)                            !! Write the geometry information

        IF (PRESENT(celldatasets)) THEN
            WRITE(newunit,101) celldatasets(1)%attribute%nvals
            DO i = 1, SIZE(celldatasets)
                CALL celldatasets(i)%attribute%write(newunit)   !! Write the cell data values
            END DO
        ELSE IF (PRESENT(celldata)) THEN
            WRITE(newunit,101) celldata%nvals
            CALL celldata%write(newunit)                        !! Write the cell data values
        END IF

        IF (PRESENT(pointdatasets)) THEN
            WRITE(newunit,102) pointdatasets(1)%attribute%nvals
            DO I = 1, SIZE(pointdatasets)
                CALL pointdatasets(i)%attribute%write(newunit)
            END DO
        ELSE IF (PRESENT(pointdata)) THEN
            WRITE(newunit,102) pointdata%nvals
            CALL pointdata%write(newunit)                       !! Write the point data values
        END IF

        IF (ANY([ PRESENT(celldatasets), PRESENT(celldata), PRESENT(pointdatasets), PRESENT(pointdata) ])) THEN
            CALL vtk_legacy_finalize (finished=.TRUE.)          !! Full legacy write w/ data. Close file.
        ELSE
            CALL vtk_legacy_finalize (finished=.FALSE.)         !! No data was provided, only geometry info. Do not close file.
        END IF

100     FORMAT(a)
101     FORMAT('CELL_DATA ',i0)
102     FORMAT('POINT_DATA ',i0)

        END PROCEDURE vtk_legacy_full_write

        MODULE PROCEDURE vtk_legacy_append
        USE Misc,     ONLY : to_uppercase
        USE vtk_vars, ONLY : default_fn, default_title, version
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 12/1/2017
        !!
        !! This subroutine writes the legacy vtk output file
        !!
        INTEGER(i4k) :: i, inputstat
        LOGICAL :: file_is_still_open

        INQUIRE(unit=newunit,opened=file_is_still_open)
        IF (.NOT. file_is_still_open) THEN
            !! For some reason, file was closed. Re-open the file to a new unit
            OPEN(newunit=newunit, file=vtkfilename, iostat=inputstat, status='REPLACE', form=form, position='APPEND')
        END IF

        IF (PRESENT(celldatasets)) THEN
            IF (.NOT. printed_cell_data_header) THEN
                WRITE(newunit,101) celldatasets(1)%attribute%nvals
            END IF
            DO i = 1, SIZE(celldatasets)
                CALL celldatasets(i)%attribute%write(newunit)   !! Write the cell data values
            END DO
        ELSE IF (PRESENT(celldata)) THEN
            IF (.NOT. printed_cell_data_header) THEN
                WRITE(newunit,101) celldata%nvals
                printed_cell_data_header = .TRUE.
            END IF
            CALL celldata%write(newunit)                        !! Write the cell data values
        END IF

        IF (PRESENT(pointdatasets)) THEN
            IF (.NOT. printed_point_data_header) THEN
                WRITE(newunit,102) pointdatasets(1)%attribute%nvals
            END IF
            DO I = 1, SIZE(pointdatasets)
                CALL pointdatasets(i)%attribute%write(newunit)
            END DO
        ELSE IF (PRESENT(pointdata)) THEN
            IF (.NOT. printed_point_data_header) THEN
                WRITE(newunit,102) pointdata%nvals
                printed_point_data_header = .TRUE.
            END IF

            CALL pointdata%write(newunit)                       !! Write the point data values
        END IF

101     FORMAT('CELL_DATA ',i0)
102     FORMAT('POINT_DATA ',i0)

        END PROCEDURE vtk_legacy_append

        MODULE PROCEDURE vtk_legacy_finalize
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 06/03/2019
        !!
        !! This subroutine is a finalizer for the vtk file write
        !!

        IF (finished) THEN
            IF (.NOT. file_was_already_open) THEN
                CLOSE(newunit)                                  !! Close the VTK file if file was not open prior to calling vtkmofo
            END IF
            printed_cell_data_header  = .FALSE.
            printed_point_data_header = .FALSE.
        END IF

        END PROCEDURE vtk_legacy_finalize

        MODULE PROCEDURE vtk_legacy_read
        USE Misc,     ONLY : def_len
        USE vtk_vars, ONLY : default_fn, default_title, vtkfilename, vtktitle, version
        USE XML,      ONLY : convert_string_to_format, ascii, binary, file_format, file_format_text
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 12/20/2017
        !!
        !! This subroutine reads the legacy vtk output file
        !!
        INTEGER(i4k) :: i, inputstat
        LOGICAL      :: file_is_open
        CHARACTER(LEN=:), ALLOCATABLE :: form, vtk_version
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
        ALLOCATE(file_format_text, source=TRIM(line))
        line = ''

        CLOSE(unit)                                        !! Close the file to re-open it in the proper format

        file_format = convert_string_to_format(file_format_text)
        SELECT CASE (file_format)
        CASE (ascii)
            ALLOCATE(form, source='formatted')
        CASE (binary)
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

100     FORMAT(a)

        END PROCEDURE vtk_legacy_read

        MODULE PROCEDURE vtk_serial_full_write
        USE vtk_datasets,    ONLY : struct_pts, struct_grid, rectlnr_grid, polygonal_data, unstruct_grid
        USE VTK_serial_file, ONLY : serial_file
        USE VTK_serial_Grid, ONLY : VTK_serial_RectilinearGrid_dt, VTK_serial_StructuredGrid_dt, &
          &                         VTK_serial_UnstructuredGrid_dt, VTK_serial_ImageData_dt
        USE VTK_vars,        ONLY : vtk_extension
        USE XML,             ONLY : file_format, file_format_text, convert_format_to_string, ascii, format_ascii
        USE Misc,            ONLY : trim_from_string
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 5/08/2019
        !!
        !! This subroutine writes the modern serial vtk output file
        !!

        ! Clear out any pre-existing data
        IF (ALLOCATED(vtkfilename))      DEALLOCATE(vtkfilename)
        IF (ALLOCATED(form))             DEALLOCATE(form)
        IF (ALLOCATED(file_format_text)) DEALLOCATE(file_format_text)

        IF (PRESENT(format)) THEN
            ALLOCATE(file_format_text, source=convert_format_to_string(format))
            file_format = format
        ELSE
            ALLOCATE(file_format_text,source=format_ascii)     !! Default to binary
            file_format = ascii
        END IF
        IF (PRESENT(filename)) THEN
            ALLOCATE(vtkfilename, source=trim_from_string(filename,vtk_extension))
                                                                !! Calling program provided a filename
        ELSE
            ALLOCATE(vtkfilename, source=default_fn)            !! Calling program did not provide a filename. Use default
        END IF

        IF (PRESENT(multiple_io)) THEN
            IF (multiple_io) THEN
                mio_filename: BLOCK
                    CHARACTER(LEN=8) :: fcnt_char = ''          !! File count character
                    CHARACTER(LEN=:), ALLOCATABLE :: base_fn    !! Base file name
                    WRITE (fcnt_char,FMT='(i8)') fcnt
                    ALLOCATE(base_fn, source=vtkfilename)
                    DEALLOCATE(vtkfilename)
                    ALLOCATE(vtkfilename, source=base_fn // "_" // TRIM(ADJUSTL(fcnt_char)))
                    fcnt = fcnt + 1                             !! Increase timestep file counter by 1
                END BLOCK mio_filename
            END IF
        END IF

        ALLOCATE(serial_file)

        SELECT TYPE (geometry)
        CLASS IS (struct_pts)
            ALLOCATE(VTK_serial_ImageData_dt::serial_file%vtk_dataset)
        CLASS IS (struct_grid)
            ALLOCATE(VTK_serial_StructuredGrid_dt::serial_file%vtk_dataset)
        CLASS IS (rectlnr_grid)
            ALLOCATE(VTK_serial_RectilinearGrid_dt::serial_file%vtk_dataset)
        CLASS IS (polygonal_data)
            ERROR STOP 'Procedure not yet implemented for: POLYGONAL GRID. Termination in subroutine: vtk_serial_full_write'
        CLASS IS (unstruct_grid)
            ALLOCATE(VTK_serial_UnstructuredGrid_dt::serial_file%vtk_dataset)
        CLASS DEFAULT
            ERROR STOP 'Unsupported geometry type. Termination in subroutine: vtk_serial_full_write'
        END SELECT

        CALL serial_file%vtk_dataset%set_grid(geometry)

        CALL serial_file%setup(filename=vtkfilename // TRIM(serial_file%vtk_dataset%file_extension))
        !! Append data
        CALL vtk_serial_append (celldata, pointdata, celldatasets, pointdatasets)
        !! Finalize the write
        IF (ANY([ PRESENT(celldatasets), PRESENT(celldata), PRESENT(pointdatasets), PRESENT(pointdata) ])) THEN
            CALL vtk_serial_finalize (finished=.TRUE.)          !! Full legacy write w/ data. Close file.
        ELSE
            CALL vtk_serial_finalize (finished=.FALSE.)         !! No data was provided, only geometry info. Do not close file.
        END IF

        END PROCEDURE vtk_serial_full_write

        MODULE PROCEDURE vtk_serial_append
        USE VTK_serial_file, ONLY : serial_file
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 06/24/2019
        !!
        !! This subroutine appends data to the legacy vtk output file
        !!

        IF (.NOT. ALLOCATED(serial_file%vtk_dataset%piece)) ALLOCATE(serial_file%vtk_dataset%piece)
        CALL serial_file%vtk_dataset%piece%add_data(celldata, pointdata, celldatasets, pointdatasets)

        END PROCEDURE vtk_serial_append

        MODULE PROCEDURE vtk_serial_finalize
        USE VTK_serial_file, ONLY : serial_file
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 06/24/2019
        !!
        !! This subroutine is a finalizer for the legacy vtk file write
        !!

        IF (finished) THEN
            CALL serial_file%vtk_dataset%finalize()
                  !! This should write everything inside of the piece
            CALL serial_file%add(serial_file%vtk_dataset)
            CALL serial_file%write()
            CALL serial_file%close_file()                    !! Close the VTK file
            CALL serial_file%me_deallocate()
            IF (ALLOCATED(serial_file)) DEALLOCATE(serial_file)
        END IF

        END PROCEDURE vtk_serial_finalize

END SUBMODULE vtk_io_implementation
