SUBMODULE (file_utility) file_utility_implementation
    USE Precision, ONLY : i4k, r8k
    IMPLICIT NONE
    !! author: Ian Porter
    !! date: 04/04/2018
    !!
    !! This module contains a derived type for file information
    !!
    CONTAINS

        MODULE PROCEDURE setup_file_information
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 04/16/2018
        !!
        !! Establishes the file information
        !!

        ALLOCATE(me%filename, source=TRIM(filename))
        IF (PRESENT(open_status)) THEN
            ALLOCATE(me%open_status, source=open_status)
        ELSE
            ALLOCATE(me%open_status, source='UNKNOWN')
        END IF
        IF (PRESENT(close_status)) THEN
            ALLOCATE(me%close_status, source=close_status)
        ELSE
            ALLOCATE(me%close_status, source='KEEP')
        END IF
        IF (PRESENT(form)) THEN
            ALLOCATE(me%form, source=form)
        ELSE
            ALLOCATE(me%form, source='UNKNOWN')
        END IF
        IF (PRESENT(access)) THEN
            ALLOCATE(me%access, source=access)
        ELSE
            ALLOCATE(me%access, source='UNKNOWN')
        END IF

        END PROCEDURE setup_file_information

        MODULE PROCEDURE check_if_exists
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 04/04/2018
        !!
        !! Checks to see if the file is open
        !!

        INQUIRE (file=me%filename, exist=file_exists)

        END PROCEDURE check_if_exists

        MODULE PROCEDURE check_if_open
        USE Misc, ONLY : TO_UPPERCASE
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 04/04/2018
        !!
        !! Checks to see if the file is open by file name
        !!
        CHARACTER(LEN=:), ALLOCATABLE :: check_by_type

        IF (PRESENT(check)) THEN
            ALLOCATE(check_by_type, source=TO_UPPERCASE(check))
        ELSE
            ALLOCATE(check_by_type, source='FILENAME')
        END IF

        SELECT CASE (check_by_type)
        CASE ('FILENAME')
            INQUIRE (file=me%filename, opened=is_open)
        CASE ('UNIT')
            INQUIRE (unit=me%unit, opened=is_open)
        CASE DEFAULT
            ERROR STOP 'Unrecognized check_by_type in module procedure check_if_open'
        END SELECT

        END PROCEDURE check_if_open

        MODULE PROCEDURE open_file
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 04/04/2018
        !!
        !! Opens the file if it already exists
        !!
        INTEGER(i4k) :: inputstat

        IF (.NOT. me%exists()) THEN
            ERROR STOP 'File does not exist. Execution terminated in Module: File_utility, Subroutine: open_file'
        END IF

        IF (.NOT. me%check_if_open(check='FILENAME')) THEN
            OPEN (newunit=me%unit, file=me%filename, iostat=inputstat, Status=me%open_status, Form=me%form)
            IF (InputStat > 0) CALL me%file_read_error (inputstat)
        ELSE
            !! File is already open. Rewind to start from the beginning
            REWIND (me%unit)
        END IF

        END PROCEDURE open_file

        MODULE PROCEDURE close_file
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 10/25/2018
        !!
        !! Closes the file
        !!

        IF (me%check_if_open()) THEN
            !! Close the file
            CLOSE(unit=me%unit,status=me%close_status)
        END IF

        END PROCEDURE close_file

        MODULE PROCEDURE make_file
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 10/26/2018
        !!
        !! Makes a new file
        !!
        INTEGER(i4k) :: inputstat

        IF (me%unit < 0) THEN
            me%unit = 0 !! Re-set this to a non-negative number for a gfortran-8.3 bug w/ newunit
        END IF

        OPEN (newunit=me%unit, file=me%filename, iostat=inputstat, Status='REPLACE', Form=me%form)

        END PROCEDURE make_file

        MODULE PROCEDURE file_read_error
        USE ISO_FORTRAN_ENV, ONLY : output_unit
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 04/04/2018
        !!
        !! Prints that a file i/o error as occurred
        !!
        WRITE(output_unit,100) me%unit, inputstat, me%filename, me%open_status, me%form, me%access
        ERROR STOP 'Error in opening file. Execution terminated in Subroutine: iofiles.'

100     FORMAT(/' iofiles: Error on trying to open unit ',i0,/, &
          &     '  Error message number = ',i0              ,/, &
          &     '  File name = ',a                           ,/, &
          &     '  Status    = ',a                           ,/, &
          &     '  Form      = ',a                           ,/, &
          &     '  Access    = ',a)

        END PROCEDURE file_read_error

        MODULE PROCEDURE wait_for_file
        USE Misc, ONLY : sleep_for
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 08/10/2018
        !!
        !! Waits for a file to exist
        !!

        check_if_exists: DO
            IF (me%exists()) THEN
                !! File exists. Now check to see if it is open
                IF (me%check_if_open()) THEN
                    !! It is still being written-to by another file
!                    CALL sleep_for(10)
                ELSE
                    CALL me%open_file()
!                    EXIT check_if_exists
                END IF
                EXIT check_if_exists
            END IF
            CALL sleep_for(10) !! Code waits 10 milliseconds before checking again
        END DO check_if_exists

        END PROCEDURE wait_for_file

        MODULE PROCEDURE get_unit
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 04/03/2019
        !!
        !! Function returns the unit of a file
        !!

        unit = me%unit

        END PROCEDURE get_unit

END SUBMODULE file_utility_implementation
