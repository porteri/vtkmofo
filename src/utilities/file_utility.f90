MODULE File_utility
    USE Precision, ONLY : i4k, r8k
    IMPLICIT NONE
    !! author: Ian Porter
    !! date: 04/04/2018
    !!
    !! This module contains a derived type for file information
    !!
    PRIVATE
    PUBLIC :: file_data_structure, is_little_endian

    TYPE file_data_structure
        !! File data derived type
        INTEGER(i4k) :: unit
        CHARACTER(LEN=:), ALLOCATABLE :: filename
        CHARACTER(LEN=:), ALLOCATABLE :: open_status
        CHARACTER(LEN=:), ALLOCATABLE :: close_status
        CHARACTER(LEN=:), ALLOCATABLE :: form
        CHARACTER(LEN=:), ALLOCATABLE :: access
    CONTAINS
        PROCEDURE, PUBLIC  :: setup_file_information
        GENERIC,   PUBLIC  :: setup => setup_file_information
        PROCEDURE, PRIVATE :: check_if_exists
        GENERIC,   PUBLIC  :: exists => check_if_exists
        PROCEDURE, PRIVATE :: check_if_open
        PROCEDURE, PUBLIC  :: open_file
        PROCEDURE, PUBLIC  :: close_file
        PROCEDURE, PUBLIC  :: make_file
        PROCEDURE, PUBLIC  :: file_read_error
        PROCEDURE, PUBLIC  :: wait_for_file
        PROCEDURE, PUBLIC  :: get_unit
        PROCEDURE, PUBLIC, NOPASS :: is_little_endian
    END TYPE file_data_structure

    INTERFACE

        MODULE SUBROUTINE setup_file_information (me, filename, open_status, close_status, form, access)
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 04/16/2018
        !!
        !! Establishes the file information
        !!
        CLASS(file_data_structure), INTENT(INOUT) :: me                  !! DT
        CHARACTER(LEN=*),           INTENT(IN)    :: filename            !! File name
        CHARACTER(LEN=*),           INTENT(IN), OPTIONAL :: open_status  !! File open status
        CHARACTER(LEN=*),           INTENT(IN), OPTIONAL :: close_status !! File close status
        CHARACTER(LEN=*),           INTENT(IN), OPTIONAL :: form         !! File format (formatted or unformatted)
        CHARACTER(LEN=*),           INTENT(IN), OPTIONAL :: access       !! File access type

        END SUBROUTINE setup_file_information

        MODULE FUNCTION check_if_exists (me) RESULT (file_exists)
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 04/04/2018
        !!
        !! Checks to see if the file exists
        !!
        CLASS(file_data_structure), INTENT(IN) :: me           !! DT
        LOGICAL                                :: file_exists  !! Determins if file exists

        END FUNCTION check_if_exists

        MODULE FUNCTION check_if_open (me, check) RESULT (is_open)
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 04/04/2018
        !!
        !! Checks to see if the file is open
        !!
        CLASS(file_data_structure), INTENT(IN)           :: me       !! DT
        CHARACTER(LEN=*),           INTENT(IN), OPTIONAL :: check    !! Type of check (filename or unit)
        LOGICAL                                          :: is_open  !! Determins if file is open

        END FUNCTION check_if_open

        MODULE SUBROUTINE open_file (me)
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 04/04/2018
        !!
        !! Opens the file
        !!
        CLASS(file_data_structure), INTENT(INOUT) :: me   !! DT

        END SUBROUTINE open_file

        MODULE SUBROUTINE close_file (me)
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 10/25/2018
        !!
        !! Closes the file
        !!
        CLASS(file_data_structure), INTENT(IN) :: me   !! DT

        END SUBROUTINE close_file

        MODULE SUBROUTINE make_file (me)
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 10/26/2018
        !!
        !! Makes a new file
        !!
        CLASS(file_data_structure), INTENT(INOUT) :: me   !! DT

        END SUBROUTINE make_file

        MODULE SUBROUTINE file_read_error (me, inputstat)
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 04/04/2018
        !!
        !! Prints that a file i/o error as occurred
        !!
        CLASS(file_data_structure), INTENT(IN) :: me          !! DT
        INTEGER(i4k),               INTENT(IN) :: inputstat   !! File read error #

        END SUBROUTINE file_read_error

        MODULE SUBROUTINE wait_for_file (me)
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 04/04/2018
        !!
        !! Waits for a file to exist
        !!
        CLASS(file_data_structure), INTENT(INOUT) :: me   !! DT

        END SUBROUTINE wait_for_file

        MODULE FUNCTION get_unit (me) RESULT (unit)
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 04/03/2019
        !!
        !! Function returns the unit of a file
        !!
        CLASS(file_data_structure), INTENT(IN) :: me   !! DT
        INTEGER :: unit                                !! File unit #

        END FUNCTION get_unit

        PURE MODULE FUNCTION is_little_endian() RESULT (is_little)
        !! Checks the type of bit ordering to determine if the architecture is little endian
        LOGICAL :: is_little !! Flag to determine if little endian

        END FUNCTION is_little_endian

    END INTERFACE

END MODULE File_utility
