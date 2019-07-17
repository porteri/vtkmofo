MODULE Misc
    USE Precision, ONLY : i4k, i8k, r4k, r8k
    IMPLICIT NONE
    !! author: Ian Porter
    !! date: 12/13/2017
    !!
    !! This module contains miscellaneous routines used to read/write to the .vtk file
    !!
    PRIVATE
    PUBLIC :: interpret_string, def_len, to_uppercase, to_lowercase, char_dt, sleep_for, convert_to_string

    INTERFACE get_string_value
        PROCEDURE :: get_string_char
        PROCEDURE :: get_string_int
        PROCEDURE :: get_string_real
    END INTERFACE

    INTERFACE convert_to_string
        PROCEDURE :: convert_real32_to_string
        PROCEDURE :: convert_real64_to_string
        PROCEDURE :: convert_int32_to_string
        PROCEDURE :: convert_int64_to_string
    END INTERFACE convert_to_string

    INTEGER(i4k), PARAMETER :: def_len = 1024          !! Default character length for each line in file

    TYPE char_dt
        !! Character string DT
        CHARACTER(LEN=:), ALLOCATABLE :: text
    END TYPE char_dt

    INTERFACE

        MODULE SUBROUTINE interpret_string (line, datatype, ignore, separator, reals, ints, chars)
        IMPLICIT NONE
        !! Interprets a string (typically read from an input file) into a user-defined # of character and/or integer inputs
        CHARACTER(LEN=*), INTENT(INOUT) :: line
        CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: ignore, separator
        CHARACTER(LEN=1), DIMENSION(:), INTENT(IN) :: datatype
        INTEGER(i4k),     DIMENSION(:), ALLOCATABLE, OPTIONAL :: ints
        REAL(r8k),        DIMENSION(:), ALLOCATABLE, OPTIONAL :: reals
        TYPE(char_dt),    DIMENSION(:), ALLOCATABLE, OPTIONAL :: chars

        END SUBROUTINE interpret_string

        MODULE SUBROUTINE reduce_string (string, sep)
        IMPLICIT NONE
        CHARACTER(LEN=:), ALLOCATABLE, INTENT(INOUT) :: string
        CHARACTER(LEN=*), INTENT(IN)  :: sep

        END SUBROUTINE reduce_string

        MODULE SUBROUTINE get_string_char (string, sep, name)
        IMPLICIT NONE
        CHARACTER(LEN=*), INTENT(IN)  :: string, sep
        CHARACTER(LEN=:), ALLOCATABLE, INTENT(OUT) :: name

        END SUBROUTINE get_string_char

        MODULE SUBROUTINE get_string_int (string, sep, name)
        IMPLICIT NONE
        CHARACTER(LEN=*), INTENT(IN)  :: string, sep
        INTEGER(i4k),     INTENT(OUT) :: name

        END SUBROUTINE get_string_int

        MODULE SUBROUTINE get_string_real (string, sep, name)
        IMPLICIT NONE
        CHARACTER(LEN=*), INTENT(IN)  :: string, sep
        REAL(r8k),        INTENT(OUT) :: name

        END SUBROUTINE get_string_real

        MODULE FUNCTION convert_real32_to_string (var) RESULT (string)
        IMPLICIT NONE
        !! Converts a real32 to a character string
        REAL(r4k),        INTENT(IN)  :: var      !! Real variable
        CHARACTER(LEN=:), ALLOCATABLE :: string   !! Character string
        END FUNCTION convert_real32_to_string

        MODULE FUNCTION convert_real64_to_string (var) RESULT (string)
        IMPLICIT NONE
        !! Converts a real64 to a character string
        REAL(r8k),        INTENT(IN)  :: var      !! Real variable
        CHARACTER(LEN=:), ALLOCATABLE :: string   !! Character string
        END FUNCTION convert_real64_to_string

        MODULE FUNCTION convert_int32_to_string (var) RESULT (string)
        IMPLICIT NONE
        !! Converts a real32 to a character string
        INTEGER(i4k),     INTENT(IN)  :: var      !! Integer variable
        CHARACTER(LEN=:), ALLOCATABLE :: string   !! Character string
        END FUNCTION convert_int32_to_string

        MODULE FUNCTION convert_int64_to_string (var) RESULT (string)
        IMPLICIT NONE
        !! Converts a real64 to a character string
        INTEGER(i8k),     INTENT(IN)  :: var      !! Integer variable
        CHARACTER(LEN=:), ALLOCATABLE :: string   !! Character string
        END FUNCTION convert_int64_to_string

        PURE MODULE FUNCTION to_uppercase (string) RESULT (new_string)
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 01/23/2019
        !!
        !! This function changes lowercase text in a string to uppercase text
        !!
        CHARACTER(LEN=*), INTENT(IN)  :: string
        CHARACTER(LEN=:), ALLOCATABLE :: new_string

        END FUNCTION to_uppercase

        PURE MODULE FUNCTION to_lowercase (string) RESULT (new_string)
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 01/23/2019
        !!
        !! This function changes uppercase text in a string to lowercase text
        !!
        CHARACTER(LEN=*), INTENT(IN)  :: string
        CHARACTER(LEN=:), ALLOCATABLE :: new_string

        END FUNCTION to_lowercase

        MODULE SUBROUTINE sleep_for (msecs)
        IMPLICIT NONE
        !! author: Zaak Beekman, ParaTools
        !! date: 8/8/2018
        !!
        !! This performs a 'sleep' for a specified amount of time
        !!
        INTEGER(i4k), INTENT(IN) :: msecs  !! # of milliseconds to sleep for

        END SUBROUTINE

    END INTERFACE

END MODULE Misc
