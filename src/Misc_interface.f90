MODULE Misc
    USE Precision
    IMPLICIT NONE
    !! author: Ian Porter
    !! date: 12/13/2017
    !!
    !! This module contains miscellaneous routines used to read/write to the .vtk file
    !!
    PRIVATE
    PUBLIC :: interpret_string, def_len, to_uppercase, to_lowercase, char_dt

    INTERFACE get_string_value
        PROCEDURE :: get_string_char, get_string_int, get_string_real
    END INTERFACE

    INTEGER(i4k), PARAMETER :: def_len = 1024          !! Default character length for each line in file

    TYPE char_dt
        !! Character string DT
        CHARACTER(LEN=:), ALLOCATABLE :: text
    END TYPE char_dt

    INTERFACE

        MODULE SUBROUTINE interpret_string (line, datatype, ignore, separator, reals, ints, chars)
        !!
        !! Interprets a string (typically read from an input file) into a user-defined # of character and/or integer inputs
        CHARACTER(LEN=*), INTENT(INOUT) :: line
        CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: ignore, separator
        CHARACTER(LEN=1), DIMENSION(:), INTENT(IN) :: datatype
        INTEGER(i4k),     DIMENSION(:), ALLOCATABLE, OPTIONAL :: ints
        REAL(r8k),        DIMENSION(:), ALLOCATABLE, OPTIONAL :: reals
        TYPE(char_dt),    DIMENSION(:), ALLOCATABLE, OPTIONAL :: chars

        END SUBROUTINE interpret_string

        MODULE SUBROUTINE reduce_string (string, sep)
        CHARACTER(LEN=:), ALLOCATABLE, INTENT(INOUT) :: string
        CHARACTER(LEN=*), INTENT(IN)  :: sep

        END SUBROUTINE reduce_string

        MODULE SUBROUTINE get_string_char (string, sep, name)
        CHARACTER(LEN=*), INTENT(IN)  :: string, sep
        CHARACTER(LEN=:), ALLOCATABLE, INTENT(OUT) :: name

        END SUBROUTINE get_string_char

        MODULE SUBROUTINE get_string_int (string, sep, name)
        CHARACTER(LEN=*), INTENT(IN)  :: string, sep
        INTEGER(i4k),     INTENT(OUT) :: name

        END SUBROUTINE get_string_int

        MODULE SUBROUTINE get_string_real (string, sep, name)
        CHARACTER(LEN=*), INTENT(IN)  :: string, sep
        REAL(r8k),        INTENT(OUT) :: name

        END SUBROUTINE get_string_real

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

    END INTERFACE

END MODULE Misc
