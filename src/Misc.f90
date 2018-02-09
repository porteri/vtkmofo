MODULE Misc
    USE Precision
    IMPLICIT NONE

    PRIVATE
    PUBLIC :: interpret_string, def_len, to_uppercase

    INTERFACE get_string_value
        PROCEDURE :: get_string_char, get_string_int, get_string_real
    END INTERFACE

    INTEGER(i4k), PARAMETER :: def_len = 1000          !! Default character length for each line in file

    CONTAINS
        SUBROUTINE interpret_string (line, datatype, ignore, separator, reals, ints, chars)
        !>@brief
        !> Interprets a string (typically read from an input file) into a user-defined # of character and/or integer inputs
        INTEGER(i4k) :: i
        CHARACTER(LEN=*), INTENT(INOUT) :: line
        CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: ignore, separator
        CHARACTER(LEN=:), ALLOCATABLE :: string, sep, char
        CHARACTER(LEN=1), DIMENSION(:), INTENT(IN) :: datatype
        INTEGER(i4k),     DIMENSION(:), ALLOCATABLE, OPTIONAL :: ints
        REAL(r8k),        DIMENSION(:), ALLOCATABLE, OPTIONAL :: reals
        CHARACTER(LEN=:), DIMENSION(:), ALLOCATABLE, OPTIONAL :: chars
        TYPE :: counter
            INTEGER(i4k) :: t = 0, i = 0, r = 0, c = 0
        END TYPE counter
        TYPE (counter) :: cnt

        IF (PRESENT(ignore)) THEN
            string = TRIM(ADJUSTL(line(INDEX(line,ignore)+LEN(ignore):)))
        ELSE
            string = TRIM(ADJUSTL(line))
        END IF
        IF (PRESENT(separator)) THEN
            sep = separator
        ELSE
            sep = ' '
        END IF
        IF (PRESENT(ints)) THEN
            IF (ALLOCATED(ints)) DEALLOCATE(ints)
            ALLOCATE(ints(1:SIZE(datatype)),source=0_i4k)
        END IF
        IF (PRESENT(reals)) THEN
            IF (ALLOCATED(reals)) DEALLOCATE(reals)
            ALLOCATE(reals(1:SIZE(datatype)),source=0.0_r8k)
        END IF
        IF (PRESENT(chars)) THEN
            IF (ALLOCATED(chars)) DEALLOCATE(chars)
            ALLOCATE(chars(1:SIZE(datatype)),source=string)
        END IF

        DO i = 1, SIZE(datatype)
            SELECT CASE (datatype(i))
            CASE ('I', 'i')
                !! Integer
                cnt%i = cnt%i + 1
                CALL get_string_value (string, sep, ints(cnt%i))
            CASE ('R', 'r')
                !! Real
                cnt%r = cnt%r + 1
                CALL get_string_value (string, sep, reals(cnt%r))
            CASE ('C', 'c')
                !! Character
                cnt%c = cnt%c + 1
                CALL get_string_value (string, sep, char)
                chars(cnt%c) = char
            END SELECT
            CALL reduce_string (string, sep)
            cnt%t = cnt%t + 1
        END DO

        line = string

        END SUBROUTINE interpret_string

        SUBROUTINE reduce_string (string, sep)
        CHARACTER(LEN=:), ALLOCATABLE, INTENT(INOUT) :: string
        CHARACTER(LEN=*), INTENT(IN)  :: sep

        IF (INDEX(string,sep) == 0) THEN
            string = ''
        ELSE
            string = ADJUSTL(string(INDEX(string,sep)+LEN(sep):))
        END IF

        END SUBROUTINE reduce_string

        SUBROUTINE get_string_char (string, sep, name)
        CHARACTER(LEN=*), INTENT(IN)  :: string, sep
        CHARACTER(LEN=:), ALLOCATABLE, INTENT(OUT) :: name

        IF (INDEX(string,sep) == 0) THEN
            name = string(1:)                    !! Read to end of string
        ELSE
            name = string(1:INDEX(string,sep))   !! Read until sep is found
        END IF

        END SUBROUTINE get_string_char

        SUBROUTINE get_string_int (string, sep, name)
        CHARACTER(LEN=*), INTENT(IN)  :: string, sep
        INTEGER(i4k),     INTENT(OUT) :: name
        CHARACTER(LEN=:), ALLOCATABLE :: text

        IF (INDEX(string,sep) == 0) THEN
            text = string(1:)                    !! Read to end of string
        ELSE
            text = string(1:INDEX(string,sep))   !! Read until sep is found
        END IF
        READ(text,'(i8)') name                   !! Store value

        END SUBROUTINE get_string_int

        SUBROUTINE get_string_real (string, sep, name)
        CHARACTER(LEN=*), INTENT(IN)  :: string, sep
        REAL(r8k),        INTENT(OUT) :: name
        CHARACTER(LEN=:), ALLOCATABLE :: text

        IF (INDEX(string,sep) == 0) THEN
            text = string(1:)                    !! Read to end of string
        ELSE
            text = string(1:INDEX(string,sep))   !! Read until sep is found
        END IF
        READ(text,'(es13.6)') name               !! Store value

        END SUBROUTINE get_string_real

        PURE FUNCTION to_uppercase (string) RESULT (new_string)
        USE Precision
        IMPLICIT NONE
        !>@brief
        !> This function changes lowercase text in a string to uppercase text
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 1/30/2015
        INTEGER(i4k) :: i, j
        CHARACTER(LEN=*), INTENT(IN)    :: string
        CHARACTER(LEN=LEN_TRIM(string)) :: new_string
        CHARACTER(LEN=26), PARAMETER    :: CAPL = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
        CHARACTER(LEN=26), PARAMETER    :: LOWL = 'abcdefghijklmnopqrstuvwxyz'

        new_string = ''

        DO i = 1, LEN_TRIM(string)
            j = INDEX(LOWL, string(i:i))
            IF (j > 0) THEN
                new_string(i:i) = CAPL(j:j)
            ELSE
                new_string(i:i) = string(i:i)
            END IF
        END DO

        END FUNCTION to_uppercase
END MODULE Misc
