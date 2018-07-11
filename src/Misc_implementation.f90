SUBMODULE (Misc) Misc_implementation

    CONTAINS

        MODULE PROCEDURE interpret_string
        !>@brief
        !> Interprets a string (typically read from an input file) into a user-defined # of character and/or integer inputs
        INTEGER(i4k) :: i
        CHARACTER(LEN=:), ALLOCATABLE :: string, sep, char
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

        END PROCEDURE interpret_string

        MODULE PROCEDURE reduce_string

        IF (INDEX(string,sep) == 0) THEN
            string = ''
        ELSE
            string = ADJUSTL(string(INDEX(string,sep)+LEN(sep):))
        END IF

        END PROCEDURE reduce_string

        MODULE PROCEDURE get_string_char

        IF (INDEX(string,sep) == 0) THEN
            name = string(1:)                    !! Read to end of string
        ELSE
            name = string(1:INDEX(string,sep)-1) !! Read until sep is found
        END IF

        END PROCEDURE get_string_char

        MODULE PROCEDURE get_string_int
        CHARACTER(LEN=:), ALLOCATABLE :: text

        IF (INDEX(string,sep) == 0) THEN
            text = string(1:)                    !! Read to end of string
        ELSE
            text = string(1:INDEX(string,sep)-1) !! Read until sep is found
        END IF
        READ(text,'(i8)') name                   !! Store value

        END PROCEDURE get_string_int

        MODULE PROCEDURE get_string_real
        CHARACTER(LEN=:), ALLOCATABLE :: text

        IF (INDEX(string,sep) == 0) THEN
            text = string(1:)                    !! Read to end of string
        ELSE
            text = string(1:INDEX(string,sep)-1) !! Read until sep is found
        END IF
        READ(text,'(es13.6)') name               !! Store value

        END PROCEDURE get_string_real

        MODULE PROCEDURE to_uppercase
        INTEGER(i4k) :: i, j
        CHARACTER(LEN=26), PARAMETER    :: CAPL = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
        CHARACTER(LEN=26), PARAMETER    :: LOWL = 'abcdefghijklmnopqrstuvwxyz'

        new_string = string(1:LEN_TRIM(string))

        DO i = 1, LEN_TRIM(string)
            j = INDEX(LOWL, string(i:i))
            IF (j > 0) THEN
                new_string(i:i) = CAPL(j:j)
            ELSE
                new_string(i:i) = string(i:i)
            END IF
        END DO

        END PROCEDURE to_uppercase

END SUBMODULE Misc_implementation
