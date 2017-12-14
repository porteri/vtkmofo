MODULE Misc
    USE Kinds
    IMPLICIT NONE

    PRIVATE
    PUBLIC :: interpret_string

    INTERFACE get_len
        PROCEDURE :: get_len_int!, get_len_real
    END INTERFACE

    INTERFACE get_string_value
        PROCEDURE :: get_string_char, get_string_int, get_string_real
    END INTERFACE

    CONTAINS
        SUBROUTINE get_len_int (len_size, intval)
        INTEGER(i4k), INTENT(IN)  :: intval
        INTEGER(i4k), INTENT(OUT) :: len_size

        IF      (intval < 1e1) THEN
            len_size = 1
        ELSE IF (intval < 1e2) THEN
            len_size = 2
        ELSE IF (intval < 1e3) THEN
            len_size = 3
        ELSE IF (intval < 1e4) THEN
            len_size = 4
        ELSE IF (intval < 1e5) THEN
            len_size = 5
        ELSE IF (intval < 1e6) THEN
            len_size = 6
        ELSE IF (intval < 1e7) THEN
            len_size = 7
        ELSE
            len_size = 8
        END IF

        END SUBROUTINE get_len_int

        SUBROUTINE interpret_string (line, datatype, ignore, separator, reals, ints, chars)
        !>@brief
        !> Interprets a string (typically read from an input file) into a user-defined # of character and/or integer inputs
        INTEGER(i4k) :: i
        LOGICAL :: end_of_file
        CHARACTER(LEN=*), INTENT(IN) :: line
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
            string = TRIM(line(INDEX(line,ignore)+LEN(ignore):))
        ELSE
            string = TRIM(line)
        END IF
        WRITE(*,*) 'string=',string

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
          WRITE(*,*) 'I=',i
          WRITE(*,*) 'string=',string
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

        END SUBROUTINE interpret_string

        SUBROUTINE reduce_string (string, sep)
        CHARACTER(LEN=:), ALLOCATABLE, INTENT(INOUT) :: string
        CHARACTER(LEN=*), INTENT(IN)  :: sep

        string = string(INDEX(string,sep)+LEN(sep):)

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
        WRITE(*,*) 'text=',text
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
        READ(text,'(es12.5)') name               !! Store value

        END SUBROUTINE get_string_real
    END MODULE Misc
