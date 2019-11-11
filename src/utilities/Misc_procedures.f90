SUBMODULE (Misc) Misc_implementation
    USE Precision, ONLY : i4k, i8k, r8k
    IMPLICIT NONE

    CONTAINS

        MODULE PROCEDURE interpret_string
        IMPLICIT NONE
        !! Interprets a string (typically read from an input file) into a user-defined # of character and/or integer inputs
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
            ALLOCATE(sep, source=separator)
        ELSE
            ALLOCATE(sep, source=' ')
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
            ALLOCATE(chars(1:SIZE(datatype)))
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
                ALLOCATE(chars(cnt%c)%text, source=char)
            END SELECT
            CALL reduce_string (string, sep)
            cnt%t = cnt%t + 1
        END DO

        line = string

        END PROCEDURE interpret_string

        MODULE PROCEDURE reduce_string
        IMPLICIT NONE

        IF (INDEX(string,sep) == 0) THEN
            string = ''
        ELSE
            string = ADJUSTL(string(INDEX(string,sep)+LEN(sep):))
        END IF

        END PROCEDURE reduce_string

        MODULE PROCEDURE get_string_char
        IMPLICIT NONE

        IF (INDEX(string,sep) == 0) THEN
            name = string(1:)                    !! Read to end of string
        ELSE
            name = string(1:INDEX(string,sep)-1) !! Read until sep is found
        END IF

        END PROCEDURE get_string_char

        MODULE PROCEDURE get_string_int
        IMPLICIT NONE

        CHARACTER(LEN=:), ALLOCATABLE :: text

        IF (INDEX(string,sep) == 0) THEN
            ALLOCATE(text, source=string(1:))                    !! Read to end of string
        ELSE
            ALLOCATE(text, source=string(1:INDEX(string,sep)-1)) !! Read until sep is found
        END IF
        READ(text,'(i8)') name                                   !! Store value

        END PROCEDURE get_string_int

        MODULE PROCEDURE get_string_real
        IMPLICIT NONE
        CHARACTER(LEN=:), ALLOCATABLE :: text

        IF (INDEX(string,sep) == 0) THEN
            ALLOCATE(text, source=string(1:))                    !! Read to end of string
        ELSE
            ALLOCATE(text, source=string(1:INDEX(string,sep)-1)) !! Read until sep is found
        END IF
        READ(text,'(es13.6)') name                               !! Store value

        END PROCEDURE get_string_real

        MODULE PROCEDURE convert_real32_to_string
        IMPLICIT NONE
        !! Converts a real to a character string
        CHARACTER(LEN=20) :: tmp_string = '                    '

        WRITE(tmp_string,*) var
        ALLOCATE(string,source=TRIM(ADJUSTL(tmp_string)))

        END PROCEDURE convert_real32_to_string

        MODULE PROCEDURE convert_real64_to_string
        IMPLICIT NONE
        !! Converts a real to a character string
        CHARACTER(LEN=30) :: tmp_string = '                              '

        WRITE(tmp_string,*) var
        ALLOCATE(string,source=TRIM(ADJUSTL(tmp_string)))

        END PROCEDURE convert_real64_to_string

        MODULE PROCEDURE convert_real64_array_to_string
        IMPLICIT NONE
        !! Converts a real to a character string
        INTEGER(i4k) :: i
        CHARACTER(LEN=:), ALLOCATABLE :: tmp_string

        DO i = 1, SIZE(var)
            ALLOCATE(tmp_string, source=convert_real64_to_string(var(i)))
            IF (.NOT. ALLOCATED(string)) THEN
                ALLOCATE(string,source=tmp_string)
            ELSE
                string = string // " " // tmp_string
            END IF
            DEALLOCATE(tmp_string)
        END DO

        END PROCEDURE convert_real64_array_to_string

        MODULE PROCEDURE convert_int32_to_string
        IMPLICIT NONE
        CHARACTER(LEN=20) :: tmp_string = '                    '

        WRITE(tmp_string,*) var
        ALLOCATE(string,source=TRIM(ADJUSTL(tmp_string)))

        END PROCEDURE convert_int32_to_string

        MODULE PROCEDURE convert_int64_to_string
        IMPLICIT NONE
        CHARACTER(LEN=30) :: tmp_string = '                              '

        WRITE(tmp_string,*) var
        ALLOCATE(string,source=TRIM(ADJUSTL(tmp_string)))

        END PROCEDURE convert_int64_to_string

        MODULE PROCEDURE convert_logical_to_string
        IMPLICIT NONE

        IF (var) THEN
            ALLOCATE(string,source='True')
        ELSE
            ALLOCATE(string,source='False')
        END IF

        END PROCEDURE convert_logical_to_string

        MODULE PROCEDURE to_uppercase
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 01/23/2019
        !!
        !! This function changes lowercase text in a string to uppercase text
        !!
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

        MODULE PROCEDURE to_lowercase
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 01/23/2019
        !!
        !! This function changes uppercase text in a string to lowercase text
        !!
        INTEGER(i4k) :: i, j
        CHARACTER(LEN=26), PARAMETER    :: CAPL = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
        CHARACTER(LEN=26), PARAMETER    :: LOWL = 'abcdefghijklmnopqrstuvwxyz'

        new_string = string(1:LEN_TRIM(string))

        DO i = 1, LEN_TRIM(string)
            j = INDEX(CAPL, string(i:i))
            IF (j > 0) THEN
                new_string(i:i) = LOWL(j:j)
            ELSE
                new_string(i:i) = string(i:i)
            END IF
        END DO

        END PROCEDURE to_lowercase

        MODULE PROCEDURE TRIM_FROM_STRING
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 11/06/2019
        !!
        !! This function trims <item> from a string
        !!
        INTEGER(i4k) :: start_len  !! Length to the start of where to trim string from
        INTEGER(i4k) :: string_len !! Length of the string
        INTEGER(i4k) :: item_len   !! Length of the item to trim
        LOGICAL :: search_by_case  !! Flag to determine whether consider case sensitivity in search

        IF (PRESENT(case_sensitive)) THEN
            search_by_case = case_sensitive
        ELSE
            search_by_case = .true.
        END IF

        IF (search_by_case) THEN
            start_len = INDEX(string,item,back=.true.)
        ELSE
            start_len = INDEX(to_uppercase(string),to_uppercase(item),back=.true.)
        END IF

        IF (start_len > 0) THEN
            item_len = LEN(item)
            string_len = LEN(string)
            IF (string_len == item_len) THEN
                new_string = ''
            ELSE
                new_string = string(1:start_len - 1)
                IF (LEN(new_string) + item_len < string_len) THEN
                    new_string = new_string // string(start_len + item_len:)
                END IF
            END IF
        ELSE
            new_string = string
        END IF

        END PROCEDURE TRIM_FROM_STRING

        MODULE PROCEDURE sleep_for
        IMPLICIT NONE
        !! author: Zaak Beekman, ParaTools
        !! date: 8/8/2018
        !!
        !! This performs a 'sleep' for a specified amount of time
        !!
        INTEGER(i4k), DIMENSION(8) :: time
        INTEGER(i8k) :: ms_t1, ms_t2, msecs_big

        CALL DATE_AND_TIME(values=time)

        ms_t1=(time(5)*3600+time(6)*60+time(7))*1000+time(8)
        msecs_big = msecs

        DO !! spin until elapsed time is greater than msecs
            CALL DATE_AND_TIME(values=time)
            ms_t2=(time(5)*3600+time(6)*60+time(7))*1000+time(8)
            IF ( ms_t2 - ms_t1 >= msecs_big ) EXIT
        END DO

        END PROCEDURE

END SUBMODULE Misc_implementation
