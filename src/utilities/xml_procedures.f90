SUBMODULE (XML) XML_implementation
    USE Precision, ONLY : i4k
    IMPLICIT NONE
    !! author: Ian Porter
    !! date: 04/02/2019
    !!
    !! This implements the simple xml format writer
    !!
    INTEGER, PARAMETER :: def_offset = 4          !! Default # of leading spaces
    CHARACTER(LEN=:), ALLOCATABLE :: prior_offset !! Number of leading spaces for prior XML block
    CHARACTER(LEN=*), PARAMETER :: version = '<?xml version="1.0" encoding="UTF-8"?>'
    TYPE(xml_element_dt), DIMENSION(:), ALLOCATABLE, SAVE :: gcc_bug_tmp_element_dt

    CONTAINS

        MODULE PROCEDURE element_setup
        IMPLICIT NONE
        !! This sets up the information needed to define the XML element block
        INTEGER(i4k) :: i, my_offset

        CALL me%deallocate()

        ALLOCATE(me%name,source=name)
        IF (PRESENT(string)) THEN
            IF (LEN_TRIM(string) == 0) THEN
                ALLOCATE(me%additional_data,source='')
            ELSE
                IF (string(1:1) == " ") THEN
                    !! Don't add an extra space between the name and the string
                    ALLOCATE(me%additional_data, source=string)
                ELSE
                    ALLOCATE(me%additional_data, source=" " // string)
                END IF
            END IF
        END IF

        my_offset = def_offset
        IF (PRESENT(offset)) THEN
            IF (offset >= 0) my_offset = offset
        END IF

        DO i = 0, my_offset
            IF (i == 0) THEN
                ALLOCATE(me%offset,source='')
            ELSE
                me%offset = me%offset // ' '
            END IF
        END DO

        END PROCEDURE element_setup

        MODULE PROCEDURE element_begin
        IMPLICIT NONE
        !! This begins an xml element block
        CHARACTER(LEN=:), ALLOCATABLE :: tmp_offset

        SELECT CASE (file_format)
        CASE (ascii)
            WRITE(unit,'(a)',advance='no') prior_offset // '<' // me%name // me%additional_data // '>' // new_line('a')
        CASE (binary)
            WRITE(unit) '<' // me%name // me%additional_data // '>' // new_line('a')
        END SELECT

        ALLOCATE(tmp_offset,source=prior_offset // me%offset)   !! Set the new offset length
        CALL MOVE_ALLOC(tmp_offset,prior_offset)

        END PROCEDURE element_begin

        MODULE PROCEDURE element_add_real32
        USE Misc, ONLY : convert_to_string
        IMPLICIT NONE
        !! This adds data inside of an xml element block
        INTEGER(i4k) :: i
        TYPE(real32_dt) :: real32
        TYPE(string_dt), DIMENSION(:), ALLOCATABLE :: tmp_string_dt
        TYPE(real32_dt), DIMENSION(:), ALLOCATABLE :: tmp_real32_dt
        CHARACTER(LEN=:), ALLOCATABLE :: string

        SELECT CASE (file_format)
        CASE (binary)
            IF (.NOT. ALLOCATED(me%real32)) THEN
                ALLOCATE(me%real32(0))
            END IF
            ALLOCATE(real32%val, source=var)
            ALLOCATE(tmp_real32_dt, source = [me%real32, real32])
            CALL MOVE_ALLOC(tmp_real32_dt, me%real32)

        CASE (ascii)
            IF (.NOT. ALLOCATED(me%string)) THEN
                ALLOCATE(me%string(0))
            END IF

            ALLOCATE(tmp_string_dt(1:SIZE(me%string)+1))
            tmp_string_dt(1:SIZE(me%string)) = me%string
            CALL MOVE_ALLOC(tmp_string_dt, me%string)

            DO i = 1, SIZE(var)
                IF (i == 1) THEN
                    ALLOCATE(string, source=convert_to_string(var(i)))
                ELSE
                    string = string // " " // convert_to_string(var(i))
                END IF
            END DO

            ASSOCIATE (my_entry => UBOUND(me%string,DIM=1))
                ALLOCATE(me%string(my_entry)%text,source= string // new_line('a'))
            END ASSOCIATE
        END SELECT

        END PROCEDURE element_add_real32

        MODULE PROCEDURE element_add_real64
        USE Misc, ONLY : convert_to_string
        IMPLICIT NONE
        !! This adds data inside of an xml element block
        TYPE(real64_dt) :: real64
        TYPE(string_dt), DIMENSION(:), ALLOCATABLE :: tmp_string_dt
        TYPE(real64_dt), DIMENSION(:), ALLOCATABLE :: tmp_real64_dt

        SELECT CASE (file_format)
        CASE (binary)
            IF (.NOT. ALLOCATED(me%real64)) THEN
                ALLOCATE(me%real64(0))
            END IF
            ALLOCATE(real64%val, source=var)
            ALLOCATE(tmp_real64_dt, source = [me%real64, real64])
            CALL MOVE_ALLOC(tmp_real64_dt, me%real64)
        CASE (ascii)
            IF (.NOT. ALLOCATED(me%string)) THEN
                ALLOCATE(me%string(0))
            END IF

            ALLOCATE(tmp_string_dt(1:SIZE(me%string)+1))
            tmp_string_dt(1:SIZE(me%string)) = me%string
            CALL MOVE_ALLOC(tmp_string_dt, me%string)

            ASSOCIATE (my_entry => UBOUND(me%string,DIM=1))
                ALLOCATE(me%string(my_entry)%text,source=convert_to_string(var) // new_line('a'))
            END ASSOCIATE
        END SELECT

        END PROCEDURE element_add_real64

        MODULE PROCEDURE element_add_int32
        USE Misc, ONLY : convert_to_string
        IMPLICIT NONE
        !! This adds data inside of an xml element block
        INTEGER(i4k) :: i
        TYPE(string_dt), DIMENSION(:), ALLOCATABLE :: tmp_string_dt
        CHARACTER(LEN=:), ALLOCATABLE :: string

        IF (.NOT. ALLOCATED(me%string)) THEN
            ALLOCATE(me%string(0))
        END IF

        ALLOCATE(tmp_string_dt(1:SIZE(me%string)+1))
        tmp_string_dt(1:SIZE(me%string)) = me%string
        CALL MOVE_ALLOC(tmp_string_dt, me%string)

        DO i = 1, SIZE(var)
            IF (i == 1) THEN
                ALLOCATE(string, source=convert_to_string(var(i)))
            ELSE
                string = string // " " // convert_to_string(var(i))
            END IF
        END DO

        ASSOCIATE (my_entry => UBOUND(me%string,DIM=1))
            ALLOCATE(me%string(my_entry)%text,source= string // new_line('a'))
        END ASSOCIATE

        END PROCEDURE element_add_int32

        MODULE PROCEDURE element_add_int64
        USE Misc, ONLY : convert_to_string
        IMPLICIT NONE
        !! This adds data inside of an xml element block
        INTEGER(i4k) :: i
        TYPE(string_dt), DIMENSION(:), ALLOCATABLE :: tmp_string_dt
        CHARACTER(LEN=:), ALLOCATABLE :: string

        IF (.NOT. ALLOCATED(me%string)) THEN
            ALLOCATE(me%string(0))
        END IF

        ALLOCATE(tmp_string_dt(1:SIZE(me%string)+1))
        tmp_string_dt(1:SIZE(me%string)) = me%string
        CALL MOVE_ALLOC(tmp_string_dt, me%string)

        DO i = 1, SIZE(var)
            IF (i == 1) THEN
                ALLOCATE(string, source=convert_to_string(var(i)))
            ELSE
                string = string // " " // convert_to_string(var(i))
            END IF
        END DO

        ASSOCIATE (my_entry => UBOUND(me%string,DIM=1))
            ALLOCATE(me%string(my_entry)%text,source= string // new_line('a'))
        END ASSOCIATE

        END PROCEDURE element_add_int64

        MODULE PROCEDURE element_add_logical
        USE Misc, ONLY : convert_to_string
        IMPLICIT NONE
        !! This adds data inside of an xml element block
        INTEGER(i4k) :: i
        TYPE(string_dt), DIMENSION(:), ALLOCATABLE :: tmp_string_dt
        CHARACTER(LEN=:), ALLOCATABLE :: string

        IF (.NOT. ALLOCATED(me%string)) THEN
            ALLOCATE(me%string(0))
        END IF

        ALLOCATE(tmp_string_dt(1:SIZE(me%string)+1))
        tmp_string_dt(1:SIZE(me%string)) = me%string
        CALL MOVE_ALLOC(tmp_string_dt, me%string)

        DO i = 1, SIZE(var)
            IF (i == 1) THEN
                ALLOCATE(string, source=convert_to_string(var(i)))
            ELSE
                string = string // " " // convert_to_string(var(i))
            END IF
        END DO

        ASSOCIATE (my_entry => UBOUND(me%string,DIM=1))
            ALLOCATE(me%string(my_entry)%text,source= string // new_line('a'))
        END ASSOCIATE

        END PROCEDURE element_add_logical

        MODULE PROCEDURE element_add_string
        IMPLICIT NONE
        !! This adds data inside of an xml element block
        LOGICAL :: add_quotes
        TYPE(string_dt), DIMENSION(:), ALLOCATABLE :: tmp_string_dt

        IF (PRESENT(quotes)) THEN
            add_quotes = quotes
        ELSE
            add_quotes = .TRUE.  !! By default, add quotation marks around a string
        END IF

        IF (.NOT. ALLOCATED(me%string)) THEN
            ALLOCATE(me%string(0))
        END IF

        ALLOCATE(tmp_string_dt(1:SIZE(me%string)+1))
        tmp_string_dt(1:SIZE(me%string)) = me%string
        CALL MOVE_ALLOC(tmp_string_dt, me%string)

        ASSOCIATE (my_entry => UBOUND(me%string,DIM=1))
            IF (add_quotes) THEN
                ALLOCATE(me%string(my_entry)%text,source='"' // string // '"' // new_line('a'))
            ELSE
                ALLOCATE(me%string(my_entry)%text,source= string // new_line('a'))
            END IF
        END ASSOCIATE

        END PROCEDURE element_add_string

        MODULE PROCEDURE element_add_element
        IMPLICIT NONE
        !! This adds an element inside of an xml element block
        TYPE(xml_element_dt), DIMENSION(:), ALLOCATABLE :: tmp_element_dt
        !! This is how this routine should work (and does work w/ Intel)
        !        IF (.NOT. ALLOCATED(me%element)) THEN
        !            ALLOCATE(me%element(1),source=element)
        !        ELSE
        !            ALLOCATE(tmp_element_dt,source=[ me%element(:), element ]) ! This segfaults at runtime
        !            CALL MOVE_ALLOC(tmp_element_dt, me%element)
        !        END IF
        !! This is a temporary work around
        IF (.NOT. ALLOCATED(me%element)) THEN
            SELECT TYPE (element)
            CLASS IS (xml_element_dt)
                CALL gcc_bug_workaround_allocate(me%element, element)
            END SELECT
        ELSE
            SELECT TYPE (element)
            CLASS IS (xml_element_dt)
                CALL gcc_bug_workaround_allocate(tmp_element_dt, oldfoo=me%element)
                CALL gcc_bug_workaround_allocate(me%element, element, tmp_element_dt)
            END SELECT
        END IF
        CALL gcc_bug_workaround_deallocate (tmp_element_dt)

        END PROCEDURE element_add_element

        MODULE PROCEDURE element_end
        IMPLICIT NONE
        !! This ends an XML element block
        CHARACTER(LEN=:), ALLOCATABLE :: tmp_offset

        ASSOCIATE (new_len => LEN(prior_offset) - LEN(me%offset))
            ALLOCATE(tmp_offset,source=prior_offset(1:new_len))
            CALL MOVE_ALLOC(tmp_offset,prior_offset) !! Reset the offset length
        END ASSOCIATE

        SELECT CASE (file_format)
        CASE (ascii)
            WRITE(unit,'(a)',advance='no') prior_offset // '</' // me%name // '>' // new_line('a')
        CASE (binary)
            WRITE(unit) '</' // me%name // '>' // new_line('a')
        END SELECT

        END PROCEDURE element_end

        MODULE PROCEDURE element_write
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 05/03/2019
        !!
        !! Writes the element to the file
        !!
        INTEGER(i4k) :: i, j

        CALL me%begin(unit)

        IF (ALLOCATED(me%string)) THEN
            DO i = 1, SIZE(me%string)
                SELECT CASE (file_format)
                CASE (ASCII)
                    WRITE(unit,'(a)',advance='no') prior_offset // me%string(i)%text
                CASE (BINARY)
                    WRITE(unit) me%string(i)%text
                END SELECT
            END DO
        ELSE IF (ALLOCATED(me%real32)) THEN
            DO i = 1, SIZE(me%real32)
                ASSOCIATE (n_vals => SIZE(me%real32(i)%val))
                    WRITE(unit) (me%real32(i)%val(j),j=1,n_vals)
                END ASSOCIATE
            END DO
            WRITE(unit) new_line('a')
        ELSE IF (ALLOCATED(me%real64)) THEN
            DO i = 1, SIZE(me%real64)
                ASSOCIATE (n_vals => SIZE(me%real64(i)%val))
                    DO j = 1, n_vals
                        WRITE(unit) me%real64(i)%val(j)
                    END DO
                END ASSOCIATE
            END DO
            WRITE(unit) new_line('a')
        END IF

        IF (ALLOCATED(me%element)) THEN
            DO i = 1, SIZE(me%element)
                CALL me%element(i)%write(unit)
            END DO
        END IF

        CALL me%end(unit)

        END PROCEDURE element_write

        MODULE PROCEDURE replace_in_string
        IMPLICIT NONE
        !! Replaces the existing value associated with tag with value

        !! Find where "tag" is located

        !! Find the end of the string associated with "tag"

        !! Replace that value with value

        ERROR STOP 'Error: replace_in_string in xml_procedures is not yet implemented.'

        END PROCEDURE replace_in_string

        MODULE PROCEDURE XML_file_setup
        USE Misc, ONLY : to_lowercase
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 05/02/2019
        !!
        !! Establishes the XML file information
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
            SELECT CASE (to_lowercase(form))
            CASE ('formatted')
                ALLOCATE(me%form, source='FORMATTED')    !! Ignore the user-defined form, even if present
                file_format = ascii
            CASE ('unformatted')
                ALLOCATE(me%form, source='UNFORMATTED')  !! Ignore the user-defined form, even if present
                file_format = binary
            CASE DEFAULT
                ERROR STOP 'Error: Unknown value for form. Terminated in XML_file_setup'
            END SELECT
        ELSE
            SELECT CASE (file_format)
            CASE (ascii)
                ALLOCATE(me%form, source='FORMATTED')
                file_format = ascii
            CASE (binary)
                ALLOCATE(me%form, source='UNFORMATTED')
                file_format = binary
            CASE DEFAULT
                ERROR STOP 'Error: Unknown value for file_format. Terminated in XML_file_setup'
            END SELECT
        END IF

        file_format_text = convert_format_to_string (file_format)
!        ALLOCATE(me%access, source='SEQUENTIAL') !! Ignore the user-defined access, even if present
        ALLOCATE(me%access, source='STREAM') !! Ignore the user-defined access, even if present

        IF (.NOT. ALLOCATED(prior_offset)) ALLOCATE(prior_offset,source='')

        END PROCEDURE XML_file_setup

        MODULE PROCEDURE XML_begin
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 05/03/2019
        !!
        !! Begins the writing of the XMl file
        !!

        IF (.NOT. ALLOCATED(me%filename)) THEN
            WRITE(0,*) 'WARNING: FILE NAME HAS NOT YET BEEN SET IN XML_begin'
            CALL me%setup('dummy')
        END IF

        CALL me%make_file()

        SELECT CASE (file_format)
        CASE (ascii)
            WRITE(me%unit,'(a)',advance='no') version // new_line('a')
        CASE (binary)
            WRITE(me%unit) version // new_line('a')
        END SELECT

        END PROCEDURE XML_begin

        MODULE PROCEDURE XML_add
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 05/03/2019
        !!
        !! This adds data inside of the file
        !!
        TYPE(xml_element_dt), DIMENSION(:), ALLOCATABLE :: tmp_element_dt

!! This is how this routine should work (and does work w/ Intel)
!        IF (.NOT. ALLOCATED(me%element)) THEN
!            ALLOCATE(me%element(1),source=element)
!        ELSE
!            ALLOCATE(tmp_element_dt,source=[ me%element(:), element ]) ! This segfaults at runtime
!            CALL MOVE_ALLOC(tmp_element_dt, me%element)
!        END IF
!! This is a temporary work around
        IF (.NOT. ALLOCATED(me%element)) THEN
            SELECT TYPE (element)
            CLASS IS (xml_element_dt)
                CALL gcc_bug_workaround_allocate(me%element, element)
            END SELECT
        ELSE
            SELECT TYPE (element)
            CLASS IS (xml_element_dt)
                CALL gcc_bug_workaround_allocate(tmp_element_dt, oldfoo=me%element)
                CALL gcc_bug_workaround_allocate(me%element, element, tmp_element_dt)
            END SELECT
        END IF

        CALL gcc_bug_workaround_deallocate (tmp_element_dt)

        END PROCEDURE XML_add

        MODULE PROCEDURE XML_end
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 05/03/2019
        !!
        !! Ends the writing of the XMl file
        !!

        CALL me%close_file()

        END PROCEDURE XML_end

        MODULE PROCEDURE xml_write
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 05/03/2019
        !!
        !! Writes the XMl file
        !!
        INTEGER(i4k) :: i

        CALL me%begin()

        DO i = 1, SIZE(me%element)
            CALL me%element(i)%write(me%unit)
        END DO

        CALL me%end()

        END PROCEDURE xml_write

        MODULE PROCEDURE gcc_bug_workaround_allocate
        IMPLICIT NONE
        !! gcc Work-around for allocating a multi-dimension derived type w/ allocatable character strings
        !! when trying to increase the size of the foo array by 1
        INTEGER(i4k) :: i

        IF (ALLOCATED(me)) CALL gcc_bug_workaround_deallocate(me)
        IF (PRESENT(oldfoo)) THEN
            IF (PRESENT(addfoo)) THEN
                ALLOCATE (me(SIZE(oldfoo)+1))
            ELSE
                ALLOCATE (me(SIZE(oldfoo)))
            END IF
            DO i = 1, SIZE(oldfoo)
                IF (ALLOCATED(oldfoo(i)%name)) ALLOCATE(me(i)%name, source=oldfoo(i)%name)
                me(i)%unit = oldfoo(i)%unit
                IF (ALLOCATED(oldfoo(i)%offset)) ALLOCATE(me(i)%offset, source=oldfoo(i)%offset)
                IF (ALLOCATED(oldfoo(i)%additional_data)) &
                  &  ALLOCATE(me(i)%additional_data, source=oldfoo(i)%additional_data)
                IF (ALLOCATED(oldfoo(i)%string)) ALLOCATE(me(i)%string, source=oldfoo(i)%string)
                IF (ALLOCATED(oldfoo(i)%real32)) ALLOCATE(me(i)%real32, source=oldfoo(i)%real32)
                IF (ALLOCATED(oldfoo(i)%real64)) ALLOCATE(me(i)%real64, source=oldfoo(i)%real64)
                IF (ALLOCATED(oldfoo(i)%element)) CALL gcc_bug_workaround_allocate(me(i)%element, oldfoo=oldfoo(i)%element)
            END DO
        ELSE
            ALLOCATE(me(1))
        END IF
        IF (PRESENT(addfoo)) THEN
            i = UBOUND(me,DIM=1)
            IF (ALLOCATED(addfoo%name)) ALLOCATE(me(i)%name, source=addfoo%name)
            me(i)%unit = addfoo%unit
            IF (ALLOCATED(addfoo%offset)) ALLOCATE(me(i)%offset, source=addfoo%offset)
            IF (ALLOCATED(addfoo%additional_data)) &
              &  ALLOCATE(me(i)%additional_data, source=addfoo%additional_data)
            IF (ALLOCATED(addfoo%string)) ALLOCATE(me(i)%string, source=addfoo%string)
            IF (ALLOCATED(addfoo%real32)) ALLOCATE(me(i)%real32, source=addfoo%real32)
            IF (ALLOCATED(addfoo%real64)) ALLOCATE(me(i)%real64, source=addfoo%real64)
            IF (ALLOCATED(addfoo%element)) CALL gcc_bug_workaround_allocate(me(i)%element, oldfoo=addfoo%element)
        END IF

        END PROCEDURE gcc_bug_workaround_allocate

        MODULE PROCEDURE gcc_bug_workaround_deallocate_array
        IMPLICIT NONE
        !! gcc Work-around for deallocating a multi-dimension derived type w/ allocatable character strings
        INTEGER(i4k) :: i

        IF (ALLOCATED(me)) THEN
            DO i = LBOUND(me,DIM=1), UBOUND(me,DIM=1)
                CALL gcc_bug_workaround_deallocate(me(i))
            END DO
            IF (ALLOCATED(me)) DEALLOCATE(me)
        END IF

        END PROCEDURE gcc_bug_workaround_deallocate_array

        MODULE PROCEDURE gcc_bug_workaround_deallocate_single
        IMPLICIT NONE
        !! gcc Work-around for deallocating a multi-dimension derived type w/ allocatable character strings
        INTEGER(i4k) :: i

        IF (ALLOCATED(me%name))            DEALLOCATE(me%name)
        IF (ALLOCATED(me%offset))          DEALLOCATE(me%offset)
        IF (ALLOCATED(me%additional_data)) DEALLOCATE(me%additional_data)
        IF (ALLOCATED(me%string)) THEN
            DO i = LBOUND(me%string,DIM=1), UBOUND(me%string,DIM=1)
                CALL gcc_bug_deallocate_string_dt(me%string(i))
                !IF (ALLOCATED(me%string(i))) DEALLOCATE(me%string(i))
            END DO
            IF (ALLOCATED(me%string)) DEALLOCATE(me%string)
        END IF
        IF (ALLOCATED(me%real32))     DEALLOCATE(me%real32)
        IF (ALLOCATED(me%real64))     DEALLOCATE(me%real64)
        IF (ALLOCATED(me%element)) THEN
            DO i = LBOUND(me%element,DIM=1), UBOUND(me%element,DIM=1)
                CALL gcc_bug_workaround_deallocate (me%element(i))
            END DO
            IF (ALLOCATED(me%element)) DEALLOCATE(me%element)
        END IF

        END PROCEDURE gcc_bug_workaround_deallocate_single

        MODULE PROCEDURE gcc_bug_deallocate_string_dt
        IMPLICIT NONE
        !! gcc Work-around to de-allocate the string derived type

        IF (ALLOCATED(me%text)) DEALLOCATE(me%text)

        END PROCEDURE gcc_bug_deallocate_string_dt

        MODULE PROCEDURE gcc_bug_workaround_deallocate_xml_file_dt
        IMPLICIT NONE
        !! gcc Work-around to de-allocate the string derived type

        IF (ALLOCATED(me%element)) CALL gcc_bug_workaround_deallocate (me%element)

        END PROCEDURE gcc_bug_workaround_deallocate_xml_file_dt

        MODULE PROCEDURE convert_format_to_string
        IMPLICIT NONE
        !! Converts the format integer to string

        SELECT CASE (format)
        CASE (ascii)
            ALLOCATE(string,source=format_ascii)
        CASE (binary)
            ALLOCATE(string,source=format_binary)
        CASE (append)
            ALLOCATE(string,source=format_append)
        CASE DEFAULT
            ERROR STOP 'Error: Undefined format in convert_format_to_string'
        END SELECT

        END PROCEDURE convert_format_to_string

        MODULE PROCEDURE convert_string_to_format
        USE Misc, ONLY : to_lowercase
        IMPLICIT NONE
        !! Converts the format integer to string

        SELECT CASE (to_lowercase(string))
        CASE (format_ascii)
            format = ascii
        CASE (format_binary)
            format = binary
        CASE (format_append)
            format = append
        CASE DEFAULT
            ERROR STOP 'Error: Undefined string in convert_string_to_format'
        END SELECT

        END PROCEDURE convert_string_to_format

END SUBMODULE XML_implementation
