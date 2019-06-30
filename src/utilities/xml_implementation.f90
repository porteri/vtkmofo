SUBMODULE (XML) XML_implementation
    IMPLICIT NONE

    INTEGER, PARAMETER :: def_offset = 4          !! Default # of leading spaces
    CHARACTER(LEN=:), ALLOCATABLE :: prior_offset !! Number of leading spaces for prior XML block
    CHARACTER(LEN=*), PARAMETER :: version = '<?xml version="1.0" encoding="UTF-8"?>'
    TYPE(xml_element_dt), DIMENSION(:), ALLOCATABLE, SAVE :: gcc_bug_tmp_element_dt

    CONTAINS

        MODULE PROCEDURE element_setup
        IMPLICIT NONE
        !! This sets up the information needed to define the XML element block
        INTEGER :: i, my_offset

        me%name = name
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

        my_offset = def_offset
        IF (present(offset)) THEN
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

        WRITE(unit,'(a)',advance='no') prior_offset // '<' // me%name // me%additional_data // '>' // new_line('a')

        prior_offset = prior_offset // me%offset  !! Set the new offset length

        END PROCEDURE element_begin

        MODULE PROCEDURE element_add_data
        IMPLICIT NONE
        !! This adds data inside of an xml element block
        TYPE(string_dt), DIMENSION(:), ALLOCATABLE :: tmp_string_dt

        IF (.NOT. ALLOCATED(me%string)) THEN
            ALLOCATE(me%string(0))
        END IF

        ALLOCATE(tmp_string_dt(1:SIZE(me%string)+1))
        tmp_string_dt(1:SIZE(me%string)) = me%string
        CALL MOVE_ALLOC(tmp_string_dt, me%string)

        ASSOCIATE (my_entry => UBOUND(me%string,DIM=1))
            ALLOCATE(me%string(my_entry)%text,source=string // new_line('a'))
        END ASSOCIATE

        END PROCEDURE element_add_data

        MODULE PROCEDURE element_add_element
        IMPLICIT NONE
        !! This adds an element inside of an xml element block
        TYPE(xml_element_dt), DIMENSION(:), ALLOCATABLE :: tmp_element_dt

        IF (.NOT. ALLOCATED(me%element)) THEN
            ALLOCATE(me%element(1),source=element)
        ELSE
            ALLOCATE(tmp_element_dt,source=[ me%element, element ])
            CALL MOVE_ALLOC(tmp_element_dt, me%element)
        END IF

        END PROCEDURE element_add_element

        MODULE PROCEDURE element_end
        IMPLICIT NONE
        !! This ends an XML element block

        ASSOCIATE (new_len => LEN(prior_offset) - LEN(me%offset))
            prior_offset = prior_offset(1:new_len) !! Reset the offset length
        END ASSOCIATE

        WRITE(unit,'(a)',advance='no') prior_offset // '</' // me%name // '>' // new_line('a')

        END PROCEDURE element_end

        MODULE PROCEDURE element_write
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 05/03/2019
        !!
        !! Writes the element to the file
        !!
        INTEGER :: i

        CALL me%begin(unit)

        IF (ALLOCATED(me%string)) THEN
            DO i = 1, SIZE(me%string)
                WRITE(unit,'(a)',advance='no') prior_offset // me%string(i)%text
            END DO
        END IF

        IF (ALLOCATED(me%element)) THEN
            DO i = 1, SIZE(me%element)
                CALL me%element(i)%write(unit)
            END DO
        END IF

        CALL me%end(unit)

        END PROCEDURE element_write

        MODULE PROCEDURE XML_file_setup
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 05/02/2019
        !!
        !! Establishes the XML file information
        !!
write(0,*) 'in xml_file_setup'
        me%filename = TRIM(filename)
        IF (PRESENT(open_status)) THEN
            me%open_status = open_status
        ELSE
            me%open_status = 'UNKNOWN'
        END IF
        IF (PRESENT(close_status)) THEN
            me%close_status = close_status
        ELSE
            me%close_status = 'KEEP'
        END IF
        me%form   = 'FORMATTED'  !! Ignore the user-defined form, even if present
        me%access = 'SEQUENTIAL' !! Ignore the user-defined access, even if present
write(0,*) 'before allocation of prior_offset'
        IF (.NOT. ALLOCATED(prior_offset)) ALLOCATE(prior_offset,source='')
write(0,*) 'after allocation of prior_offset'
        END PROCEDURE XML_file_setup

        MODULE PROCEDURE XML_begin
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 05/03/2019
        !!
        !! Begins the writing of the XMl file
        !!

        CALL me%make_file()

        WRITE(me%unit,'(a)',advance='no') version // new_line('a')

        END PROCEDURE XML_begin

        MODULE PROCEDURE XML_add
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 05/03/2019
        !!
        !! This adds data inside of the file
        !!
        INTEGER(i4k) :: i
        TYPE(xml_element_dt), DIMENSION(:), ALLOCATABLE :: tmp_element_dt

!! This is how this routine should work (and does work w/ Intel)
!        IF (.NOT. ALLOCATED(me%element)) THEN
!            ALLOCATE(me%element(1),source=element)
!        ELSE
!            ALLOCATE(tmp_element_dt,source=[ me%element(:), element ]) ! This segfaults at runtime
!            CALL MOVE_ALLOC(tmp_element_dt, me%element)
!        END IF
!! This is a temporary work around
write(0,*) 'start of xml_add'
        IF (.NOT. ALLOCATED(gcc_bug_tmp_element_dt)) THEN
            SELECT TYPE (element)
            CLASS IS (xml_element_dt)
                write(0,*) 'before initial gcc_bug_tmp_element_dt allocation'
                ALLOCATE (gcc_bug_tmp_element_dt(1))
                IF (ALLOCATED(element%name)) gcc_bug_tmp_element_dt(1)%name = element%name
                gcc_bug_tmp_element_dt(1)%unit = element%unit
                IF (ALLOCATED(element%offset)) gcc_bug_tmp_element_dt(1)%offset = element%offset
                IF (ALLOCATED(element%additional_data)) gcc_bug_tmp_element_dt(1)%additional_data = element%additional_data
                IF (ALLOCATED(element%string)) gcc_bug_tmp_element_dt(1)%string = element%string
                IF (ALLOCATED(element%element)) gcc_bug_tmp_element_dt(1)%element = element%element
                write(0,*) 'after initial gcc_bug_tmp_element_dt allocation'
            END SELECT
        ELSE
            write(0,*) 'before ALLOCATE tmp_element_dt when > 1'
            ALLOCATE (tmp_element_dt(SIZE(gcc_bug_tmp_element_dt)+1))
            DO i = 1, SIZE(gcc_bug_tmp_element_dt)
                IF (ALLOCATED(gcc_bug_tmp_element_dt(i)%name)) tmp_element_dt(i)%name = gcc_bug_tmp_element_dt(i)%name
                tmp_element_dt(i)%unit = gcc_bug_tmp_element_dt(i)%unit
                IF (ALLOCATED(gcc_bug_tmp_element_dt(i)%offset)) tmp_element_dt(i)%offset = gcc_bug_tmp_element_dt(i)%offset
                IF (ALLOCATED(gcc_bug_tmp_element_dt(i)%additional_data)) &
                  &  tmp_element_dt(i)%additional_data = gcc_bug_tmp_element_dt(i)%additional_data
                IF (ALLOCATED(gcc_bug_tmp_element_dt(i)%string)) tmp_element_dt(i)%string = gcc_bug_tmp_element_dt(i)%string
                IF (ALLOCATED(gcc_bug_tmp_element_dt(i)%element)) tmp_element_dt(i)%element = gcc_bug_tmp_element_dt(i)%element
                write(0,*) 'before MOVE_ALLOC on tmp_element_dt when > 1'
            END DO
            IF (ALLOCATED(element%name)) tmp_element_dt(i)%name = element%name
            tmp_element_dt(i)%unit = element%unit
            IF (ALLOCATED(element%offset)) tmp_element_dt(i)%offset = element%offset
            IF (ALLOCATED(element%additional_data)) &
              &  tmp_element_dt(i)%additional_data = element%additional_data
            IF (ALLOCATED(element%string)) tmp_element_dt(i)%string = element%string
            IF (ALLOCATED(element%element)) tmp_element_dt(i)%element = element%element
            write(0,*) 'before re-allocation of gcc_bug_tmp_element_dt'
            CALL gcc_bug_workaround_deallocate (gcc_bug_tmp_element_dt)
            write(0,*) 'afte re-allocation of gcc_bug_tmp_element_dt'
            ALLOCATE(gcc_bug_tmp_element_dt(1:SIZE(tmp_element_dt)))
            DO i = 1, SIZE(gcc_bug_tmp_element_dt)
                IF (ALLOCATED(tmp_element_dt(i)%name)) gcc_bug_tmp_element_dt(i)%name = tmp_element_dt(i)%name
                gcc_bug_tmp_element_dt(i)%unit = tmp_element_dt(i)%unit
                IF (ALLOCATED(tmp_element_dt(i)%offset)) gcc_bug_tmp_element_dt(i)%offset = tmp_element_dt(i)%offset
                IF (ALLOCATED(tmp_element_dt(i)%additional_data)) &
                  &  gcc_bug_tmp_element_dt(i)%additional_data = tmp_element_dt(i)%additional_data
                IF (ALLOCATED(tmp_element_dt(i)%string)) gcc_bug_tmp_element_dt(i)%string = tmp_element_dt(i)%string
                IF (ALLOCATED(tmp_element_dt(i)%element)) gcc_bug_tmp_element_dt(i)%element = tmp_element_dt(i)%element
                write(0,*) 'before MOVE_ALLOC on tmp_element_dt when > 1'
            END DO
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
write(0,*) 'start of xml_write'
        CALL me%begin()
write(0,*) 'before allocate in xml_write'
        ALLOCATE(me%element, source=gcc_bug_tmp_element_dt)
write(0,*) 'after allocate in xml_write'
        DO i = 1, SIZE(me%element)
            CALL me%element(i)%write(me%unit)
        END DO
write(0,*) 'before me%end'
        CALL me%end()

        END PROCEDURE xml_write

        SUBROUTINE gcc_bug_workaround_allocate (foo)
        IMPLICIT NONE
        !! gcc Work-around for allocating a multi-dimension derived type w/ allocatable character strings
        TYPE(xml_element_dt), DIMENSION(:), INTENT(INOUT), ALLOCATABLE :: foo
        INTEGER :: i

        END SUBROUTINE gcc_bug_workaround_allocate

        SUBROUTINE gcc_bug_workaround_deallocate (foo)
        IMPLICIT NONE
        !! gcc Work-around for deallocating a multi-dimension derived type w/ allocatable character strings
        TYPE(xml_element_dt), DIMENSION(:), INTENT(INOUT), ALLOCATABLE :: foo
        INTEGER :: i

        IF (ALLOCATED(foo)) THEN
            DO i = 1, SIZE(foo)
                IF (ALLOCATED(foo(i)%name))            DEALLOCATE(foo(i)%name)
                IF (ALLOCATED(foo(i)%offset))          DEALLOCATE(foo(i)%offset)
                IF (ALLOCATED(foo(i)%additional_data)) DEALLOCATE(foo(i)%additional_data)
                IF (ALLOCATED(foo(i)%string))          DEALLOCATE(foo(i)%string)
                IF (ALLOCATED(foo(i)%element))         DEALLOCATE(foo(i)%element)
            END DO
            DEALLOCATE(foo)
        END IF

        END SUBROUTINE gcc_bug_workaround_deallocate

END SUBMODULE XML_implementation
