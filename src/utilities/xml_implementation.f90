SUBMODULE (XML) XML_implementation
    IMPLICIT NONE

    INTEGER, PARAMETER :: def_offset = 4          !! Default # of leading spaces
    CHARACTER(LEN=:), ALLOCATABLE :: prior_offset !! Number of leading spaces for prior XML block
    CHARACTER(LEN=*), PARAMETER :: version = '<?xml version="1.0" encoding="UTF-8"?>'

    CONTAINS

        MODULE PROCEDURE element_setup
        IMPLICIT NONE
        !! This sets up the information needed to define the XML element block
        INTEGER :: i, my_offset

        me%name = name
        IF (LEN_TRIM(string) == 0) THEN
            ALLOCATE(me%additional_data,source='')
        ELSE
            me%additional_data = ' ' // string
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
        INTEGER(i4k) :: i

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

        IF (.NOT. ALLOCATED(prior_offset)) ALLOCATE(prior_offset,source='')

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
        TYPE(xml_element_dt), DIMENSION(:), ALLOCATABLE :: tmp_element_dt

        IF (.NOT. ALLOCATED(me%element)) THEN
            ALLOCATE(me%element(1),source=element)
        ELSE
!            WRITE(0,*) allocated(TMP_ELEMENT_DT)
!            WRITE(0,*) allocated(ME%ELEMENT)
!            WRITE(0,*) SIZE(me%element)
!            CALL element%write(0)
!            allocate(tmp_element_dt,source=[element]) !This causes a gcc8.3 ICE
            ALLOCATE(tmp_element_dt,source=[ me%element, element ]) ! This segfaults at runtime
            CALL MOVE_ALLOC(tmp_element_dt, me%element)
        END IF

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

END SUBMODULE XML_implementation
