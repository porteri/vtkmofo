SUBMODULE (XML) XML_implementation
    IMPLICIT NONE

    INTEGER, PARAMETER :: def_offset = 4          !! Default # of leading spaces
    CHARACTER(LEN=:), ALLOCATABLE :: prior_offset !! Number of leading spaces for prior XML block

    CONTAINS

        MODULE PROCEDURE setup
        IMPLICIT NONE
        !! This sets up the information needed to define the XML block
        INTEGER :: i

        me%name = name
        me%unit = unit

        DO i = 0, offset
            IF (i == 0) THEN
                ALLOCATE(me%offset,source='')
            ELSE
                me%offset = me%offset // ' '
            END IF
        END DO

        IF (.NOT. ALLOCATED(prior_offset)) ALLOCATE(prior_offset,source='')

        END PROCEDURE setup

        MODULE PROCEDURE begin
        IMPLICIT NONE
        !! This begins an xml block

        IF (LEN_TRIM(string) == 0) THEN
            WRITE(me%unit,'(a)',advance='no') prior_offset // '<' // me%name // '>' // new_line('a')
        ELSE
            WRITE(me%unit,'(a)',advance='no') prior_offset // '<' // me%name // ' ' // string // '>' // new_line('a')
        END IF

        prior_offset = prior_offset // me%offset  !! Set the new offset length

        END PROCEDURE begin

        MODULE PROCEDURE add
        IMPLICIT NONE
        !! This adds data inside of an xml block

        WRITE(me%unit,'(a)',advance='no') prior_offset // string // new_line('a')

        END PROCEDURE add

        MODULE PROCEDURE end
        IMPLICIT NONE
        !! This ends an XML block

        ASSOCIATE (new_len => LEN(prior_offset) - LEN(me%offset))
            prior_offset = prior_offset(1:new_len) !! Reset the offset length
        END ASSOCIATE

        WRITE(me%unit,'(a)',advance='no') prior_offset // '</' // me%name // '>' // new_line('a')

        END PROCEDURE end

END SUBMODULE XML_implementation
