MODULE vtk_attributes
    USE Kinds
    IMPLICIT NONE
    !>@brief
    !> This module contains the dataset attributes for vtk format
    !>@author
    !> Ian Porter
    !>@date
    !> 12/13/2017
    !
    ! The following dataset attributes are available:
    ! 1) scalars
    ! 2) vectors
    ! 3) normals
    ! 4) texture coordinates (1D, 2D & 3D)
    ! 5) 3x3 tensors
    ! 6) field data
    !

    PRIVATE
    PUBLIC :: attribute, scalar

    CHARACTER(LEN=*), PARAMETER :: default = 'default'

    TYPE, ABSTRACT :: attribute
        CHARACTER(LEN=:), ALLOCATABLE :: dataname
    CONTAINS
        PROCEDURE(abs_read),  DEFERRED, PUBLIC :: read
        PROCEDURE(abs_write), DEFERRED, PUBLIC :: write
        PROCEDURE(abs_setup), DEFERRED, PUBLIC :: setup
        PROCEDURE, PRIVATE :: check_for_diffs
        GENERIC :: OPERATOR(.diff.) => check_for_diffs
    END TYPE attribute

    TYPE, EXTENDS(attribute) :: scalar
        INTEGER(i4k) :: numcomp
        CHARACTER(LEN=:), ALLOCATABLE :: datatype
        CHARACTER(LEN=:), ALLOCATABLE :: tablename
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: scalars
    CONTAINS
        PROCEDURE :: read  => scalar_read
        PROCEDURE :: write => scalar_write
        PROCEDURE :: setup => scalar_setup
        PROCEDURE, PRIVATE :: check_for_diffs => check_for_diffs_scalar
    END TYPE scalar

    CONTAINS
        SUBROUTINE abs_read (me, unit)
        USE Misc, ONLY : interpret_string
        CLASS(attribute), INTENT(OUT), TARGET :: me
        INTEGER(i4k),     INTENT(IN)  :: unit
        END SUBROUTINE abs_read

        SUBROUTINE scalar_read (me, unit)
        USE Misc, ONLY : interpret_string
        CLASS(scalar), INTENT(OUT), TARGET :: me
        INTEGER(i4k),  INTENT(IN)  :: unit
        INTEGER(i4k) :: i, iostat
        LOGICAL :: end_of_file
        CHARACTER(LEN=200) :: line
        CHARACTER(LEN=:), ALLOCATABLE :: text
        INTEGER(i4k), DIMENSION(:), ALLOCATABLE :: ints
        CHARACTER(LEN=:), DIMENSION(:), ALLOCATABLE :: chars
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: dummy

        READ(unit,100) line
        CALL interpret_string (line=line, separator=' ', datatype=(/ 'C','C','I' /), &
          &                    ints=ints, chars=chars)
        me%numcomp = ints(1); me%dataname = TRIM(chars(1)); me%datatype = TRIM(chars(2))

        READ(unit,101) line
        CALL interpret_string (line=line, separator=' ', datatype=(/ 'C' /), chars=chars)
        me%tablename = TRIM(chars(1))

        ALLOCATE(me%scalars(1))
        end_of_file  = .FALSE.
        i = 1
        DO
            READ(unit,102,iostat=iostat) me%scalars(i)
            end_of_file = (iostat < 0)
            IF (.NOT. end_of_file) THEN
                ALLOCATE(dummy(1:UBOUND(me%scalars,DIM=1)+1),source=0.0_r8k)
                dummy(1:UBOUND(me%scalars,DIM=1)) = me%scalars
                CALL MOVE_ALLOC(dummy, me%scalars)
                i = i + 1
            ELSE
                EXIT
            END IF
        END DO

100     FORMAT('SCALARS ',(a),' ',(a),' ',(i1))
101     FORMAT('LOOKUP_TABLE ',(a))
102     FORMAT(es12.5)
        END SUBROUTINE scalar_read

        SUBROUTINE abs_write (me, unit)
        CLASS(attribute), INTENT(IN) :: me
        INTEGER(i4k),     INTENT(IN) :: unit
        SELECT TYPE (me)
        END SELECT
        END SUBROUTINE abs_write

        SUBROUTINE scalar_write (me, unit)
        CLASS(scalar), INTENT(IN) :: me
        INTEGER(i4k),  INTENT(IN) :: unit
        INTEGER(i4k) :: i

        WRITE(unit,100) me%dataname, me%datatype, me%numcomp
        WRITE(unit,101) me%tablename
        DO i = 1, SIZE(me%scalars)
            WRITE(unit,102) me%scalars(i)
        END DO

100     FORMAT('SCALARS ',(a),' ',(a),' ',(i1))
101     FORMAT('LOOKUP_TABLE ',(a))
102     FORMAT(es12.5)
        END SUBROUTINE scalar_write

        SUBROUTINE abs_setup (me, dataname, datatype, numcomp, tablename, scalars)
        CLASS(attribute), INTENT(INOUT) :: me
        CHARACTER(LEN=*), INTENT(IN)    :: dataname
        INTEGER(i4k),     INTENT(IN), OPTIONAL :: numcomp
        CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: datatype, tablename
        REAL(r8k), DIMENSION(:), INTENT(IN), OPTIONAL :: scalars
        SELECT TYPE (me)
        END SELECT
        END SUBROUTINE abs_setup

        SUBROUTINE scalar_setup (me, dataname, datatype, numcomp, tablename, scalars)
        CLASS(scalar),    INTENT(INOUT) :: me
        CHARACTER(LEN=*), INTENT(IN)    :: dataname
        INTEGER(i4k),     INTENT(IN), OPTIONAL :: numcomp
        CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: datatype, tablename
        REAL(r8k), DIMENSION(:), INTENT(IN), OPTIONAL :: scalars

        me%dataname = dataname
        IF (PRESENT(datatype)) THEN
            me%datatype = datatype
        ELSE
            me%datatype = 'double'
        END IF
        IF (PRESENT(numcomp)) THEN
            me%numcomp = numcomp
        ELSE
            me%numcomp = 1
        END IF
        IF (PRESENT(tablename)) THEN
            me%tablename = tablename
        ELSE
            me%tablename = default
        END IF
        IF (.NOT. PRESENT(scalars)) THEN
            ERROR STOP 'Must provide scalars in scalar_setup'
        ELSE
            me%scalars = scalars
        END IF

        END SUBROUTINE scalar_setup
        
        FUNCTION check_for_diffs (me, you) RESULT (diffs)
        CLASS(attribute), INTENT(IN) :: me, you
        LOGICAL :: diffs

        diffs = .FALSE.
        IF      (.NOT. SAME_TYPE_AS(me,you))  THEN
            diffs = .TRUE.
        ELSE IF (me%dataname /= you%dataname) THEN
            diffs = .TRUE.
        END IF

        END FUNCTION check_for_diffs

        FUNCTION check_for_diffs_scalar (me, you) RESULT (diffs)
        CLASS(scalar), INTENT(IN) :: me
        CLASS(attribute), INTENT(IN) :: you
        INTEGER(i4k) :: i
        LOGICAL :: diffs

        diffs = .FALSE.
        IF (.NOT. SAME_TYPE_AS(me,you)) THEN
            diffs = .TRUE.
        ELSE
            SELECT TYPE (you)
            CLASS IS (scalar)
                IF (me%dataname /= you%dataname)        THEN
                    diffs = .TRUE.
                ELSE IF (me%datatype /= you%datatype)   THEN
                    diffs = .TRUE.
                ELSE IF (me%numcomp /= you%numcomp)     THEN
                    diffs = .TRUE.
                ELSE IF (me%tablename /= you%tablename) THEN
                    diffs = .TRUE.
                ELSE
                    DO i = 1, UBOUND(me%scalars,DIM=1)
                        IF (me%scalars(i) /= you%scalars(i))     THEN
                            diffs = .TRUE.
                        END IF
                    END DO
                END IF
            END SELECT
        END IF

        END FUNCTION check_for_diffs_scalar
END MODULE vtk_attributes
