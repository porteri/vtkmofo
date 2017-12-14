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
    PUBLIC :: attribute, scalar, vector, normal, texture

    CHARACTER(LEN=*), PARAMETER :: default = 'default'

    TYPE, ABSTRACT :: attribute
        CHARACTER(LEN=:), ALLOCATABLE :: dataname
        CHARACTER(LEN=:), ALLOCATABLE :: datatype
    CONTAINS
        PROCEDURE(abs_read),  DEFERRED, PUBLIC :: read
        PROCEDURE(abs_write), DEFERRED, PUBLIC :: write
        PROCEDURE(abs_setup), DEFERRED, PUBLIC :: setup
        PROCEDURE, PRIVATE :: check_for_diffs
        GENERIC :: OPERATOR(.diff.) => check_for_diffs
    END TYPE attribute

    TYPE, EXTENDS(attribute) :: scalar
        INTEGER(i4k) :: numcomp
        CHARACTER(LEN=:), ALLOCATABLE :: tablename
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: scalars
    CONTAINS
        PROCEDURE :: read  => scalar_read
        PROCEDURE :: write => scalar_write
        PROCEDURE :: setup => scalar_setup
        PROCEDURE, PRIVATE :: check_for_diffs => check_for_diffs_scalar
    END TYPE scalar

    TYPE, EXTENDS(attribute) :: vector
        REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: vectors
    CONTAINS
        PROCEDURE :: read  => vector_read
        PROCEDURE :: write => vector_write
        PROCEDURE :: setup => vector_setup
        PROCEDURE, PRIVATE :: check_for_diffs => check_for_diffs_vector
    END TYPE vector

    TYPE, EXTENDS(attribute) :: normal
        REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: normals
    CONTAINS
        PROCEDURE :: read  => normal_read
        PROCEDURE :: write => normal_write
        PROCEDURE :: setup => normal_setup
        PROCEDURE, PRIVATE :: check_for_diffs => check_for_diffs_normal
    END TYPE normal

    TYPE, EXTENDS(attribute) :: texture
        REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: textures
    CONTAINS
        PROCEDURE :: read  => texture_read
        PROCEDURE :: write => texture_write
        PROCEDURE :: setup => texture_setup
        PROCEDURE, PRIVATE :: check_for_diffs => check_for_diffs_texture
    END TYPE texture

    CONTAINS
        SUBROUTINE abs_read (me, unit)
        !>@brief
        !> Abstract for reading an attribute
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 12/13/2017
        CLASS(attribute), INTENT(OUT), TARGET :: me
        INTEGER(i4k),     INTENT(IN)  :: unit
        END SUBROUTINE abs_read

        SUBROUTINE abs_write (me, unit)
        !>@brief
        !> Abstract for writing an attribute
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 12/13/2017
        CLASS(attribute), INTENT(IN) :: me
        INTEGER(i4k),     INTENT(IN) :: unit
        SELECT TYPE (me)
        END SELECT
        END SUBROUTINE abs_write

        SUBROUTINE abs_setup (me, dataname, datatype, numcomp, tablename, values1d, values2d)
        !>@brief
        !> Abstract for performing the set-up of an attribute
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 12/13/2017
        CLASS(attribute), INTENT(INOUT) :: me
        CHARACTER(LEN=*), INTENT(IN)    :: dataname
        INTEGER(i4k),     INTENT(IN), OPTIONAL :: numcomp
        CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: datatype, tablename
!        REAL(r8k), DIMENSION(..), INTENT(IN), OPTIONAL :: values
        REAL(r8k), DIMENSION(:),   INTENT(IN), OPTIONAL :: values1d
        REAL(r8k), DIMENSION(:,:), INTENT(IN), OPTIONAL :: values2d
        SELECT TYPE (me)
        END SELECT
        END SUBROUTINE abs_setup

        FUNCTION check_for_diffs (me, you) RESULT (diffs)
        !>@brief
        !> Function checks for differences in an attribute
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 12/13/2017
        CLASS(attribute), INTENT(IN) :: me, you
        LOGICAL :: diffs

        diffs = .FALSE.
        IF      (.NOT. SAME_TYPE_AS(me,you))  THEN
            diffs = .TRUE.
        ELSE IF (me%dataname /= you%dataname) THEN
            diffs = .TRUE.
        END IF

        END FUNCTION check_for_diffs
!********
! Scalars
!********
        SUBROUTINE scalar_read (me, unit)
        USE Misc, ONLY : interpret_string
        !>@brief
        !> Subroutine performs the read for a scalar attribute
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 12/13/2017
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
        CALL interpret_string (line=line, datatype=(/ 'C','C','I' /), ignore='SCALARS ', separator=' ', &
          &                    ints=ints, chars=chars)
        me%numcomp = ints(1); me%dataname = TRIM(chars(1)); me%datatype = TRIM(chars(2))

        READ(unit,100) line
        CALL interpret_string (line=line, datatype=(/ 'C' /), ignore='LOOKUP_TABLE ', separator=' ', chars=chars)
        me%tablename = TRIM(chars(1))

        ALLOCATE(me%scalars(1))
        end_of_file  = .FALSE.
        i = 1
        DO
            READ(unit,101,iostat=iostat) me%scalars(i)
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

100     FORMAT((a))
101     FORMAT(es12.5)
        END SUBROUTINE scalar_read

        SUBROUTINE scalar_write (me, unit)
        !>@brief
        !> Subroutine performs the write for a scalar attribute
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 12/13/2017
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

        SUBROUTINE scalar_setup (me, dataname, datatype, numcomp, tablename, values1d, values2d)
        !>@brief
        !> Subroutine performs the set-up for a scalar attribute
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 12/13/2017
        CLASS(scalar),    INTENT(INOUT) :: me
        CHARACTER(LEN=*), INTENT(IN)    :: dataname
        INTEGER(i4k),     INTENT(IN), OPTIONAL :: numcomp
        CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: datatype, tablename
!        REAL(r8k), DIMENSION(..), INTENT(IN), OPTIONAL :: values
        REAL(r8k), DIMENSION(:),   INTENT(IN), OPTIONAL :: values1d
        REAL(r8k), DIMENSION(:,:), INTENT(IN), OPTIONAL :: values2d

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
        IF (.NOT. PRESENT(values1d)) THEN
            ERROR STOP 'Must provide scalars in scalar_setup'
        ELSE
            !! TODO: Implement this once SELECT RANK is incorporated into compilers (Fortran 2015)
!            SELECT RANK (values)
!            RANK(1)
!                me%scalars = values
!            RANK DEFAULT
!                ERROR STOP 'Bad rank for values. Must be RANK=2. Execution terminated in Subroutine: scalar_setup'
!            END SELECT
            me%scalars = values1d
        END IF

        END SUBROUTINE scalar_setup

        FUNCTION check_for_diffs_scalar (me, you) RESULT (diffs)
        !>@brief
        !> Function checks for differences in a scalar attribute
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 12/13/2017
        CLASS(scalar),    INTENT(IN) :: me
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
!********
! Vectors
!********
        SUBROUTINE vector_read (me, unit)
        USE Misc, ONLY : interpret_string
        !>@brief
        !> Subroutine performs the read for a vector attribute
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 12/14/2017
        CLASS(vector), INTENT(OUT), TARGET :: me
        INTEGER(i4k),  INTENT(IN)  :: unit
        INTEGER(i4k) :: i, iostat
        LOGICAL :: end_of_file
        CHARACTER(LEN=200) :: line
        CHARACTER(LEN=:), ALLOCATABLE :: text
        INTEGER(i4k), DIMENSION(:), ALLOCATABLE :: ints
        CHARACTER(LEN=:), DIMENSION(:), ALLOCATABLE :: chars
        REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: dummy

        READ(unit,100) line
        CALL interpret_string (line=line, datatype=(/ 'C','C' /), ignore='VECTORS ', separator=' ', chars=chars)
        me%dataname = TRIM(chars(1)); me%datatype = TRIM(chars(2))

        ALLOCATE(me%vectors(1,1:3))
        end_of_file  = .FALSE.
        i = 1
        DO
            READ(unit,101,iostat=iostat) me%vectors(i,1:3)
            end_of_file = (iostat < 0)
            IF (.NOT. end_of_file) THEN
                ALLOCATE(dummy(1:UBOUND(me%vectors,DIM=1)+1,1:3),source=0.0_r8k)
                dummy(1:UBOUND(me%vectors,DIM=1),1:3) = me%vectors
                CALL MOVE_ALLOC(dummy, me%vectors)
                i = i + 1
            ELSE
                EXIT
            END IF
        END DO

100     FORMAT((a))
101     FORMAT(*(es12.5))
        END SUBROUTINE vector_read

        SUBROUTINE vector_write (me, unit)
        !>@brief
        !> Subroutine performs the write for a vector attribute
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 12/13/2017
        CLASS(vector), INTENT(IN) :: me
        INTEGER(i4k),  INTENT(IN) :: unit
        INTEGER(i4k) :: i

        WRITE(unit,100) me%dataname, me%datatype
        DO i = 1, SIZE(me%vectors,DIM=1)
            WRITE(unit,101) me%vectors(i,1:3)
        END DO

100     FORMAT('VECTORS ',(a),' ',(a))
101     FORMAT(3(es12.5))
        END SUBROUTINE vector_write

        SUBROUTINE vector_setup (me, dataname, datatype, numcomp, tablename, values1d, values2d)
        !>@brief
        !> Subroutine performs the set-up for a vector attribute
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 12/14/2017
        CLASS(vector),    INTENT(INOUT) :: me
        CHARACTER(LEN=*), INTENT(IN)    :: dataname
        INTEGER(i4k),     INTENT(IN), OPTIONAL :: numcomp
        CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: datatype, tablename
!        REAL(r8k), DIMENSION(..), INTENT(IN), OPTIONAL :: values
        REAL(r8k), DIMENSION(:),   INTENT(IN), OPTIONAL :: values1d
        REAL(r8k), DIMENSION(:,:), INTENT(IN), OPTIONAL :: values2d

        me%dataname = dataname
        IF (PRESENT(datatype)) THEN
            me%datatype = datatype
        ELSE
            me%datatype = 'double'
        END IF
        IF (.NOT. PRESENT(values2d)) THEN
            ERROR STOP 'Must provide vectors in vector_setup'
        ELSE
            !! TODO: Implement this once SELECT RANK is incorporated into compilers (Fortran 2015)
!            SELECT RANK (values)
!            RANK(2)
!                me%vectors = values
!            RANK DEFAULT
!                ERROR STOP 'Bad rank for values. Must be RANK=2. Execution terminated in Subroutine: vector_setup'
!            END SELECT
            me%vectors = values2d
        END IF

        END SUBROUTINE vector_setup

        FUNCTION check_for_diffs_vector (me, you) RESULT (diffs)
        !>@brief
        !> Function checks for differences in a vector attribute
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 12/14/2017
        CLASS(vector),    INTENT(IN) :: me
        CLASS(attribute), INTENT(IN) :: you
        INTEGER(i4k) :: i, j
        LOGICAL :: diffs

        diffs = .FALSE.
        IF (.NOT. SAME_TYPE_AS(me,you)) THEN
            diffs = .TRUE.
        ELSE
            SELECT TYPE (you)
            CLASS IS (vector)
                IF (me%dataname /= you%dataname)        THEN
                    diffs = .TRUE.
                ELSE IF (me%datatype /= you%datatype)   THEN
                    diffs = .TRUE.
                ELSE
                    DO i = 1, UBOUND(me%vectors,DIM=1)
                        DO j = 1, UBOUND(me%vectors,DIM=2)
                            IF (me%vectors(i,j) /= you%vectors(i,j))     THEN
                                diffs = .TRUE.
                            END IF
                        END DO
                    END DO
                END IF
            END SELECT
        END IF

        END FUNCTION check_for_diffs_vector
!********
! Normals
!********
        SUBROUTINE normal_read (me, unit)
        USE Misc, ONLY : interpret_string
        !>@brief
        !> Subroutine performs the read for a normal attribute
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 12/14/2017
        CLASS(normal), INTENT(OUT), TARGET :: me
        INTEGER(i4k),  INTENT(IN)  :: unit
        INTEGER(i4k) :: i, iostat
        LOGICAL :: end_of_file
        CHARACTER(LEN=200) :: line
        CHARACTER(LEN=:), ALLOCATABLE :: text
        INTEGER(i4k), DIMENSION(:), ALLOCATABLE :: ints
        CHARACTER(LEN=:), DIMENSION(:), ALLOCATABLE :: chars
        REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: dummy

        READ(unit,100) line
        CALL interpret_string (line=line, datatype=(/ 'C','C' /), ignore='NORMALS ', separator=' ', chars=chars)
        me%dataname = TRIM(chars(1)); me%datatype = TRIM(chars(2))

        ALLOCATE(me%normals(1,1:3))
        end_of_file  = .FALSE.
        i = 1
        DO
            READ(unit,101,iostat=iostat) me%normals(i,1:3)
            end_of_file = (iostat < 0)
            IF (.NOT. end_of_file) THEN
                ALLOCATE(dummy(1:UBOUND(me%normals,DIM=1)+1,1:3),source=0.0_r8k)
                dummy(1:UBOUND(me%normals,DIM=1),1:3) = me%normals
                CALL MOVE_ALLOC(dummy, me%normals)
                i = i + 1
            ELSE
                EXIT
            END IF
        END DO

100     FORMAT((a))
101     FORMAT(*(es12.5))
        END SUBROUTINE normal_read

        SUBROUTINE normal_write (me, unit)
        !>@brief
        !> Subroutine performs the write for a normal attribute
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 12/13/2017
        CLASS(normal), INTENT(IN) :: me
        INTEGER(i4k),  INTENT(IN) :: unit
        INTEGER(i4k) :: i

        WRITE(unit,100) me%dataname, me%datatype
        DO i = 1, SIZE(me%normals,DIM=1)
            WRITE(unit,101) me%normals(i,1:3)
        END DO

100     FORMAT('NORMALS ',(a),' ',(a))
101     FORMAT(3(es12.5))
        END SUBROUTINE normal_write

        SUBROUTINE normal_setup (me, dataname, datatype, numcomp, tablename, values1d, values2d)
        !>@brief
        !> Subroutine performs the set-up for a normal attribute
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 12/14/2017
        CLASS(normal),    INTENT(INOUT) :: me
        CHARACTER(LEN=*), INTENT(IN)    :: dataname
        INTEGER(i4k),     INTENT(IN), OPTIONAL :: numcomp
        CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: datatype, tablename
!        REAL(r8k), DIMENSION(..), INTENT(IN), OPTIONAL :: values
        REAL(r8k), DIMENSION(:),   INTENT(IN), OPTIONAL :: values1d
        REAL(r8k), DIMENSION(:,:), INTENT(IN), OPTIONAL :: values2d

        me%dataname = dataname
        IF (PRESENT(datatype)) THEN
            me%datatype = datatype
        ELSE
            me%datatype = 'double'
        END IF
        IF (.NOT. PRESENT(values2d)) THEN
            ERROR STOP 'Must provide normals in normal_setup'
        ELSE
            !! TODO: Implement this once SELECT RANK is incorporated into compilers (Fortran 2015)
!            SELECT RANK (values)
!            RANK(2)
!                me%normals = values
!            RANK DEFAULT
!                ERROR STOP 'Bad rank for values. Must be RANK=2. Execution terminated in Subroutine: normal_setup'
!            END SELECT
            me%normals = values2d
        END IF

        END SUBROUTINE normal_setup

        FUNCTION check_for_diffs_normal (me, you) RESULT (diffs)
        !>@brief
        !> Function checks for differences in a normal attribute
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 12/14/2017
        CLASS(normal),    INTENT(IN) :: me
        CLASS(attribute), INTENT(IN) :: you
        INTEGER(i4k) :: i, j
        LOGICAL :: diffs

        diffs = .FALSE.
        IF (.NOT. SAME_TYPE_AS(me,you)) THEN
            diffs = .TRUE.
        ELSE
            SELECT TYPE (you)
            CLASS IS (normal)
                IF (me%dataname /= you%dataname)        THEN
                    diffs = .TRUE.
                ELSE IF (me%datatype /= you%datatype)   THEN
                    diffs = .TRUE.
                ELSE
                    DO i = 1, UBOUND(me%normals,DIM=1)
                        DO j = 1, UBOUND(me%normals,DIM=2)
                            IF (me%normals(i,j) /= you%normals(i,j))     THEN
                                diffs = .TRUE.
                            END IF
                        END DO
                    END DO
                END IF
            END SELECT
        END IF

        END FUNCTION check_for_diffs_normal
!********
! textures
!********
        SUBROUTINE texture_read (me, unit)
        USE Misc, ONLY : interpret_string
        !>@brief
        !> Subroutine performs the read for a texture attribute
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 12/14/2017
        CLASS(texture), INTENT(OUT), TARGET :: me
        INTEGER(i4k),  INTENT(IN)  :: unit
        INTEGER(i4k) :: i, iostat, dim
        LOGICAL :: end_of_file
        CHARACTER(LEN=200) :: line
        CHARACTER(LEN=:), ALLOCATABLE :: text
        INTEGER(i4k), DIMENSION(:), ALLOCATABLE :: ints
        CHARACTER(LEN=:), DIMENSION(:), ALLOCATABLE :: chars
        REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: dummy

        READ(unit,100) line
        CALL interpret_string (line=line, datatype=(/ 'C','I','C' /), ignore='TEXTURE_COORDINATES ', separator=' ', &
          &                    ints=ints, chars=chars)
        me%dataname = TRIM(chars(1)); me%datatype = TRIM(chars(2)); dim = ints(1)

        ALLOCATE(me%textures(1,1:dim))
        end_of_file  = .FALSE.
        i = 1
        DO
            READ(unit,101,iostat=iostat) me%textures(i,1:dim)
            end_of_file = (iostat < 0)
            IF (.NOT. end_of_file) THEN
                ALLOCATE(dummy(1:UBOUND(me%textures,DIM=1)+1,1:dim),source=0.0_r8k)
                dummy(1:UBOUND(me%textures,DIM=1),1:dim) = me%textures
                CALL MOVE_ALLOC(dummy, me%textures)
                i = i + 1
            ELSE
                EXIT
            END IF
        END DO

100     FORMAT((a))
101     FORMAT(*(es12.5))
        END SUBROUTINE texture_read

        SUBROUTINE texture_write (me, unit)
        !>@brief
        !> Subroutine performs the write for a texture attribute
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 12/13/2017
        CLASS(texture), INTENT(IN) :: me
        INTEGER(i4k),  INTENT(IN) :: unit
        INTEGER(i4k) :: i

        WRITE(unit,100) me%dataname, SIZE(me%textures,DIM=2), me%datatype
        DO i = 1, SIZE(me%textures,DIM=1)
            WRITE(unit,101) me%textures(i,:)
        END DO

100     FORMAT('TEXTURE_COORDINATES ',(a),' ',(i1),' ',(a))
101     FORMAT(*(es12.5))
        END SUBROUTINE texture_write

        SUBROUTINE texture_setup (me, dataname, datatype, numcomp, tablename, values1d, values2d)
        !>@brief
        !> Subroutine performs the set-up for a texture attribute
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 12/14/2017
        CLASS(texture),    INTENT(INOUT) :: me
        CHARACTER(LEN=*), INTENT(IN)    :: dataname
        INTEGER(i4k),     INTENT(IN), OPTIONAL :: numcomp
        CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: datatype, tablename
!        REAL(r8k), DIMENSION(..), INTENT(IN), OPTIONAL :: values
        REAL(r8k), DIMENSION(:),   INTENT(IN), OPTIONAL :: values1d
        REAL(r8k), DIMENSION(:,:), INTENT(IN), OPTIONAL :: values2d

        me%dataname = dataname
        IF (PRESENT(datatype)) THEN
            me%datatype = datatype
        ELSE
            me%datatype = 'double'
        END IF
        IF (.NOT. PRESENT(values2d)) THEN
            ERROR STOP 'Must provide textures in texture_setup'
        ELSE
            !! TODO: Implement this once SELECT RANK is incorporated into compilers (Fortran 2015)
!            SELECT RANK (values)
!            RANK(2)
!                me%textures = values
!            RANK DEFAULT
!                ERROR STOP 'Bad rank for values. Must be RANK=2. Execution terminated in Subroutine: texture_setup'
!            END SELECT
            me%textures = values2d
        END IF

        END SUBROUTINE texture_setup

        FUNCTION check_for_diffs_texture (me, you) RESULT (diffs)
        !>@brief
        !> Function checks for differences in a texture attribute
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 12/14/2017
        CLASS(texture),    INTENT(IN) :: me
        CLASS(attribute), INTENT(IN) :: you
        INTEGER(i4k) :: i, j
        LOGICAL :: diffs

        diffs = .FALSE.
        IF (.NOT. SAME_TYPE_AS(me,you)) THEN
            diffs = .TRUE.
        ELSE
            SELECT TYPE (you)
            CLASS IS (texture)
                IF (me%dataname /= you%dataname)        THEN
                    diffs = .TRUE.
                ELSE IF (me%datatype /= you%datatype)   THEN
                    diffs = .TRUE.
                ELSE
                    DO i = 1, UBOUND(me%textures,DIM=1)
                        DO j = 1, UBOUND(me%textures,DIM=2)
                            IF (me%textures(i,j) /= you%textures(i,j))     THEN
                                diffs = .TRUE.
                            END IF
                        END DO
                    END DO
                END IF
            END SELECT
        END IF

        END FUNCTION check_for_diffs_texture
END MODULE vtk_attributes
