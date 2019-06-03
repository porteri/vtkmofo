SUBMODULE (vtk_attributes) vtk_attributes_implementation
    USE Precision, ONLY : i4k, r8k
    USE Misc,      ONLY : def_len
    IMPLICIT NONE
    !! author: Ian Porter
    !! date: 12/13/2017
    !!
    !! This module contains the dataset attributes for vtk format
    !!
    !! The following dataset attributes are available:
    !! 1) scalars
    !! 2) vectors
    !! 3) normals
    !! 4) texture coordinates (1D, 2D & 3D)
    !! 5) 3x3 tensors
    !! 6) field data
    !!
    !! Possible data types:
    !! bit, unsigned_char, char, unsigned_short, short, unsigned_int, int,
    !! unsigned_long, long, float, or double.
    CHARACTER(LEN=*), PARAMETER :: default = 'default'     !! Default table name

    CONTAINS

        MODULE PROCEDURE abs_read
        !! author: Ian Porter
        !! date: 12/13/2017
        !!
        !! Abstract for reading an attribute
        !!
        SELECT TYPE (me)
        CLASS IS (attribute)
            READ(unit,*) me%dataname !! Workaround for ifort 2018 linux compiler error (not error for 2018 on Windows)
                                     !! that a class with intent(out) was not provided a value
        END SELECT
        END PROCEDURE abs_read

        MODULE PROCEDURE abs_write
        !! author: Ian Porter
        !! date: 12/13/2017
        !!
        !! Abstract for writing an attribute
        !!
        SELECT TYPE (me)
        CLASS IS (attribute)
            WRITE(unit,*) me%dataname
        END SELECT
        END PROCEDURE abs_write

        MODULE PROCEDURE initialize
        !! author: Ian Porter
        !! date: 12/13/2017
        !!
        !! Abstract for performing the set-up of an attribute
        !!
        SELECT TYPE (me)
        CLASS IS (scalar)
            CALL me%setup(dataname, datatype, numcomp, tablename, ints1d, real1d)
        CLASS IS (vector)
            CALL me%setup(dataname, datatype, ints2d, real2d)
        CLASS IS (normal)
            CALL me%setup(dataname, datatype, ints2d, real2d)
        CLASS IS (texture)
            CALL me%setup(dataname, datatype, ints2d, real2d)
        CLASS IS (tensor)
            CALL me%setup(dataname, datatype, ints3d, real3d)
        CLASS IS (field)
            CALL me%setup(dataname, datatype, field_arrays)
        CLASS DEFAULT
            ERROR STOP 'Generic class not defined for vtkmofo class attribute'
        END SELECT

        END PROCEDURE initialize

        MODULE PROCEDURE check_for_diffs
        !! author: Ian Porter
        !! date: 12/13/2017
        !!
        !! Function checks for differences in an attribute
        !!
        diffs = .FALSE.
        IF      (.NOT. SAME_TYPE_AS(me,you))  THEN
            diffs = .TRUE.
        ELSE IF (me%dataname /= you%dataname) THEN
            diffs = .TRUE.
        END IF

        END PROCEDURE check_for_diffs
!********
! Scalars
!********
        MODULE PROCEDURE scalar_read
        USE Misc, ONLY : interpret_string, to_lowercase
        !! author: Ian Porter
        !! date: 12/13/2017
        !!
        !! Subroutine performs the read for a scalar attribute
        !!
        INTEGER(i4k)               :: i, iostat
        LOGICAL                    :: end_of_file
        CHARACTER(LEN=def_len)     :: line
        INTEGER(i4k),     DIMENSION(:), ALLOCATABLE :: ints
        REAL(r8k),        DIMENSION(:), ALLOCATABLE :: reals, dummy
        CHARACTER(LEN=:), DIMENSION(:), ALLOCATABLE :: chars

        READ(unit,100) line
        CALL interpret_string (line=line, datatype=[ 'C','C','I' ], ignore='SCALARS ', separator=' ', &
          &                    ints=ints, chars=chars)
        me%numcomp = ints(1); me%dataname = TRIM(chars(1)); me%datatype = TRIM(chars(2))
        DEALLOCATE(ints)

        READ(unit,100) line
        CALL interpret_string (line=line, datatype=[ 'C' ], ignore='LOOKUP_TABLE ', separator=' ', chars=chars)
        me%tablename = TRIM(chars(1))

        me%datatype = to_lowercase(me%datatype)
        SELECT CASE (me%datatype)
        CASE ('unsigned_int', 'int')
            ALLOCATE(me%ints(0))
        CASE ('float', 'double')
            ALLOCATE(me%reals(0))
        CASE DEFAULT
            ERROR STOP 'datatype not supported in scalar_read'
        END SELECT

        end_of_file  = .FALSE.; i = 0

        get_scalars: DO
            READ(unit,100,iostat=iostat) line
            end_of_file = (iostat < 0)
            IF (end_of_file) THEN
                EXIT get_scalars
            ELSE IF (TRIM(line) == '') THEN
                CYCLE     !! Skip blank lines
            ELSE
                SELECT CASE (me%datatype)
                CASE ('unsigned_int', 'int')
                    ALLOCATE(ints(1:UBOUND(me%ints,DIM=1)+1),source=0_i4k)
                    IF (i > 0) ints(1:UBOUND(me%ints,DIM=1)) = me%ints
                    CALL MOVE_ALLOC(ints, me%ints)
                    i = i + 1

                    CALL interpret_string (line=line, datatype=[ 'I' ], separator=' ', ints=ints)
                    me%ints(i) = ints(1)
                    DEALLOCATE(ints)
                CASE ('float', 'double')
                    ALLOCATE(dummy(1:UBOUND(me%reals,DIM=1)+1),source=0.0_r8k)
                    IF (i > 0) dummy(1:UBOUND(me%reals,DIM=1)) = me%reals
                    CALL MOVE_ALLOC(dummy, me%reals)
                    i = i + 1

                    CALL interpret_string (line=line, datatype=[ 'R' ], separator=' ', reals=reals)
                    me%reals(i) = reals(1)
                CASE DEFAULT
                    ERROR STOP 'datatype not supported in scalar_read'
                END SELECT
            END IF
        END DO get_scalars

100     FORMAT((a))
        END PROCEDURE scalar_read

        MODULE PROCEDURE scalar_write
        !! author: Ian Porter
        !! date: 12/13/2017
        !!
        !! Subroutine performs the write for a scalar attribute
        !!
        INTEGER(i4k) :: i

        WRITE(unit,100) me%dataname, me%datatype, me%numcomp
        WRITE(unit,101) me%tablename
        IF (ALLOCATED(me%reals)) THEN
            DO i = 1, SIZE(me%reals)
                WRITE(unit,102) me%reals(i)
            END DO
        ELSE IF (ALLOCATED(me%ints)) THEN
            DO i = 1, SIZE(me%ints)
                WRITE(unit,103) me%ints(i)
            END DO
        ELSE
            ERROR STOP 'Neither real or integer arrays are allocated for scalar_write'
        END IF

100     FORMAT('SCALARS ',(a),' ',(a),' ',(i1))
101     FORMAT('LOOKUP_TABLE ',(a))
102     FORMAT(es13.6)
103     FORMAT(i0)

        END PROCEDURE scalar_write

        MODULE PROCEDURE scalar_setup
        !! author: Ian Porter
        !! date: 12/13/2017
        !!
        !! Subroutine performs the set-up for a scalar attribute
        !!
        me%dataname = dataname
        IF (PRESENT(datatype)) THEN
            me%datatype = datatype
        ELSE IF (PRESENT(ints1d)) THEN
            me%datatype = 'int'
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
        IF (PRESENT(ints1d)) THEN
            me%ints = ints1d
        ELSE IF (PRESENT(real1d)) THEN
            me%reals = real1d
        ELSE
            ERROR STOP 'Must provide either array of integers or reals in scalar_setup'
        END IF

        END PROCEDURE scalar_setup

        MODULE PROCEDURE check_for_diffs_scalar
        !! author: Ian Porter
        !! date: 12/13/2017
        !!
        !! Function checks for differences in a scalar attribute
        !!
        INTEGER(i4k) :: i

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
                ELSE IF (ALLOCATED(me%reals))           THEN
                    DO i = 1, UBOUND(me%reals,DIM=1)
                        IF (me%reals(i) /= you%reals(i))THEN
                            diffs = .TRUE.
                        END IF
                    END DO
                ELSE IF (ALLOCATED(me%ints))            THEN
                    DO i = 1, UBOUND(me%ints,DIM=1)
                        IF (me%ints(i) /= you%ints(i))  THEN
                            diffs = .TRUE.
                        END IF
                    END DO
                END IF
            END SELECT
        END IF

        END PROCEDURE check_for_diffs_scalar
!********
! Vectors
!********
        MODULE PROCEDURE vector_read
        USE Misc, ONLY : interpret_string, to_lowercase
        !! author: Ian Porter
        !! date: 12/14/2017
        !!
        !! Subroutine performs the read for a vector attribute
        !!
        INTEGER(i4k)               :: i, iostat
        INTEGER(i4k),  PARAMETER   :: dim = 3
        LOGICAL                    :: end_of_file
        CHARACTER(LEN=def_len)     :: line
        INTEGER(i4k),     DIMENSION(:),   ALLOCATABLE :: ints
        REAL(r8k),        DIMENSION(:),   ALLOCATABLE :: reals
        CHARACTER(LEN=:), DIMENSION(:),   ALLOCATABLE :: chars
        INTEGER(i4k),     DIMENSION(:,:), ALLOCATABLE :: i_dummy
        REAL(r8k),        DIMENSION(:,:), ALLOCATABLE :: r_dummy

        READ(unit,100) line
        CALL interpret_string (line=line, datatype=[ 'C','C' ], ignore='VECTORS ', separator=' ', chars=chars)
        me%dataname = TRIM(chars(1)); me%datatype = to_lowercase(TRIM(chars(2)))

        SELECT CASE (me%datatype)
        CASE ('unsigned_int', 'int')
            ALLOCATE(me%i_vector(0,0))
        CASE ('float', 'double')
            ALLOCATE(me%r_vector(0,0))
        CASE DEFAULT
            ERROR STOP 'datatype not supported in scalar_read'
        END SELECT

        end_of_file = .FALSE.; i = 0

        get_vectors: DO
            READ(unit,100,iostat=iostat) line
            end_of_file = (iostat < 0)
            IF (end_of_file) THEN
                EXIT get_vectors
            ELSE IF (TRIM(line) == '') THEN
                CYCLE     !! Skip blank lines
            ELSE
                SELECT CASE (me%datatype)
                CASE ('unsigned_int', 'int')
                    ALLOCATE(i_dummy(1:UBOUND(me%i_vector,DIM=1)+1,1:dim),source=0_i4k)
                    IF (i > 0) i_dummy(1:UBOUND(me%i_vector,DIM=1),1:dim) = me%i_vector
                    CALL MOVE_ALLOC(i_dummy, me%i_vector)
                    i = i + 1

                    CALL interpret_string (line=line, datatype=[ 'I','I','I' ], separator=' ', ints=ints)
                    me%i_vector(i,1:dim) = ints(1:dim)
                CASE ('float', 'double')
                    ALLOCATE(r_dummy(1:UBOUND(me%r_vector,DIM=1)+1,1:dim),source=0.0_r8k)
                    IF (i > 0) r_dummy(1:UBOUND(me%r_vector,DIM=1),1:dim) = me%r_vector
                    CALL MOVE_ALLOC(r_dummy, me%r_vector)
                    i = i + 1

                    CALL interpret_string (line=line, datatype=[ 'R','R','R' ], separator=' ', reals=reals)
                    me%r_vector(i,1:dim) = reals(1:dim)
                END SELECT
            END IF
        END DO get_vectors

100     FORMAT((a))
        END PROCEDURE vector_read

        MODULE PROCEDURE vector_write
        !! author: Ian Porter
        !! date: 12/13/2017
        !!
        !! Subroutine performs the write for a vector attribute
        !!
        INTEGER(i4k) :: i

        WRITE(unit,100) me%dataname, me%datatype
        IF (ALLOCATED(me%i_vector)) THEN
            DO i = 1, SIZE(me%i_vector,DIM=1)
                WRITE(unit,101) me%i_vector(i,1:3)
            END DO
        ELSE IF (ALLOCATED(me%r_vector)) THEN
            DO i = 1, SIZE(me%r_vector,DIM=1)
                WRITE(unit,102) me%r_vector(i,1:3)
            END DO
        END IF

100     FORMAT('VECTORS ',(a),' ',(a))
101     FORMAT(*(i8,' '))
102     FORMAT(*(es13.6,' '))
        END PROCEDURE vector_write

        MODULE PROCEDURE vector_setup
        !! author: Ian Porter
        !! date: 12/14/2017
        !!
        !! Subroutine performs the set-up for a vector attribute
        !!
        me%dataname = dataname
        IF (PRESENT(datatype)) THEN
            me%datatype = datatype
        ELSE
            me%datatype = 'double'
        END IF
        IF (PRESENT(ints2d)) THEN
            IF (me%datatype == 'double') me%datatype = 'int'
            ALLOCATE(me%i_vector, source=ints2d)
        ELSE IF (PRESENT(real2d)) THEN
            ALLOCATE(me%r_vector, source=real2d)
        ELSE
            ERROR STOP 'Error: Must provide either ints2d or real2d in vector_setup'
        END IF

        END PROCEDURE vector_setup

        MODULE PROCEDURE check_for_diffs_vector
        !! author: Ian Porter
        !! date: 12/14/2017
        !!
        !! Function checks for differences in a vector attribute
        !!
        INTEGER(i4k) :: i, j

        diffs = .FALSE.
        IF (.NOT. SAME_TYPE_AS(me,you)) THEN
            diffs = .TRUE.
        ELSE
            SELECT TYPE (you)
            CLASS IS (vector)
                IF (me%dataname /= you%dataname)      THEN
                    diffs = .TRUE.
                ELSE IF (me%datatype /= you%datatype) THEN
                    diffs = .TRUE.
                ELSE IF (ALLOCATED(me%i_vector)) THEN
                    DO i = 1, UBOUND(me%i_vector,DIM=1)
                        DO j = 1, UBOUND(me%i_vector,DIM=2)
                            IF (me%i_vector(i,j) /= you%i_vector(i,j)) THEN
                                diffs = .TRUE.
                            END IF
                        END DO
                    END DO
                ELSE IF (ALLOCATED(me%r_vector)) THEN
                    DO i = 1, UBOUND(me%r_vector,DIM=1)
                        DO j = 1, UBOUND(me%r_vector,DIM=2)
                            IF (me%r_vector(i,j) /= you%r_vector(i,j)) THEN
                                diffs = .TRUE.
                            END IF
                        END DO
                    END DO
                ELSE
                    diffs = .TRUE.
                END IF
            END SELECT
        END IF

        END PROCEDURE check_for_diffs_vector
!********
! Normals
!********
        MODULE PROCEDURE normal_read
        USE Misc, ONLY : interpret_string, to_lowercase
        !! author: Ian Porter
        !! date: 12/14/2017
        !!
        !! Subroutine performs the read for a normal attribute
        !!
        INTEGER(i4k)               :: i, iostat
        INTEGER(i4k),  PARAMETER   :: dim = 3
        LOGICAL                    :: end_of_file
        CHARACTER(LEN=def_len)     :: line
        INTEGER(i4k),     DIMENSION(:),   ALLOCATABLE :: ints
        REAL(r8k),        DIMENSION(:),   ALLOCATABLE :: reals
        CHARACTER(LEN=:), DIMENSION(:),   ALLOCATABLE :: chars
        INTEGER(i4k),     DIMENSION(:,:), ALLOCATABLE :: i_dummy
        REAL(r8k),        DIMENSION(:,:), ALLOCATABLE :: r_dummy

        READ(unit,100) line
        CALL interpret_string (line=line, datatype=[ 'C','C' ], ignore='NORMALS ', separator=' ', chars=chars)
        me%dataname = TRIM(chars(1)); me%datatype = to_lowercase(TRIM(chars(2)))

        SELECT CASE (me%datatype)
        CASE ('unsigned_int', 'int')
            ALLOCATE(me%i_normal(0,0))
        CASE ('float', 'double')
            ALLOCATE(me%r_normal(0,0))
        CASE DEFAULT
            ERROR STOP 'datatype not supported in scalar_read'
        END SELECT

        end_of_file = .FALSE.; i = 0

        get_normals: DO
            READ(unit,100,iostat=iostat) line
            end_of_file = (iostat < 0)
            IF (end_of_file) THEN
                EXIT get_normals
            ELSE IF (TRIM(line) == '') THEN
                CYCLE     !! Skip blank lines
            ELSE
                SELECT CASE (me%datatype)
                CASE ('unsigned_int', 'int')
                    ALLOCATE(i_dummy(1:UBOUND(me%i_normal,DIM=1)+1,1:dim),source=0_i4k)
                    IF (i > 0) i_dummy(1:UBOUND(me%i_normal,DIM=1),1:dim) = me%i_normal
                    CALL MOVE_ALLOC(i_dummy, me%i_normal)
                    i = i + 1

                    CALL interpret_string (line=line, datatype=[ 'I','I','I' ], separator=' ', ints=ints)
                    me%i_normal(i,1:dim) = ints(1:dim)
                CASE ('float', 'double')
                    ALLOCATE(r_dummy(1:UBOUND(me%r_normal,DIM=1)+1,1:dim),source=0.0_r8k)
                    IF (i > 0) r_dummy(1:UBOUND(me%r_normal,DIM=1),1:dim) = me%r_normal
                    CALL MOVE_ALLOC(r_dummy, me%r_normal)
                    i = i + 1

                    CALL interpret_string (line=line, datatype=[ 'R','R','R' ], separator=' ', reals=reals)
                    me%r_normal(i,1:dim) = reals(1:dim)
                END SELECT
            END IF
        END DO get_normals

100     FORMAT((a))
        END PROCEDURE normal_read

        MODULE PROCEDURE normal_write
        !! author: Ian Porter
        !! date: 12/13/2017
        !!
        !! Subroutine performs the write for a normal attribute
        !!
        INTEGER(i4k) :: i

        WRITE(unit,100) me%dataname, me%datatype
        IF (ALLOCATED(me%i_normal)) THEN
            DO i = 1, SIZE(me%i_normal,DIM=1)
                WRITE(unit,101) me%i_normal(i,1:3)
            END DO
        ELSE IF (ALLOCATED(me%r_normal)) THEN
            DO i = 1, SIZE(me%r_normal,DIM=1)
                WRITE(unit,102) me%r_normal(i,1:3)
            END DO
        END IF

100     FORMAT('NORMALS ',(a),' ',(a))
101     FORMAT(*(i8,' '))
102     FORMAT(*(es13.6,' '))
        END PROCEDURE normal_write

        MODULE PROCEDURE normal_setup
        !! author: Ian Porter
        !! date: 12/14/2017        
        !!
        !! Subroutine performs the set-up for a normal attribute
        !!
        me%dataname = dataname
        IF (PRESENT(datatype)) THEN
            me%datatype = datatype
        ELSE
            me%datatype = 'double'
        END IF
        IF (PRESENT(ints2d)) THEN
            IF (me%datatype == 'double') me%datatype = 'int'
            ALLOCATE(me%i_normal, source=ints2d)
        ELSE IF (PRESENT(real2d)) THEN
            ALLOCATE(me%r_normal, source=real2d)
        ELSE
            ERROR STOP 'Error: Must provide either ints2d or real2d in normal_setup'
        END IF

        END PROCEDURE normal_setup

        MODULE PROCEDURE check_for_diffs_normal
        !! author: Ian Porter
        !! date: 12/14/2017
        !!
        !! Function checks for differences in a normal attribute
        !!
        INTEGER(i4k) :: i, j

        diffs = .FALSE.
        IF (.NOT. SAME_TYPE_AS(me,you)) THEN
            diffs = .TRUE.
        ELSE
            SELECT TYPE (you)
            CLASS IS (normal)
                IF (me%dataname /= you%dataname)      THEN
                    diffs = .TRUE.
                ELSE IF (me%datatype /= you%datatype) THEN
                    diffs = .TRUE.
                ELSE IF (ALLOCATED(me%i_normal)) THEN
                    DO i = 1, UBOUND(me%i_normal,DIM=1)
                        DO j = 1, UBOUND(me%i_normal,DIM=2)
                            IF (me%i_normal(i,j) /= you%i_normal(i,j)) THEN
                                diffs = .TRUE.
                            END IF
                        END DO
                    END DO
                ELSE IF (ALLOCATED(me%r_normal)) THEN
                    DO i = 1, UBOUND(me%r_normal,DIM=1)
                        DO j = 1, UBOUND(me%r_normal,DIM=2)
                            IF (me%r_normal(i,j) /= you%r_normal(i,j)) THEN
                                diffs = .TRUE.
                            END IF
                        END DO
                    END DO
                ELSE
                    diffs = .TRUE.
                END IF
            END SELECT
        END IF

        END PROCEDURE check_for_diffs_normal
!********
! Textures
!********
        MODULE PROCEDURE texture_read
        USE Misc, ONLY : interpret_string
        !! author: Ian Porter
        !! date: 12/14/2017
        !!
        !! Subroutine performs the read for a texture attribute
        !!
        INTEGER(i4k)                :: i, iostat, dim
        LOGICAL                     :: end_of_file
        CHARACTER(LEN=def_len)      :: line
        INTEGER(i4k),     DIMENSION(:),   ALLOCATABLE :: ints
        REAL(r8k),        DIMENSION(:),   ALLOCATABLE :: reals
        CHARACTER(LEN=:), DIMENSION(:),   ALLOCATABLE :: chars
        REAL(r8k),        DIMENSION(:,:), ALLOCATABLE :: dummy
        CHARACTER(LEN=1), DIMENSION(3),   PARAMETER   :: datatype = [ 'R','R','R' ]

        READ(unit,100) line
        CALL interpret_string (line=line, datatype=[ 'C','I','C' ], ignore='TEXTURE_COORDINATES ', separator=' ', &
          &                    ints=ints, chars=chars)
        me%dataname = TRIM(chars(1)); me%datatype = TRIM(chars(2)); dim = ints(1)

        ALLOCATE(me%textures(0,0)); end_of_file = .FALSE.; i = 0

        get_textures: DO
            READ(unit,100,iostat=iostat) line
            end_of_file = (iostat < 0)
            IF (end_of_file) THEN
                EXIT get_textures
            ELSE IF (TRIM(line) == '') THEN
                CYCLE     !! Skip blank lines
            ELSE
                ALLOCATE(dummy(1:UBOUND(me%textures,DIM=1)+1,1:dim),source=0.0_r8k)
                IF (i > 0) dummy(1:UBOUND(me%textures,DIM=1),1:dim) = me%textures
                CALL MOVE_ALLOC(dummy, me%textures)
                i = i + 1

                CALL interpret_string (line=line, datatype=datatype(1:dim), separator=' ', reals=reals)
                me%textures(i,1:dim) = reals(1:dim)
            END IF
        END DO get_textures

100     FORMAT((a))
        END PROCEDURE texture_read

        MODULE PROCEDURE texture_write
        !! author: Ian Porter
        !! date: 12/13/2017
        !!
        !! Subroutine performs the write for a texture attribute
        !!
        INTEGER(i4k) :: i

        WRITE(unit,100) me%dataname, SIZE(me%textures,DIM=2), me%datatype
        DO i = 1, SIZE(me%textures,DIM=1)
            WRITE(unit,101) me%textures(i,:)
        END DO

100     FORMAT('TEXTURE_COORDINATES ',(a),' ',(i1),' ',(a))
101     FORMAT(*(es13.6,' '))
        END PROCEDURE texture_write

        MODULE PROCEDURE texture_setup
        !! author: Ian Porter
        !! date: 12/14/2017
        !!
        !! Subroutine performs the set-up for a texture attribute
        !!
        me%dataname = dataname
        IF (PRESENT(datatype)) THEN
            me%datatype = datatype
        ELSE
            me%datatype = 'double'
        END IF
        me%textures = real2d

        END PROCEDURE texture_setup

        MODULE PROCEDURE check_for_diffs_texture
        !! author: Ian Porter
        !! date: 12/14/2017
        !!
        !! Function checks for differences in a texture attribute
        !!
        INTEGER(i4k) :: i, j

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

        END PROCEDURE check_for_diffs_texture
!********
! Tensors
!********
        MODULE PROCEDURE tensor_read
        USE Misc, ONLY : interpret_string, to_lowercase
        !! author: Ian Porter
        !! date: 12/14/2017
        !!
        !! Subroutine performs the read for a tensor attribute
        !!
        INTEGER(i4k)               :: i, j, iostat
        LOGICAL                    :: end_of_file
        CHARACTER(LEN=def_len)     :: line
        INTEGER(i4k),         DIMENSION(:), ALLOCATABLE :: ints
        REAL(r8k),            DIMENSION(:), ALLOCATABLE :: reals
        CHARACTER(LEN=:),     DIMENSION(:), ALLOCATABLE :: chars
        TYPE(r_tensor_array), DIMENSION(:), ALLOCATABLE :: r_dummy
        TYPE(i_tensor_array), DIMENSION(:), ALLOCATABLE :: i_dummy

        READ(unit,100) line
        CALL interpret_string (line=line, datatype=[ 'C','C' ], ignore='TENSORS ', separator=' ', &
          &                    chars=chars)
        me%dataname = TRIM(chars(1)); me%datatype = to_lowercase(TRIM(chars(2)))

        SELECT CASE (me%datatype)
        CASE ('unsigned_int', 'int')
            ALLOCATE(me%i_tensor(0))
        CASE ('float', 'double')
            ALLOCATE(me%r_tensor(0))
        CASE DEFAULT
            ERROR STOP 'Unsupported data type for tensor.'
        END SELECT
        
        end_of_file = .FALSE.; i = 0

        get_tensors: DO
            READ(unit,100,iostat=iostat) line
            end_of_file = (iostat < 0)
            IF (end_of_file) THEN
                EXIT get_tensors
            ELSE IF (TRIM(line) == '') THEN
                CYCLE      !! Skip blank lines
            ELSE
                SELECT CASE (me%datatype)
                CASE ('unsigned_int', 'int')
                    !! Integers
                    ALLOCATE(i_dummy(1:UBOUND(me%i_tensor,DIM=1)+1))
                    i_dummy(1:UBOUND(me%i_tensor,DIM=1)) = me%i_tensor
                    CALL MOVE_ALLOC(i_dummy, me%i_tensor)
                    i = i + 1

                    DO j = 1, UBOUND(me%i_tensor(i)%val,DIM=1)
                        IF (j > 1) READ(unit,100,iostat=iostat) line
                        CALL interpret_string (line=line, datatype=[ 'I','I','I' ], separator=' ', ints=ints)
                        me%i_tensor(i)%val(j,1:3) = ints(1:3)
                    END DO
                CASE ('float', 'double')
                    !! Reals
                    ALLOCATE(r_dummy(1:UBOUND(me%r_tensor,DIM=1)+1))
                    r_dummy(1:UBOUND(me%r_tensor,DIM=1)) = me%r_tensor
                    CALL MOVE_ALLOC(r_dummy, me%r_tensor)
                    i = i + 1

                    DO j = 1, UBOUND(me%r_tensor(i)%val,DIM=1)
                        IF (j > 1) READ(unit,100,iostat=iostat) line
                        CALL interpret_string (line=line, datatype=[ 'R','R','R' ], separator=' ', reals=reals)
                        me%r_tensor(i)%val(j,1:3) = reals(1:3)
                    END DO
                END SELECT
            END IF
        END DO get_tensors

100     FORMAT((a))
        END PROCEDURE tensor_read

        MODULE PROCEDURE tensor_write
        !! author: Ian Porter
        !! date: 12/13/2017
        !!
        !! Subroutine performs the write for a tensor attribute
        !!
        INTEGER(i4k) :: i, j

        WRITE(unit,100) me%dataname, me%datatype
        IF (ALLOCATED(me%i_tensor)) THEN
            DO i = 1, SIZE(me%i_tensor,DIM=1)
                DO j = 1, SIZE(me%i_tensor(i)%val,DIM=1)
                    WRITE(unit,101) me%i_tensor(i)%val(j,:)
                END DO
                WRITE(unit,105)
            END DO
        ELSE IF (ALLOCATED(me%r_tensor)) THEN
            DO i = 1, SIZE(me%r_tensor,DIM=1)
                DO j = 1, SIZE(me%r_tensor(i)%val,DIM=1)
                    WRITE(unit,102) me%r_tensor(i)%val(j,:)
                END DO
                WRITE(unit,105)
            END DO
        END IF

100     FORMAT('TENSORS ',(a),' ',(a))
101     FORMAT(*(i8,' '))
102     FORMAT(*(es13.6,' '))
105     FORMAT()
        END PROCEDURE tensor_write

        MODULE PROCEDURE tensor_setup
        !! author: Ian Porter
        !! date: 12/14/2017
        !!
        !! Subroutine performs the set-up for a tensor attribute
        !!
        INTEGER(i4k) :: i

        me%dataname = dataname
        IF (PRESENT(datatype)) THEN
            me%datatype = datatype
        ELSE IF (PRESENT(ints3d)) THEN
            me%datatype = 'int'
        ELSE
            me%datatype = 'double'
        END IF
        IF (PRESENT(ints3d)) THEN
            IF (SIZE(ints3d,DIM=2) /= 3 .OR. SIZE(ints3d,DIM=3) /= 3) THEN
                ERROR STOP 'Tensors can only be 3x3'
            ELSE
                ALLOCATE(me%i_tensor(1:UBOUND(ints3d,DIM=1)))
                DO i = 1, UBOUND(ints3d,DIM=1)
                    me%i_tensor(i)%val(1:3,1:3) = ints3d(i,1:3,1:3)
                END DO
            END IF
        ELSE IF (PRESENT(real3d)) THEN
            IF (SIZE(real3d,DIM=2) /= 3 .OR. SIZE(real3d,DIM=3) /= 3) THEN
                ERROR STOP 'Tensors can only be 3x3'
            ELSE
                ALLOCATE(me%r_tensor(1:UBOUND(real3d,DIM=1)))
                DO i = 1, UBOUND(real3d,DIM=1)
                    me%r_tensor(i)%val(1:3,1:3) = real3d(i,1:3,1:3)
                END DO
            END IF
        ELSE
            ERROR STOP 'Error: Must provide either ints3d or real3d in tensor_setup'
        END IF

        END PROCEDURE tensor_setup

        MODULE PROCEDURE check_for_diffs_tensor
        !! author: Ian Porter
        !! date: 12/14/2017
        !!
        !! Function checks for differences in a tensor attribute
        !!
        INTEGER(i4k) :: i, j, k

        diffs = .FALSE.
        IF (.NOT. SAME_TYPE_AS(me,you)) THEN
            diffs = .TRUE.
        ELSE
            SELECT TYPE (you)
            CLASS IS (tensor)
                IF (me%dataname /= you%dataname)        THEN
                    diffs = .TRUE.
                ELSE IF (me%datatype /= you%datatype)   THEN
                    diffs = .TRUE.
                ELSE IF (ALLOCATED(me%i_tensor)) THEN
                        DO i = 1, UBOUND(me%i_tensor,DIM=1)
                            DO j = 1, UBOUND(me%i_tensor(i)%val,DIM=1)
                                DO k = 1, UBOUND(me%i_tensor(i)%val,DIM=2)
                                    IF (me%i_tensor(i)%val(j,k) /= you%i_tensor(i)%val(j,k)) THEN
                                        diffs = .TRUE.
                                    END IF
                                END DO
                            END DO
                        END DO
                ELSE IF (ALLOCATED(me%r_tensor)) THEN
                    DO i = 1, UBOUND(me%r_tensor,DIM=1)
                        DO j = 1, UBOUND(me%r_tensor(i)%val,DIM=1)
                            DO k = 1, UBOUND(me%r_tensor(i)%val,DIM=2)
                                IF (me%r_tensor(i)%val(j,k) /= you%r_tensor(i)%val(j,k)) THEN
                                    diffs = .TRUE.
                                END IF
                            END DO
                        END DO
                    END DO
                ELSE
                    diffs = .TRUE.
                END IF
            END SELECT
        END IF

        END PROCEDURE check_for_diffs_tensor
!********
! Fields
!********
        MODULE PROCEDURE field_read
        USE Misc, ONLY : interpret_string
        !! author: Ian Porter
        !! date: 12/14/2017
        !!
        !! Subroutine performs the read for a field attribute
        !!
        INTEGER(i4k)              :: i, j, iostat, dim
        LOGICAL                   :: end_of_file
        CHARACTER(LEN=def_len)    :: line
        CHARACTER(*), PARAMETER   :: real_char = 'R'
        REAL(r8k),              DIMENSION(:), ALLOCATABLE :: reals
        INTEGER(i4k),           DIMENSION(:), ALLOCATABLE :: ints
        CHARACTER(LEN=:),       DIMENSION(:), ALLOCATABLE :: chars
        CHARACTER(LEN=1),       DIMENSION(:), ALLOCATABLE :: datatype
!        TYPE(field_data_array), DIMENSION(:), ALLOCATABLE :: dummy

        READ(unit,100) line
        CALL interpret_string (line=line, datatype=[ 'C','I' ], ignore='FIELD ', separator=' ', &
          &                    ints=ints, chars=chars)
        me%dataname = TRIM(chars(1)); dim = ints(1)

        ALLOCATE(me%array(1:dim)); end_of_file = .FALSE.; i = 0

        get_fields: DO
            READ(unit,100,iostat=iostat) line
            end_of_file = (iostat < 0)
            IF (end_of_file) THEN
                EXIT get_fields
            ELSE IF (TRIM(line) == '') THEN
                CYCLE      !! Skip blank lines
            ELSE
!                ALLOCATE(dummy(1:UBOUND(me%array,DIM=1)))
!                dummy(1:UBOUND(me%array,DIM=1)) = me%array
!                CALL MOVE_ALLOC(dummy, me%array)
                i = i + 1

                CALL interpret_string (line=line, datatype=[ 'C','I','I','C' ], separator=' ', chars=chars, ints=ints)
                me%array(i)%name = TRIM(chars(1)); me%array(i)%numComponents = ints(1)
                me%array(i)%numTuples = ints(2); me%array(i)%datatype = chars(2)
                ALLOCATE(datatype(1:me%array(i)%numComponents),source=real_char)
                ALLOCATE(me%array(i)%data(1:me%array(i)%numTuples,1:me%array(i)%numComponents),source=0.0_r8k)

                DO j = 1, me%array(i)%numTuples
                    READ(unit,100,iostat=iostat) line
                    CALL interpret_string (line=line, datatype=datatype, separator=' ', reals=reals)
                    me%array(i)%data(j,:) = reals(:)
                END DO
                DEALLOCATE(datatype)

            END IF
        END DO get_fields

100     FORMAT((a))
        END PROCEDURE field_read

        MODULE PROCEDURE field_write
        !! author: Ian Porter
        !! date: 12/13/2017        
        !!
        !! Subroutine performs the write for a field attribute
        !!
        INTEGER(i4k) :: i, j

        WRITE(unit,100) me%dataname, SIZE(me%array,DIM=1)
        DO i = 1, SIZE(me%array,DIM=1)
            WRITE(unit,101) me%array(i)%name, me%array(i)%numComponents, me%array(i)%numTuples, me%array(i)%datatype
            DO j = 1, me%array(i)%numTuples
                WRITE(unit,102) me%array(i)%data(j,:)
            END DO
            WRITE(unit,103)
        END DO

100     FORMAT('FIELD ',(a),' ',(i0))
101     FORMAT((a),' ',(i0),' ',(i0),' ',(a))
102     FORMAT(*(es13.6,' '))
103     FORMAT()
        END PROCEDURE field_write

        MODULE PROCEDURE field_setup
        !! author: Ian Porter
        !! date: 12/14/2017
        !!
        !! Subroutine performs the set-up for a field attribute
        !!
        me%dataname = dataname
        IF (PRESENT(datatype)) THEN
            me%datatype = datatype
        ELSE
            me%datatype = 'double'
        END IF
        me%array = field_arrays

        END PROCEDURE field_setup

        MODULE PROCEDURE check_for_diffs_field
        !! author: Ian Porter
        !! date: 12/14/2017
        !!
        !! Function checks for differences in a field attribute
        !!
        INTEGER(i4k) :: i, j, k

        diffs = .FALSE.
        IF (.NOT. SAME_TYPE_AS(me,you)) THEN
            diffs = .TRUE.
        ELSE
            SELECT TYPE (you)
            CLASS IS (field)
                IF      (me%dataname /= you%dataname) THEN
                    diffs = .TRUE.
                ELSE
                    DO i = 1, UBOUND(me%array,DIM=1)
                        IF      (me%array(i)%name          /= you%array(i)%name         ) THEN
                            diffs = .TRUE.
                        ELSE IF (me%array(i)%numComponents /= you%array(i)%numComponents) THEN
                            diffs = .TRUE.
                        ELSE IF (me%array(i)%numTuples     /= you%array(i)%numTuples    ) THEN
                            diffs = .TRUE.
                        ELSE IF (me%array(i)%datatype      /= you%array(i)%datatype     ) THEN
                            diffs = .TRUE.
                        ELSE
                            DO j = 1, UBOUND(me%array(i)%data,DIM=1)
                                DO k = 1, UBOUND(me%array(i)%data,DIM=2)
                                    IF (me%array(i)%data(j,k) /= me%array(i)%data(j,k)) THEN
                                        diffs = .TRUE.
                                    END IF
                                END DO
                            END DO
                        END IF
                    END DO
                END IF
            END SELECT
        END IF

        END PROCEDURE check_for_diffs_field

END SUBMODULE vtk_attributes_implementation
