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
    PUBLIC :: attribute, scalar, vector, normal, texture, tensor, field, field_data_array

    INTEGER(i4k),     PARAMETER :: def_len = 1000          !! Default character length for each line in file
    CHARACTER(LEN=*), PARAMETER :: default = 'default'     !! Default table name

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
        INTEGER(i4k) :: numcomp = 0
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

    TYPE :: tensor_array
        REAL(r8k), DIMENSION(3,3) :: val = 0.0_r8k
    END TYPE tensor_array

    TYPE, EXTENDS(attribute) :: tensor
        TYPE(tensor_array), DIMENSION(:), ALLOCATABLE :: tensors
    CONTAINS
        PROCEDURE :: read  => tensor_read
        PROCEDURE :: write => tensor_write
        PROCEDURE :: setup => tensor_setup
        PROCEDURE, PRIVATE :: check_for_diffs => check_for_diffs_tensor
    END TYPE tensor

    TYPE :: field_data_array
        CHARACTER(LEN=:), ALLOCATABLE :: name
        INTEGER(i4k) :: numComponents = 0
        INTEGER(i4k) :: numTuples     = 0
        CHARACTER(LEN=:), ALLOCATABLE :: datatype
        REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: data
    END TYPE field_data_array

    TYPE, EXTENDS(attribute) :: field
        TYPE(field_data_array), DIMENSION(:), ALLOCATABLE :: array
    CONTAINS
        PROCEDURE :: read  => field_read
        PROCEDURE :: write => field_write
        PROCEDURE :: setup => field_setup
        PROCEDURE, PRIVATE :: check_for_diffs => check_for_diffs_field
    END TYPE field

    CONTAINS
        SUBROUTINE abs_read (me, unit)
        !>@brief
        !> Abstract for reading an attribute
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 12/13/2017
        CLASS(attribute), INTENT(OUT) :: me
        INTEGER(i4k),     INTENT(IN)  :: unit
        SELECT TYPE (me)
        CLASS IS (attribute)
            READ(unit,*) me%dataname !! Workaround for ifort 2018 linux compiler error (not error for 2018 on Windows)
                                     !! that a class with intent(out) was not provided a value
        END SELECT
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
        CLASS IS (attribute)
            WRITE(unit,*) me%dataname
        END SELECT
        END SUBROUTINE abs_write

        SUBROUTINE abs_setup (me, dataname, datatype, numcomp, tablename, values1d, values2d, values3d, field_arrays)
        !>@brief
        !> Abstract for performing the set-up of an attribute
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 12/13/2017
        CLASS(attribute), INTENT(OUT) :: me
        CHARACTER(LEN=*), INTENT(IN)  :: dataname
        INTEGER(i4k),     INTENT(IN), OPTIONAL :: numcomp
        CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: datatype, tablename
!        REAL(r8k), DIMENSION(..),   INTENT(IN), OPTIONAL :: values
        REAL(r8k), DIMENSION(:),     INTENT(IN), OPTIONAL :: values1d
        REAL(r8k), DIMENSION(:,:),   INTENT(IN), OPTIONAL :: values2d
        REAL(r8k), DIMENSION(:,:,:), INTENT(IN), OPTIONAL :: values3d
        TYPE(field_data_array), DIMENSION(:), INTENT(IN), OPTIONAL :: field_arrays
        SELECT TYPE (me)
        CLASS IS (attribute)
            me%dataname = dataname   !! Workaround for ifort 2018 linux compiler error (not error for 2018 on Windows)
                                     !! that a class with intent(out) was not provided a value
            IF (PRESENT(datatype) .AND. PRESENT(numcomp)  .AND. PRESENT(tablename) .AND. &
              & PRESENT(values1d) .AND. PRESENT(values2d) .AND. PRESENT(values3d)  .AND. PRESENT(field_arrays)) THEN
                !! DO NOTHING. ONLY ELIMINATES COMPILER WARNINGS
              END IF
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
        CLASS(scalar), INTENT(OUT) :: me
        INTEGER(i4k),  INTENT(IN)  :: unit
        INTEGER(i4k)               :: i, iostat
        LOGICAL                    :: end_of_file
        CHARACTER(LEN=def_len)     :: line
        INTEGER(i4k),     DIMENSION(:), ALLOCATABLE :: ints
        REAL(r8k),        DIMENSION(:), ALLOCATABLE :: reals, dummy
        CHARACTER(LEN=:), DIMENSION(:), ALLOCATABLE :: chars

        READ(unit,100) line
        CALL interpret_string (line=line, datatype=(/ 'C','C','I' /), ignore='SCALARS ', separator=' ', &
          &                    ints=ints, chars=chars)
        me%numcomp = ints(1); me%dataname = TRIM(chars(1)); me%datatype = TRIM(chars(2))

        READ(unit,100) line
        CALL interpret_string (line=line, datatype=(/ 'C' /), ignore='LOOKUP_TABLE ', separator=' ', chars=chars)
        me%tablename = TRIM(chars(1))

        ALLOCATE(me%scalars(0)); end_of_file  = .FALSE.; i = 0

        get_scalars: DO
            READ(unit,100,iostat=iostat) line
            end_of_file = (iostat < 0)
            IF (end_of_file) THEN
                EXIT get_scalars
            ELSE IF (TRIM(line) == '') THEN
                CYCLE     !! Skip blank lines
            ELSE
                ALLOCATE(dummy(1:UBOUND(me%scalars,DIM=1)+1),source=0.0_r8k)
                IF (i > 0) dummy(1:UBOUND(me%scalars,DIM=1)) = me%scalars
                CALL MOVE_ALLOC(dummy, me%scalars)
                i = i + 1

                CALL interpret_string (line=line, datatype=(/ 'R' /), separator=' ', reals=reals)
                me%scalars(i) = reals(1)
            END IF
        END DO get_scalars

100     FORMAT((a))
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
102     FORMAT(es13.6)
        END SUBROUTINE scalar_write

        SUBROUTINE scalar_setup (me, dataname, datatype, numcomp, tablename, values1d, values2d, values3d, field_arrays)
        !>@brief
        !> Subroutine performs the set-up for a scalar attribute
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 12/13/2017
        CLASS(scalar),    INTENT(OUT) :: me
        CHARACTER(LEN=*), INTENT(IN)  :: dataname
        INTEGER(i4k),     INTENT(IN), OPTIONAL :: numcomp
        CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: datatype, tablename
!        REAL(r8k), DIMENSION(..),   INTENT(IN), OPTIONAL :: values
        REAL(r8k), DIMENSION(:),     INTENT(IN), OPTIONAL :: values1d
        REAL(r8k), DIMENSION(:,:),   INTENT(IN), OPTIONAL :: values2d
        REAL(r8k), DIMENSION(:,:,:), INTENT(IN), OPTIONAL :: values3d
        TYPE(field_data_array), DIMENSION(:), INTENT(IN), OPTIONAL :: field_arrays

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
        IF (PRESENT(values2d) .AND. PRESENT(values3d) .AND. PRESENT(field_arrays)) THEN
            !! DO NOTHING. ONLY ELIMINATES COMPILER WARNINGS
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
        LOGICAL      :: diffs

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
        CLASS(vector), INTENT(OUT) :: me
        INTEGER(i4k),  INTENT(IN)  :: unit
        INTEGER(i4k)               :: i, iostat
        INTEGER(i4k),  PARAMETER   :: dim = 3
        LOGICAL                    :: end_of_file
        CHARACTER(LEN=def_len)     :: line
        REAL(r8k),        DIMENSION(:),   ALLOCATABLE :: reals
        CHARACTER(LEN=:), DIMENSION(:),   ALLOCATABLE :: chars
        REAL(r8k),        DIMENSION(:,:), ALLOCATABLE :: dummy

        READ(unit,100) line
        CALL interpret_string (line=line, datatype=(/ 'C','C' /), ignore='VECTORS ', separator=' ', chars=chars)
        me%dataname = TRIM(chars(1)); me%datatype = TRIM(chars(2))

        ALLOCATE(me%vectors(0,0)); end_of_file = .FALSE.; i = 0

        get_vectors: DO
            READ(unit,100,iostat=iostat) line
            end_of_file = (iostat < 0)
            IF (end_of_file) THEN
                EXIT get_vectors
            ELSE IF (TRIM(line) == '') THEN
                CYCLE     !! Skip blank lines
            ELSE
                ALLOCATE(dummy(1:UBOUND(me%vectors,DIM=1)+1,1:dim),source=0.0_r8k)
                IF (i > 0) dummy(1:UBOUND(me%vectors,DIM=1),1:dim) = me%vectors
                CALL MOVE_ALLOC(dummy, me%vectors)
                i = i + 1

                CALL interpret_string (line=line, datatype=(/ 'R','R','R' /), separator=' ', reals=reals)
                me%vectors(i,1:dim) = reals(1:dim)
            END IF
        END DO get_vectors

100     FORMAT((a))
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
101     FORMAT(*(es13.6,' '))
        END SUBROUTINE vector_write

        SUBROUTINE vector_setup (me, dataname, datatype, numcomp, tablename, values1d, values2d, values3d, field_arrays)
        !>@brief
        !> Subroutine performs the set-up for a vector attribute
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 12/14/2017
        CLASS(vector),    INTENT(OUT) :: me
        CHARACTER(LEN=*), INTENT(IN)  :: dataname
        INTEGER(i4k),     INTENT(IN), OPTIONAL :: numcomp
        CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: datatype, tablename
!        REAL(r8k), DIMENSION(..),   INTENT(IN), OPTIONAL :: values
        REAL(r8k), DIMENSION(:),     INTENT(IN), OPTIONAL :: values1d
        REAL(r8k), DIMENSION(:,:),   INTENT(IN), OPTIONAL :: values2d
        REAL(r8k), DIMENSION(:,:,:), INTENT(IN), OPTIONAL :: values3d
        TYPE(field_data_array), DIMENSION(:), INTENT(IN), OPTIONAL :: field_arrays

        me%dataname = dataname
        IF (PRESENT(datatype)) THEN
            me%datatype = datatype
        ELSE
            me%datatype = 'double'
        END IF
        IF (PRESENT(numcomp)  .AND. PRESENT(tablename) .AND. &
          & PRESENT(values1d) .AND. PRESENT(values3d)  .AND. PRESENT(field_arrays)) THEN
            !! DO NOTHING. ONLY ELIMINATES COMPILER WARNINGS
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
        LOGICAL      :: diffs

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
        CLASS(normal), INTENT(OUT) :: me
        INTEGER(i4k),  INTENT(IN)  :: unit
        INTEGER(i4k)               :: i, iostat
        INTEGER(i4k),  PARAMETER   :: dim = 3
        LOGICAL                    :: end_of_file
        CHARACTER(LEN=def_len)     :: line
        REAL(r8k),        DIMENSION(:),   ALLOCATABLE :: reals
        CHARACTER(LEN=:), DIMENSION(:),   ALLOCATABLE :: chars
        REAL(r8k),        DIMENSION(:,:), ALLOCATABLE :: dummy

        READ(unit,100) line
        CALL interpret_string (line=line, datatype=(/ 'C','C' /), ignore='NORMALS ', separator=' ', chars=chars)
        me%dataname = TRIM(chars(1)); me%datatype = TRIM(chars(2))

        ALLOCATE(me%normals(0,0)); end_of_file = .FALSE.; i = 0

        get_normals: DO
            READ(unit,100,iostat=iostat) line
            end_of_file = (iostat < 0)
            IF (end_of_file) THEN
                EXIT get_normals
            ELSE IF (TRIM(line) == '') THEN
                CYCLE     !! Skip blank lines
            ELSE
                ALLOCATE(dummy(1:UBOUND(me%normals,DIM=1)+1,1:dim),source=0.0_r8k)
                IF (i > 0) dummy(1:UBOUND(me%normals,DIM=1),1:dim) = me%normals
                CALL MOVE_ALLOC(dummy, me%normals)
                i = i + 1

                CALL interpret_string (line=line, datatype=(/ 'R','R','R' /), separator=' ', reals=reals)
                me%normals(i,1:dim) = reals(1:dim)
            END IF
        END DO get_normals

100     FORMAT((a))
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
101     FORMAT(*(es13.6,' '))
        END SUBROUTINE normal_write

        SUBROUTINE normal_setup (me, dataname, datatype, numcomp, tablename, values1d, values2d, values3d, field_arrays)
        !>@brief
        !> Subroutine performs the set-up for a normal attribute
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 12/14/2017
        CLASS(normal),    INTENT(OUT) :: me
        CHARACTER(LEN=*), INTENT(IN)  :: dataname
        INTEGER(i4k),     INTENT(IN), OPTIONAL :: numcomp
        CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: datatype, tablename
!        REAL(r8k), DIMENSION(..),   INTENT(IN), OPTIONAL :: values
        REAL(r8k), DIMENSION(:),     INTENT(IN), OPTIONAL :: values1d
        REAL(r8k), DIMENSION(:,:),   INTENT(IN), OPTIONAL :: values2d
        REAL(r8k), DIMENSION(:,:,:), INTENT(IN), OPTIONAL :: values3d
        TYPE(field_data_array), DIMENSION(:), INTENT(IN), OPTIONAL :: field_arrays

        me%dataname = dataname
        IF (PRESENT(datatype)) THEN
            me%datatype = datatype
        ELSE
            me%datatype = 'double'
        END IF
        IF (PRESENT(numcomp)  .AND. PRESENT(tablename) .AND. &
          & PRESENT(values1d) .AND. PRESENT(values3d)  .AND. PRESENT(field_arrays)) THEN
            !! DO NOTHING. ONLY ELIMINATES COMPILER WARNINGS
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
        LOGICAL      :: diffs

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
! Textures
!********
        SUBROUTINE texture_read (me, unit)
        USE Misc, ONLY : interpret_string
        !>@brief
        !> Subroutine performs the read for a texture attribute
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 12/14/2017
        CLASS(texture), INTENT(OUT) :: me
        INTEGER(i4k),   INTENT(IN)  :: unit
        INTEGER(i4k)                :: i, iostat, dim
        LOGICAL                     :: end_of_file
        CHARACTER(LEN=def_len)      :: line
        INTEGER(i4k),     DIMENSION(:),   ALLOCATABLE :: ints
        REAL(r8k),        DIMENSION(:),   ALLOCATABLE :: reals
        CHARACTER(LEN=:), DIMENSION(:),   ALLOCATABLE :: chars
        REAL(r8k),        DIMENSION(:,:), ALLOCATABLE :: dummy
        CHARACTER(LEN=1), DIMENSION(3),   PARAMETER   :: datatype = (/ 'R','R','R' /)

        READ(unit,100) line
        CALL interpret_string (line=line, datatype=(/ 'C','I','C' /), ignore='TEXTURE_COORDINATES ', separator=' ', &
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
        END SUBROUTINE texture_read

        SUBROUTINE texture_write (me, unit)
        !>@brief
        !> Subroutine performs the write for a texture attribute
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 12/13/2017
        CLASS(texture), INTENT(IN) :: me
        INTEGER(i4k),   INTENT(IN) :: unit
        INTEGER(i4k) :: i

        WRITE(unit,100) me%dataname, SIZE(me%textures,DIM=2), me%datatype
        DO i = 1, SIZE(me%textures,DIM=1)
            WRITE(unit,101) me%textures(i,:)
        END DO

100     FORMAT('TEXTURE_COORDINATES ',(a),' ',(i1),' ',(a))
101     FORMAT(*(es13.6,' '))
        END SUBROUTINE texture_write

        SUBROUTINE texture_setup (me, dataname, datatype, numcomp, tablename, values1d, values2d, values3d, field_arrays)
        !>@brief
        !> Subroutine performs the set-up for a texture attribute
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 12/14/2017
        CLASS(texture),   INTENT(OUT) :: me
        CHARACTER(LEN=*), INTENT(IN)  :: dataname
        INTEGER(i4k),     INTENT(IN), OPTIONAL :: numcomp
        CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: datatype, tablename
!        REAL(r8k), DIMENSION(..),   INTENT(IN), OPTIONAL :: values
        REAL(r8k), DIMENSION(:),     INTENT(IN), OPTIONAL :: values1d
        REAL(r8k), DIMENSION(:,:),   INTENT(IN), OPTIONAL :: values2d
        REAL(r8k), DIMENSION(:,:,:), INTENT(IN), OPTIONAL :: values3d
        TYPE(field_data_array), DIMENSION(:), INTENT(IN), OPTIONAL :: field_arrays

        me%dataname = dataname
        IF (PRESENT(datatype)) THEN
            me%datatype = datatype
        ELSE
            me%datatype = 'double'
        END IF
        IF (PRESENT(numcomp)  .AND. PRESENT(tablename) .AND. &
          & PRESENT(values1d) .AND. PRESENT(values3d)  .AND. PRESENT(field_arrays)) THEN
            !! DO NOTHING. ONLY ELIMINATES COMPILER WARNINGS
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
        CLASS(texture),   INTENT(IN) :: me
        CLASS(attribute), INTENT(IN) :: you
        INTEGER(i4k) :: i, j
        LOGICAL      :: diffs

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
!********
! Tensors
!********
        SUBROUTINE tensor_read (me, unit)
        USE Misc, ONLY : interpret_string
        !>@brief
        !> Subroutine performs the read for a tensor attribute
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 12/14/2017
        CLASS(tensor), INTENT(OUT) :: me
        INTEGER(i4k),  INTENT(IN)  :: unit
        INTEGER(i4k)               :: i, j, iostat
        LOGICAL                    :: end_of_file
        CHARACTER(LEN=def_len)     :: line
        REAL(r8k),          DIMENSION(:), ALLOCATABLE :: reals
        CHARACTER(LEN=:),   DIMENSION(:), ALLOCATABLE :: chars
        TYPE(tensor_array), DIMENSION(:), ALLOCATABLE :: dummy

        READ(unit,100) line
        CALL interpret_string (line=line, datatype=(/ 'C','C' /), ignore='TENSORS ', separator=' ', &
          &                    chars=chars)
        me%dataname = TRIM(chars(1)); me%datatype = TRIM(chars(2))

        ALLOCATE(me%tensors(0)); end_of_file = .FALSE.; i = 0

        get_tensors: DO
            READ(unit,100,iostat=iostat) line
            end_of_file = (iostat < 0)
            IF (end_of_file) THEN
                EXIT get_tensors
            ELSE IF (TRIM(line) == '') THEN
                CYCLE      !! Skip blank lines
            ELSE
                ALLOCATE(dummy(1:UBOUND(me%tensors,DIM=1)+1))
                dummy(1:UBOUND(me%tensors,DIM=1)) = me%tensors
                CALL MOVE_ALLOC(dummy, me%tensors)
                i = i + 1

                DO j = 1, UBOUND(me%tensors(i)%val,DIM=1)
                    IF (j > 1) READ(unit,100,iostat=iostat) line
                    CALL interpret_string (line=line, datatype=(/ 'R','R','R' /), separator=' ', reals=reals)
                    me%tensors(i)%val(j,1:3) = reals(1:3)
                END DO

            END IF
        END DO get_tensors

100     FORMAT((a))
        END SUBROUTINE tensor_read

        SUBROUTINE tensor_write (me, unit)
        !>@brief
        !> Subroutine performs the write for a tensor attribute
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 12/13/2017
        CLASS(tensor), INTENT(IN) :: me
        INTEGER(i4k),  INTENT(IN) :: unit
        INTEGER(i4k) :: i, j

        WRITE(unit,100) me%dataname, me%datatype
        DO i = 1, SIZE(me%tensors,DIM=1)
            DO j = 1, SIZE(me%tensors(i)%val,DIM=1)
                WRITE(unit,101) me%tensors(i)%val(j,:)
            END DO
            WRITE(unit,102)
        END DO

100     FORMAT('TENSORS ',(a),' ',(a))
101     FORMAT(*(es13.6,' '))
102     FORMAT()
        END SUBROUTINE tensor_write

        SUBROUTINE tensor_setup (me, dataname, datatype, numcomp, tablename, values1d, values2d, values3d, field_arrays)
        !>@brief
        !> Subroutine performs the set-up for a tensor attribute
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 12/14/2017
        CLASS(tensor),    INTENT(OUT) :: me
        CHARACTER(LEN=*), INTENT(IN)  :: dataname
        INTEGER(i4k),     INTENT(IN), OPTIONAL :: numcomp
        CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: datatype, tablename
        INTEGER(i4k) :: i
!        REAL(r8k), DIMENSION(..),   INTENT(IN), OPTIONAL :: values
        REAL(r8k), DIMENSION(:),     INTENT(IN), OPTIONAL :: values1d
        REAL(r8k), DIMENSION(:,:),   INTENT(IN), OPTIONAL :: values2d
        REAL(r8k), DIMENSION(:,:,:), INTENT(IN), OPTIONAL :: values3d
        TYPE(field_data_array), DIMENSION(:), INTENT(IN), OPTIONAL :: field_arrays

        me%dataname = dataname
        IF (PRESENT(datatype)) THEN
            me%datatype = datatype
        ELSE
            me%datatype = 'double'
        END IF
        IF (PRESENT(numcomp)  .AND. PRESENT(tablename) .AND. &
          & PRESENT(values1d) .AND. PRESENT(values2d)  .AND. PRESENT(field_arrays)) THEN
            !! DO NOTHING. ONLY ELIMINATES COMPILER WARNINGS
        END IF
        IF (.NOT. PRESENT(values3d)) THEN
            ERROR STOP 'Must provide tensors in tensor_setup'
        ELSE IF (SIZE(values3d,DIM=2) /= 3 .OR. SIZE(values3d,DIM=3) /= 3) THEN
            ERROR STOP 'Tensors can only be 3x3'
        ELSE
            IF (ALLOCATED(me%tensors)) DEALLOCATE(me%tensors)
            ALLOCATE(me%tensors(1:UBOUND(values3d,DIM=1)))
            DO i = 1, UBOUND(values3d,DIM=1)
                me%tensors(i)%val(1:3,1:3) = values3d(i,1:3,1:3)
            END DO
        END IF

        END SUBROUTINE tensor_setup

        FUNCTION check_for_diffs_tensor (me, you) RESULT (diffs)
        !>@brief
        !> Function checks for differences in a tensor attribute
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 12/14/2017
        CLASS(tensor),    INTENT(IN) :: me
        CLASS(attribute), INTENT(IN) :: you
        INTEGER(i4k) :: i, j, k
        LOGICAL      :: diffs

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
                ELSE
                    DO i = 1, UBOUND(me%tensors,DIM=1)
                        DO j = 1, UBOUND(me%tensors(i)%val,DIM=1)
                            DO k = 1, UBOUND(me%tensors(i)%val,DIM=2)
                                IF (me%tensors(i)%val(j,k) /= you%tensors(i)%val(j,k)) THEN
                                    diffs = .TRUE.
                                END IF
                            END DO
                        END DO
                    END DO
                END IF
            END SELECT
        END IF

        END FUNCTION check_for_diffs_tensor
!********
! Fields
!********
        SUBROUTINE field_read (me, unit)
        USE Misc, ONLY : interpret_string
        !>@brief
        !> Subroutine performs the read for a field attribute
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 12/14/2017
        CLASS(field), INTENT(OUT) :: me
        INTEGER(i4k), INTENT(IN)  :: unit
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
        CALL interpret_string (line=line, datatype=(/ 'C','I' /), ignore='FIELD ', separator=' ', &
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

                CALL interpret_string (line=line, datatype=(/ 'C','I','I','C' /), separator=' ', chars=chars, ints=ints)
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
        END SUBROUTINE field_read

        SUBROUTINE field_write (me, unit)
        !>@brief
        !> Subroutine performs the write for a field attribute
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 12/13/2017
        CLASS(field), INTENT(IN) :: me
        INTEGER(i4k), INTENT(IN) :: unit
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
        END SUBROUTINE field_write

        SUBROUTINE field_setup (me, dataname, datatype, numcomp, tablename, values1d, values2d, values3d, field_arrays)
        !>@brief
        !> Subroutine performs the set-up for a field attribute
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 12/14/2017
        CLASS(field),     INTENT(OUT) :: me
        CHARACTER(LEN=*), INTENT(IN)  :: dataname
        INTEGER(i4k),     INTENT(IN), OPTIONAL :: numcomp
        CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: datatype, tablename
!        REAL(r8k), DIMENSION(..),   INTENT(IN), OPTIONAL :: values
        REAL(r8k), DIMENSION(:),     INTENT(IN), OPTIONAL :: values1d
        REAL(r8k), DIMENSION(:,:),   INTENT(IN), OPTIONAL :: values2d
        REAL(r8k), DIMENSION(:,:,:), INTENT(IN), OPTIONAL :: values3d
        TYPE(field_data_array), DIMENSION(:), INTENT(IN), OPTIONAL :: field_arrays

        me%dataname = dataname
        IF (PRESENT(datatype)) THEN
            me%datatype = datatype
        ELSE
            me%datatype = 'double'
        END IF
        IF (PRESENT(numcomp)  .AND. PRESENT(tablename) .AND. &
          & PRESENT(values1d) .AND. PRESENT(values2d)  .AND. PRESENT(values3d)) THEN
            !! DO NOTHING. ONLY ELIMINATES COMPILER WARNINGS
        END IF
        IF (.NOT. PRESENT(field_arrays)) THEN
            ERROR STOP 'Must provide field_arrays in field_setup'
        ELSE
            me%array = field_arrays
        END IF

        END SUBROUTINE field_setup

        FUNCTION check_for_diffs_field (me, you) RESULT (diffs)
        !>@brief
        !> Function checks for differences in a field attribute
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 12/14/2017
        CLASS(field),     INTENT(IN) :: me
        CLASS(attribute), INTENT(IN) :: you
        INTEGER(i4k) :: i, j, k
        LOGICAL      :: diffs

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

        END FUNCTION check_for_diffs_field
END MODULE vtk_attributes
