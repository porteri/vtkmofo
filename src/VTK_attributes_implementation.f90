SUBMODULE (vtk_attributes) vtk_attributes_implementation
    USE Precision
    USE Misc, ONLY : def_len
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
    !! Possible data types:
    !! bit, unsigned_char, char, unsigned_short, short, unsigned_int, int,
    !! unsigned_long, long, float, or double.
    CHARACTER(LEN=*), PARAMETER :: default = 'default'     !! Default table name

    CONTAINS

        MODULE PROCEDURE abs_read
        !>@brief
        !> Abstract for reading an attribute
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 12/13/2017

        SELECT TYPE (me)
        CLASS IS (attribute)
            READ(unit,*) me%dataname !! Workaround for ifort 2018 linux compiler error (not error for 2018 on Windows)
                                     !! that a class with intent(out) was not provided a value
        END SELECT
        END PROCEDURE abs_read

        MODULE PROCEDURE abs_write
        !>@brief
        !> Abstract for writing an attribute
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 12/13/2017
        SELECT TYPE (me)
        CLASS IS (attribute)
            WRITE(unit,*) me%dataname
        END SELECT
        END PROCEDURE abs_write

        MODULE PROCEDURE initialize
        !>@brief
        !> Abstract for performing the set-up of an attribute
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 12/13/2017

        SELECT TYPE (me)
        CLASS IS (scalar)
            CALL me%setup(dataname, datatype, numcomp, tablename, ints1d, values1d)
        CLASS IS (vector)
            CALL me%setup(dataname, datatype, numcomp, tablename, values1d, values2d, values3d, field_arrays)
        CLASS IS (normal)
            CALL me%setup(dataname, datatype, numcomp, tablename, values1d, values2d, values3d, field_arrays)
        CLASS IS (texture)
            CALL me%setup(dataname, datatype, numcomp, tablename, values1d, values2d, values3d, field_arrays)
        CLASS IS (tensor)
            CALL me%setup(dataname, datatype, numcomp, tablename, values1d, values2d, values3d, field_arrays)
        CLASS IS (field)
            CALL me%setup(dataname, datatype, numcomp, tablename, values1d, values2d, values3d, field_arrays)
        CLASS DEFAULT
            ERROR STOP 'Generic class not defined for vtkmofo class attribute'
        END SELECT

        END PROCEDURE initialize

        MODULE PROCEDURE check_for_diffs
        !>@brief
        !> Function checks for differences in an attribute
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 12/13/2017

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
        !>@brief
        !> Subroutine performs the read for a scalar attribute
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 12/13/2017
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
        DEALLOCATE(ints)

        READ(unit,100) line
        CALL interpret_string (line=line, datatype=(/ 'C' /), ignore='LOOKUP_TABLE ', separator=' ', chars=chars)
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

                    CALL interpret_string (line=line, datatype=(/ 'I' /), separator=' ', ints=ints)
                    me%ints(i) = ints(1)
                    DEALLOCATE(ints)
                CASE ('float', 'double')
                    ALLOCATE(dummy(1:UBOUND(me%reals,DIM=1)+1),source=0.0_r8k)
                    IF (i > 0) dummy(1:UBOUND(me%reals,DIM=1)) = me%reals
                    CALL MOVE_ALLOC(dummy, me%reals)
                    i = i + 1

                    CALL interpret_string (line=line, datatype=(/ 'R' /), separator=' ', reals=reals)
                    me%reals(i) = reals(1)
                CASE DEFAULT
                    ERROR STOP 'datatype not supported in scalar_read'
                END SELECT
            END IF
        END DO get_scalars

100     FORMAT((a))
        END PROCEDURE scalar_read

        MODULE PROCEDURE scalar_write
        !>@brief
        !> Subroutine performs the write for a scalar attribute
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 12/13/2017
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
        !>@brief
        !> Subroutine performs the set-up for a scalar attribute
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 12/13/2017

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
        ELSE IF (.NOT. PRESENT(values1d)) THEN
            ERROR STOP 'Must provide scalars in scalar_setup'
        ELSE
            !! TODO: Implement this once SELECT RANK is incorporated into compilers (Fortran 2015)
!            SELECT RANK (values)
!            RANK(1)
!                me%scalars = values
!            RANK DEFAULT
!                ERROR STOP 'Bad rank for values. Must be RANK=2. Execution terminated in Subroutine: scalar_setup'
!            END SELECT
            me%reals = values1d
        END IF

        END PROCEDURE scalar_setup

        MODULE PROCEDURE check_for_diffs_scalar
        !>@brief
        !> Function checks for differences in a scalar attribute
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 12/13/2017
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
        USE Misc, ONLY : interpret_string
        !>@brief
        !> Subroutine performs the read for a vector attribute
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 12/14/2017
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
        END PROCEDURE vector_read

        MODULE PROCEDURE vector_write
        !>@brief
        !> Subroutine performs the write for a vector attribute
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 12/13/2017
        INTEGER(i4k) :: i

        WRITE(unit,100) me%dataname, me%datatype
        DO i = 1, SIZE(me%vectors,DIM=1)
            WRITE(unit,101) me%vectors(i,1:3)
        END DO

100     FORMAT('VECTORS ',(a),' ',(a))
101     FORMAT(*(es13.6,' '))
        END PROCEDURE vector_write

        MODULE PROCEDURE vector_setup
        !>@brief
        !> Subroutine performs the set-up for a vector attribute
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 12/14/2017

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

        END PROCEDURE vector_setup

        MODULE PROCEDURE check_for_diffs_vector
        !>@brief
        !> Function checks for differences in a vector attribute
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 12/14/2017
        INTEGER(i4k) :: i, j

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

        END PROCEDURE check_for_diffs_vector
!********
! Normals
!********
        MODULE PROCEDURE normal_read
        USE Misc, ONLY : interpret_string
        !>@brief
        !> Subroutine performs the read for a normal attribute
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 12/14/2017
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
        END PROCEDURE normal_read

        MODULE PROCEDURE normal_write
        !>@brief
        !> Subroutine performs the write for a normal attribute
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 12/13/2017
        INTEGER(i4k) :: i

        WRITE(unit,100) me%dataname, me%datatype
        DO i = 1, SIZE(me%normals,DIM=1)
            WRITE(unit,101) me%normals(i,1:3)
        END DO

100     FORMAT('NORMALS ',(a),' ',(a))
101     FORMAT(*(es13.6,' '))
        END PROCEDURE normal_write

        MODULE PROCEDURE normal_setup
        !>@brief
        !> Subroutine performs the set-up for a normal attribute
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 12/14/2017

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

        END PROCEDURE normal_setup

        MODULE PROCEDURE check_for_diffs_normal
        !>@brief
        !> Function checks for differences in a normal attribute
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 12/14/2017
        INTEGER(i4k) :: i, j

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

        END PROCEDURE check_for_diffs_normal
!********
! Textures
!********
        MODULE PROCEDURE texture_read
        USE Misc, ONLY : interpret_string
        !>@brief
        !> Subroutine performs the read for a texture attribute
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 12/14/2017
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
        END PROCEDURE texture_read

        MODULE PROCEDURE texture_write
        !>@brief
        !> Subroutine performs the write for a texture attribute
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 12/13/2017
        INTEGER(i4k) :: i

        WRITE(unit,100) me%dataname, SIZE(me%textures,DIM=2), me%datatype
        DO i = 1, SIZE(me%textures,DIM=1)
            WRITE(unit,101) me%textures(i,:)
        END DO

100     FORMAT('TEXTURE_COORDINATES ',(a),' ',(i1),' ',(a))
101     FORMAT(*(es13.6,' '))
        END PROCEDURE texture_write

        MODULE PROCEDURE texture_setup
        !>@brief
        !> Subroutine performs the set-up for a texture attribute
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 12/14/2017

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

        END PROCEDURE texture_setup

        MODULE PROCEDURE check_for_diffs_texture
        !>@brief
        !> Function checks for differences in a texture attribute
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 12/14/2017
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
        USE Misc, ONLY : interpret_string
        !>@brief
        !> Subroutine performs the read for a tensor attribute
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 12/14/2017
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
        END PROCEDURE tensor_read

        MODULE PROCEDURE tensor_write
        !>@brief
        !> Subroutine performs the write for a tensor attribute
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 12/13/2017
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
        END PROCEDURE tensor_write

        MODULE PROCEDURE tensor_setup
        !>@brief
        !> Subroutine performs the set-up for a tensor attribute
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 12/14/2017
        INTEGER(i4k) :: i

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
            ALLOCATE(me%tensors(1:UBOUND(values3d,DIM=1)))
            DO i = 1, UBOUND(values3d,DIM=1)
                me%tensors(i)%val(1:3,1:3) = values3d(i,1:3,1:3)
            END DO
        END IF

        END PROCEDURE tensor_setup

        MODULE PROCEDURE check_for_diffs_tensor
        !>@brief
        !> Function checks for differences in a tensor attribute
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 12/14/2017
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

        END PROCEDURE check_for_diffs_tensor
!********
! Fields
!********
        MODULE PROCEDURE field_read
        USE Misc, ONLY : interpret_string
        !>@brief
        !> Subroutine performs the read for a field attribute
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 12/14/2017
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
        END PROCEDURE field_read

        MODULE PROCEDURE field_write
        !>@brief
        !> Subroutine performs the write for a field attribute
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 12/13/2017
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
        !>@brief
        !> Subroutine performs the set-up for a field attribute
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 12/14/2017

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

        END PROCEDURE field_setup

        MODULE PROCEDURE check_for_diffs_field
        !>@brief
        !> Function checks for differences in a field attribute
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 12/14/2017
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
