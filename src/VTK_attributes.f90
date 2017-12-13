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
    PUBLIC :: attribute
    
    CHARACTER(LEN=*), PARAMETER :: default = 'default'
    
    TYPE, ABSTRACT :: attribute
        CHARACTER(LEN=:), ALLOCATABLE :: dataname
    CONTAINS
        PROCEDURE(abs_read),  DEFERRED, PUBLIC :: read
        PROCEDURE(abs_write), DEFERRED, PUBLIC :: write
        PROCEDURE(abs_setup), DEFERRED, PUBLIC :: setup
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
    END TYPE scalar

    CONTAINS
        SUBROUTINE abs_read (me, unit)
        CLASS(attribute), INTENT(INOUT) :: me
        INTEGER(i4k),     INTENT(IN)    :: unit
        SELECT TYPE (me)
        END SELECT
        END SUBROUTINE abs_read

        SUBROUTINE scalar_read (me, unit)
        CLASS(scalar), INTENT(INOUT) :: me
        INTEGER(i4k),  INTENT(IN)    :: unit
        INTEGER(i4k) :: i

        READ(unit,100) me%dataname, me%datatype, me%numcomp
        READ(unit,101) me%tablename
        DO i = 1, SIZE(me%scalars)
            READ(unit,102) me%scalars(i)
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

        SUBROUTINE abs_setup (me)
        CLASS(attribute), INTENT(INOUT) :: me
        SELECT TYPE (me)
        END SELECT
        END SUBROUTINE abs_setup

        SUBROUTINE scalar_setup (me)
        CLASS(scalar), INTENT(INOUT) :: me
        SELECT TYPE (me)
        END SELECT
        END SUBROUTINE scalar_setup
    END MODULE vtk_attributes