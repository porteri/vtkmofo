MODULE vtk_cells
    USE Kinds
    IMPLICIT NONE
    !>@brief
    !> THis module contains the types of cells used by VTK
    !>@author
    !> Ian Porter
    !>@date
    !> 12/2/2017

    PRIVATE
    PUBLIC :: vtkcell, vertex

    TYPE, ABSTRACT :: vtkcell
        INTEGER(i4k) :: n_points
        INTEGER(i4k) :: type
        INTEGER(i4k), DIMENSION(:), ALLOCATABLE :: points
    CONTAINS
        PROCEDURE(abs_init),  DEFERRED, PUBLIC :: init
        PROCEDURE, PUBLIC :: setup => abs_setup
        PROCEDURE :: define => abs_define
        PROCEDURE, PUBLIC :: write  => abs_write
    END TYPE vtkcell

    TYPE, EXTENDS(vtkcell) :: vertex
    CONTAINS
        PROCEDURE :: init   => vertex_init
    END TYPE vertex

    TYPE, EXTENDS(vtkcell) :: voxel
    CONTAINS
        PROCEDURE :: init   => voxel_init
    END TYPE voxel

    CONTAINS
        SUBROUTINE abs_init (me)
        CLASS(vtkcell), INTENT(OUT) :: me
        me%type = 0
        END SUBROUTINE abs_init

        SUBROUTINE vertex_init (me)
        CLASS(vertex), INTENT(OUT) :: me
        me%n_points = 1
        me%type     = 1
        END SUBROUTINE vertex_init

        SUBROUTINE voxel_init (me)
        CLASS(voxel), INTENT(OUT) :: me
        me%n_points = 8
        me%type     = 11
        END SUBROUTINE voxel_init

        SUBROUTINE abs_setup (me, points)
        CLASS(vtkcell), INTENT(INOUT) :: me
        INTEGER(i4k), DIMENSION(:), INTENT(IN) :: points
        me%points = points
        END SUBROUTINE abs_setup

        SUBROUTINE abs_define (me, points)
        CLASS(vtkcell), INTENT(INOUT) :: me
        INTEGER(i4k), DIMENSION(:), INTENT(IN) :: points

        IF (ALLOCATED(me%points)) DEALLOCATE(me%points)
        me%points = points

        END SUBROUTINE abs_define

        SUBROUTINE abs_write (me, unit)
        CLASS(vtkcell), INTENT(IN) :: me
        INTEGER(i4k), INTENT(IN) :: unit
        INTEGER(i4k) :: i
        WRITE(unit,100) me%n_points, (me%points(i),i=1,me%n_points)
100     FORMAT ((BN,i8),*(BN,i8))
        END SUBROUTINE abs_write

END MODULE vtk_cells
