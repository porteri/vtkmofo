MODULE vtk_datasets
    USE Kinds
    USE vtk_cells, ONLY : vtkcell
    USE Misc
    IMPLICIT NONE
    !>@brief
    !> This module contains the dataset formats for vtk format
    !>@author
    !> Ian Porter
    !>@date
    !> 12/1/2017
    !
    ! The following dataset formats are available:
    ! 1) Structured points
    ! 2) Structured grid
    ! 3) Rectilinear grid
    ! 4) Unstructured grid
    ! 5) Polygonal data
    ! 

    PRIVATE
    PUBLIC :: ascii, binary, filetype, vtkunit, version, default_title, default_fn, vtkfilename, vtktitle
    PUBLIC :: dataset, struct_pts, struct_grid, unstruct_grid

    INTEGER(i4k), PARAMETER :: ascii=0_i4k, binary=1_i4k                            !! Available file types
    INTEGER(i4k) :: filetype = ascii                                                !! Selected file type
    INTEGER(i4k) :: vtkunit  = 20_i4k                                               !! Default VTK unit #
    INTEGER(i4k), PARAMETER :: bit=0_i4k, unsigned_char=1_i4k, char=2_i4k, unsigned_short=3_i4k, short=4_i4k, &
      &                        unsigned_int=5_i4k, int=6_i4k, unsigned_long=7_i4k, long=8_i4k, float=9_i4k,   &
      &                        double=10_i4k                                        !! Types of data
    CHARACTER(LEN=*), PARAMETER :: version       = '# vtk DataFile Version 3.0'     !! VTK datafile version
    CHARACTER(LEN=*), PARAMETER :: default_title = 'Version 3.0 VTK file'           !! Title card
    CHARACTER(LEN=*), PARAMETER :: default_fn    = 'out.vtk'                        !! Default filename
    CHARACTER(LEN=:), ALLOCATABLE :: vtkfilename                                    !! Supplied filename
    CHARACTER(LEN=:), ALLOCATABLE :: vtktitle                                       !! Supplied title

    TYPE :: coordinates
        CHARACTER(LEN=:), ALLOCATABLE :: datatype
        REAL(r8k),        DIMENSION(:), ALLOCATABLE :: coord
    END TYPE coordinates

    TYPE, ABSTRACT :: dataset
        PRIVATE
        INTEGER(i4k), PUBLIC :: unit
        CHARACTER(LEN=:), ALLOCATABLE :: name
        INTEGER(i4k), DIMENSION(3) :: dimensions
        LOGICAL, PUBLIC :: firstcall = .TRUE.
    CONTAINS
        PROCEDURE(abs_read),  DEFERRED, PUBLIC :: read
        PROCEDURE(abs_write), DEFERRED, PUBLIC :: write
        PROCEDURE(abs_setup), DEFERRED, PUBLIC :: setup
    END TYPE dataset

    TYPE, EXTENDS(dataset) :: struct_pts
        !! Structured points
        REAL(r8k), DIMENSION(3) :: origin
        REAL(r8k), DIMENSION(3) :: spacing
    CONTAINS
        PROCEDURE :: read  => struct_pts_read
        PROCEDURE :: write => struct_pts_write
        PROCEDURE :: setup => struct_pts_setup
    END TYPE struct_pts

    TYPE, EXTENDS(dataset) :: struct_grid
        !! Structured grid
        INTEGER(i4k) :: n_points
        CHARACTER(LEN=:), ALLOCATABLE :: datatype
        REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: points
    CONTAINS
        PROCEDURE :: read  => struct_grid_read
        PROCEDURE :: write => struct_grid_write
        PROCEDURE :: setup => struct_grid_setup
    END TYPE struct_grid

    TYPE, EXTENDS(dataset) :: rectlnr_grid
        !! Rectilinear grid
        TYPE (coordinates) :: x
        TYPE (coordinates) :: y
        TYPE (coordinates) :: z
    CONTAINS
        PROCEDURE :: read  => rectlnr_grid_read
        PROCEDURE :: write => rectlnr_grid_write
        PROCEDURE :: setup => rectlnr_grid_setup
    END TYPE rectlnr_grid

    TYPE, EXTENDS(dataset) :: polygonal_data
        !! Polygonal data
        INTEGER(i4k) :: n_points
        CHARACTER(LEN=:), ALLOCATABLE :: datatype
        REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: points
        CLASS(vtkcell), DIMENSION(:), ALLOCATABLE :: vertices
        CLASS(vtkcell), DIMENSION(:), ALLOCATABLE :: lines
        CLASS(vtkcell), DIMENSION(:), ALLOCATABLE :: polygons
        CLASS(vtkcell), DIMENSION(:), ALLOCATABLE :: triangles
    CONTAINS
        PROCEDURE :: read  => polygonal_data_read
        PROCEDURE :: write => polygonal_data_write
        PROCEDURE :: setup => polygonal_data_setup
    END TYPE polygonal_data

    TYPE, EXTENDS(dataset) :: unstruct_grid
        !! Unstructured grid
        INTEGER(i4k) :: n_points
        INTEGER(i4k) :: n_cells
        INTEGER(i4k) :: n_cell_types
        INTEGER(i4k) :: size
        CHARACTER(LEN=:), ALLOCATABLE :: datatype
        REAL(r8k),    DIMENSION(:,:), ALLOCATABLE :: points
        CLASS(vtkcell), DIMENSION(:), ALLOCATABLE :: cell
    CONTAINS
        PROCEDURE :: read  => unstruct_grid_read
        PROCEDURE :: write => unstruct_grid_write
        PROCEDURE :: setup => unstruct_grid_setup
    END TYPE unstruct_grid

    TYPE, ABSTRACT :: vtk_dataattrib
        
    END TYPE vtk_dataattrib

    TYPE, EXTENDS(vtk_dataattrib) :: scalar
        CHARACTER, ALLOCATABLE :: dataname
        CHARACTER, ALLOCATABLE :: datatype
        CHARACTER, ALLOCATABLE :: tablename
        INTEGER(i4k) :: numcomp
    END TYPE scalar

    TYPE, EXTENDS(vtk_dataattrib) :: vector
        CHARACTER, ALLOCATABLE :: dataname
        CHARACTER, ALLOCATABLE :: datatype
        REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: dat
    END TYPE vector

    TYPE, EXTENDS(vtk_dataattrib) :: norms
        CHARACTER, ALLOCATABLE :: dataname
        CHARACTER, ALLOCATABLE :: datatype
        REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: dat
    END TYPE norms

    CONTAINS
! ****************
! Read subroutines
! ****************
        SUBROUTINE abs_read (me)
        CLASS(dataset), INTENT(INOUT) :: me
        SELECT TYPE (me)
        END SELECT
        END SUBROUTINE abs_read

        SUBROUTINE struct_pts_read (me)
        CLASS(struct_pts), INTENT(INOUT) :: me

        READ(me%unit,100) me%name
        READ(me%unit,101) me%dimensions
        READ(me%unit,102) me%origin
        READ(me%unit,103) me%spacing
100     FORMAT ((a))
101     FORMAT (*(i0,' '))
102     FORMAT (*(es12.5))
103     FORMAT (*(es12.5))

        END SUBROUTINE struct_pts_read

        SUBROUTINE struct_grid_read (me)
        CLASS(struct_grid), INTENT(INOUT) :: me
        INTEGER(i4k) :: i

        READ(me%unit,100) me%name
        READ(me%unit,101) me%dimensions
        READ(me%unit,102) me%n_points, me%datatype
        DO i = 1, me%n_points
            READ(me%unit,103) me%points(1:3,i)
        END DO

100     FORMAT ((a))
101     FORMAT (*(i0,' '))
102     FORMAT ((i0),' ',(a))
103     FORMAT (*(es12.5))

        END SUBROUTINE struct_grid_read

        SUBROUTINE rectlnr_grid_read (me)
        CLASS(rectlnr_grid), INTENT(INOUT) :: me

        READ(me%unit,100) me%name
        READ(me%unit,101) me%dimensions
        READ(me%unit,102) me%x%datatype
        READ(me%unit,110) me%x%coord
        READ(me%unit,103) me%y%datatype
        READ(me%unit,110) me%y%coord
        READ(me%unit,104) me%z%datatype
        READ(me%unit,110) me%z%coord

100     FORMAT ('DATASET ',(a))
101     FORMAT ('DIMENSIONS ',*(i0))
102     FORMAT ('X_COORDINATES ',(a))
103     FORMAT ('Y_COORDINATES ',(a))
104     FORMAT ('Z_COORDINATES ',(a))
110     FORMAT (*(es12.5))

        END SUBROUTINE rectlnr_grid_read

        SUBROUTINE polygonal_data_read (me)

        CLASS(polygonal_data), INTENT(INOUT) :: me
        INTEGER(i4k) :: i

        READ(me%unit,100) me%name
        READ(me%unit,101) me%n_points, me%datatype

        DO i = 1, me%n_points
            READ(me%unit,102) me%points(1:3,i)
        END DO

        !! The following options are not required inputs
!        IF (ALLOCATED(me%vertices)) THEN
!            READ(me%unit,103) SIZE(me%vertices,DIM=1), SIZE(me%vertices,DIM=1)
!        END IF

!        IF (ALLOCATED(me%lines)) THEN
!            READ(me%unit,104) SIZE(me%lines,DIM=1), SIZE(me%lines,DIM=1)
!        END IF

!        IF (ALLOCATED(me%polygons)) THEN
!            READ(me%unit,105) SIZE(me%polygons,DIM=1), SIZE(me%polygons,DIM=1)
!        END IF

!        IF (ALLOCATED(me%triangles)) THEN
!            READ(me%unit,106) SIZE(me%triangles,DIM=1), SIZE(me%triangles,DIM=1)
!        END IF

100     FORMAT ('DATASET ',(a))
101     FORMAT ('POINTS ',(i0),' ',(a))
102     FORMAT (*(es12.5))
103     FORMAT ('VERTICES ',(i0),' ',(i0))
104     FORMAT ('LINES ',(i0),' ',(i0))
105     FORMAT ('POLYGONS ',(i0),' ',(i0))
106     FORMAT ('TRIANGLE_STRIPS ',(i0),' ',(i0))

        END SUBROUTINE polygonal_data_read

        SUBROUTINE unstruct_grid_read (me)
        CLASS(unstruct_grid), INTENT(INOUT) :: me
        INTEGER(i4k) :: i

        READ(me%unit,100) me%name
        READ(me%unit,101) me%n_points, me%datatype

        DO i = 1, me%n_points
            READ(me%unit,102) me%points(1:3,i)
        END DO

        READ(me%unit,103) me%n_cells, me%size
        DO i = 1, me%n_cells
            CALL me%cell(i)%READ(me%unit)
        END DO

        READ(me%unit,104) me%n_cell_types
        DO i = 1, me%n_cell_types
            CALL me%cell(i)%READ(me%unit)
        END DO

100     FORMAT ('DATASET ',(a))
101     FORMAT ('POINTS ',(i0),' ',(a))
102     FORMAT (*(es12.5))
103     FORMAT ('CELLS ',(i0),' ',(i0))
104     FORMAT ('CELL_TYPES ',(i0))

        END SUBROUTINE unstruct_grid_read
! *****************
! Write subroutines
! *****************
        SUBROUTINE abs_write (me)
        CLASS(dataset), INTENT(IN) :: me
        SELECT TYPE (me)
        END SELECT
        END SUBROUTINE abs_write

        SUBROUTINE struct_pts_write (me)
        CLASS(struct_pts), INTENT(IN) :: me

        WRITE(me%unit,100) me%name
        WRITE(me%unit,101) me%dimensions
        WRITE(me%unit,102) me%origin
        WRITE(me%unit,103) me%spacing
100     FORMAT ('DATASET ',(a))
101     FORMAT ('DIMENSIONS ',*(i0,' '))
102     FORMAT ('ORIGIN ',*(es12.5))
103     FORMAT ('SPACING ',*(es12.5))

        END SUBROUTINE struct_pts_write

        SUBROUTINE struct_grid_write (me)
        CLASS(struct_grid), INTENT(IN) :: me
        INTEGER(i4k) :: i

        WRITE(me%unit,100) me%name
        WRITE(me%unit,101) me%dimensions
        WRITE(me%unit,102) me%n_points, me%datatype
        DO i = 1, me%n_points
            WRITE(me%unit,103) me%points(1:3,i)
        END DO

100     FORMAT ('DATASET ',(a))
101     FORMAT ('DIMENSIONS ',*(i0,' '))
102     FORMAT ('POINTS ',(i0),' ',(a))
103     FORMAT (*(es12.5))

        END SUBROUTINE struct_grid_write

        SUBROUTINE rectlnr_grid_write (me)
        CLASS(rectlnr_grid), INTENT(IN) :: me

        WRITE(me%unit,100) me%name
        WRITE(me%unit,101) me%dimensions
        WRITE(me%unit,102) me%x%datatype
        WRITE(me%unit,110) me%x%coord
        WRITE(me%unit,103) me%y%datatype
        WRITE(me%unit,110) me%y%coord
        WRITE(me%unit,104) me%z%datatype
        WRITE(me%unit,110) me%z%coord

100     FORMAT ('DATASET ',(a))
101     FORMAT ('DIMENSIONS ',*(i0,' '))
102     FORMAT ('X_COORDINATES ',(a))
103     FORMAT ('Y_COORDINATES ',(a))
104     FORMAT ('Z_COORDINATES ',(a))
110     FORMAT (*(es12.5))

        END SUBROUTINE rectlnr_grid_write

        SUBROUTINE polygonal_data_write (me)

        CLASS(polygonal_data), INTENT(IN) :: me
        INTEGER(i4k) :: i

        WRITE(me%unit,100) me%name
        WRITE(me%unit,101) me%n_points, me%datatype

        DO i = 1, me%n_points
            WRITE(me%unit,102) me%points(1:3,i)
        END DO

        !! The following options are not required inputs
        IF (ALLOCATED(me%vertices)) THEN
            WRITE(me%unit,103) SIZE(me%vertices,DIM=1), SIZE(me%vertices,DIM=1)
            DO i = 1, UBOUND(me%vertices,DIM=1)
                CALL me%vertices(i)%write(me%unit)
            END DO
        END IF

        IF (ALLOCATED(me%lines)) THEN
            WRITE(me%unit,104) SIZE(me%lines,DIM=1), SIZE(me%lines,DIM=1)
            DO i = 1, UBOUND(me%lines,DIM=1)
                CALL me%lines(i)%write(me%unit)
            END DO
        END IF

        IF (ALLOCATED(me%polygons)) THEN
            WRITE(me%unit,105) SIZE(me%polygons,DIM=1), SIZE(me%polygons,DIM=1)
            DO i = 1, UBOUND(me%polygons,DIM=1)
                CALL me%polygons(i)%write(me%unit)
            END DO
        END IF

        IF (ALLOCATED(me%triangles)) THEN
            WRITE(me%unit,106) SIZE(me%triangles,DIM=1), SIZE(me%triangles,DIM=1)
            DO i = 1, UBOUND(me%triangles,DIM=1)
                CALL me%triangles(i)%write(me%unit)
            END DO
        END IF

100     FORMAT ('DATASET ',(a))
101     FORMAT ('POINTS ',(i0),' ',(a))
102     FORMAT (*(es12.5))
103     FORMAT ('VERTICES ',(i0),' ',(i0))
104     FORMAT ('LINES ',(i0),' ',(i0))
105     FORMAT ('POLYGONS ',(i0),' ',(i0))
106     FORMAT ('TRIANGLE_STRIPS ',(i0),' ',(i0))

        END SUBROUTINE polygonal_data_write

        SUBROUTINE unstruct_grid_write (me)
        CLASS(unstruct_grid), INTENT(IN) :: me
        INTEGER(i4k) :: i

        WRITE(me%unit,100) me%name
        WRITE(me%unit,101) me%n_points, me%datatype

        DO i = 1, me%n_points
            WRITE(me%unit,102) me%points(1:3,i)
        END DO

        WRITE(me%unit,103) me%n_cells, me%size
        DO i = 1, me%n_cells
            CALL me%cell(i)%write(me%unit)
        END DO

        WRITE(me%unit,104) me%n_cell_types
        DO i = 1, me%n_cell_types
            CALL me%cell(i)%write(me%unit)
        END DO

100     FORMAT ('DATASET ',(a))
101     FORMAT ('POINTS ',(i0),' ',(a))
102     FORMAT (*(es12.5))
103     FORMAT ('CELLS ',(i0),' ',(i0))
104     FORMAT ('CELL_TYPES ',(i0))

        END SUBROUTINE unstruct_grid_write
! *****************
! Setup subroutines
! *****************
        SUBROUTINE abs_setup (me, unit, dims, origin, spacing, points, cells, cell_type)
        CLASS (dataset), INTENT(INOUT) :: me
        INTEGER(i4k), INTENT(IN) :: unit
        INTEGER(i4k), DIMENSION(3),   INTENT(IN), OPTIONAL :: dims
        INTEGER(i4k), DIMENSION(:),   INTENT(IN), OPTIONAL :: cell_type
        INTEGER(i4k), DIMENSION(:,:), INTENT(IN), OPTIONAL :: cells
        REAL(r8k),    DIMENSION(3),   INTENT(IN), OPTIONAL :: origin, spacing
        REAL(r8k),    DIMENSION(:,:), INTENT(IN), OPTIONAL :: points

        END SUBROUTINE abs_setup

        SUBROUTINE struct_pts_setup (me, unit, dims, origin, spacing, points, cells, cell_type)
        CLASS (struct_pts), INTENT(INOUT) :: me
        INTEGER(i4k), INTENT(IN) :: unit
        INTEGER(i4k), DIMENSION(3),   INTENT(IN), OPTIONAL :: dims
        INTEGER(i4k), DIMENSION(:),   INTENT(IN), OPTIONAL :: cell_type
        INTEGER(i4k), DIMENSION(:,:), INTENT(IN), OPTIONAL :: cells
        REAL(r8k),    DIMENSION(3),   INTENT(IN), OPTIONAL :: origin, spacing
        REAL(r8k),    DIMENSION(:,:), INTENT(IN), OPTIONAL :: points

        IF (.NOT. PRESENT(dims) .OR. .NOT. PRESENT(origin) .OR. .NOT. PRESENT(spacing)) THEN
            ERROR STOP 'Bad inputs for struct_pts_setup'
        END IF

        me%unit       = unit
        me%name       = 'STRUCTURED_POINTS'
        me%dimensions = dims
        me%origin     = origin
        me%spacing    = spacing
        me%firstcall  = .FALSE.

        END SUBROUTINE struct_pts_setup

        SUBROUTINE struct_grid_setup (me, unit, dims, origin, spacing, points, cells, cell_type)
        CLASS (struct_grid), INTENT(INOUT) :: me
        INTEGER(i4k), INTENT(IN) :: unit
        INTEGER(i4k), DIMENSION(3),   INTENT(IN), OPTIONAL :: dims
        INTEGER(i4k), DIMENSION(:),   INTENT(IN), OPTIONAL :: cell_type
        INTEGER(i4k), DIMENSION(:,:), INTENT(IN), OPTIONAL :: cells
        REAL(r8k),    DIMENSION(3),   INTENT(IN), OPTIONAL :: origin, spacing
        REAL(r8k),    DIMENSION(:,:), INTENT(IN), OPTIONAL :: points
        IF (.NOT. PRESENT(points) .OR. .NOT. PRESENT(dims)) THEN
            ERROR STOP 'Bad inputs for struct_grid_setup'
        END IF

        me%unit       = unit
        me%name       = 'STRUCTURED_GRID'
        me%dimensions = dims
        me%datatype   = 'double'
        me%n_points   = SIZE(points,DIM=2)
        IF (ALLOCATED(me%points)) DEALLOCATE(me%points)
        me%points = points
        me%firstcall  = .FALSE.

        END SUBROUTINE struct_grid_setup

        SUBROUTINE rectlnr_grid_setup (me, unit, dims, origin, spacing, points, cells, cell_type)
        CLASS (rectlnr_grid), INTENT(INOUT) :: me
        INTEGER(i4k), INTENT(IN) :: unit
        INTEGER(i4k), DIMENSION(3),   INTENT(IN), OPTIONAL :: dims
        INTEGER(i4k), DIMENSION(:),   INTENT(IN), OPTIONAL :: cell_type
        INTEGER(i4k), DIMENSION(:,:), INTENT(IN), OPTIONAL :: cells
        REAL(r8k),    DIMENSION(3),   INTENT(IN), OPTIONAL :: origin, spacing
        REAL(r8k),    DIMENSION(:,:), INTENT(IN), OPTIONAL :: points

        me%name       = 'RECTILINEAR_GRID'

        END SUBROUTINE rectlnr_grid_setup

        SUBROUTINE polygonal_data_setup (me, unit, dims, origin, spacing, points, cells, cell_type)
        CLASS (polygonal_data), INTENT(INOUT) :: me
        INTEGER(i4k), INTENT(IN) :: unit
        INTEGER(i4k), DIMENSION(3),   INTENT(IN), OPTIONAL :: dims
        INTEGER(i4k), DIMENSION(:),   INTENT(IN), OPTIONAL :: cell_type
        INTEGER(i4k), DIMENSION(:,:), INTENT(IN), OPTIONAL :: cells
        REAL(r8k),    DIMENSION(3),   INTENT(IN), OPTIONAL :: origin, spacing
        REAL(r8k),    DIMENSION(:,:), INTENT(IN), OPTIONAL :: points

        me%name       = 'POLYDATA'

        END SUBROUTINE polygonal_data_setup

        SUBROUTINE unstruct_grid_setup (me, unit, dims, origin, spacing, points, cells, cell_type)
        CLASS (unstruct_grid), INTENT(INOUT) :: me
        INTEGER(i4k), INTENT(IN) :: unit
        INTEGER(i4k), DIMENSION(3),   INTENT(IN), OPTIONAL :: dims
        INTEGER(i4k), DIMENSION(:),   INTENT(IN), OPTIONAL :: cell_type
        INTEGER(i4k), DIMENSION(:,:), INTENT(IN), OPTIONAL :: cells
        REAL(r8k),    DIMENSION(3),   INTENT(IN), OPTIONAL :: origin, spacing
        REAL(r8k),    DIMENSION(:,:), INTENT(IN), OPTIONAL :: points
        INTEGER(i4k) :: i

        IF (.NOT. PRESENT(points) .OR. .NOT. PRESENT(cells) .OR. .NOT. PRESENT(cell_type)) THEN
            ERROR STOP 'Bad inputs for unstruct_grid_setup'
        END IF

        me%unit         = unit
        me%name         = 'UNSTRUCTURED_GRID'
        me%datatype     = 'double'
        me%n_points     = SIZE(points,   DIM=2)
        me%n_cells      = SIZE(cells,    DIM=1)
        me%n_cell_types = SIZE(cell_type,DIM=1)
        IF (ALLOCATED(me%points))    DEALLOCATE(me%points)
        me%points       = points
        DO i = 1, UBOUND(cells,DIM=1)
            call me%cell(i)%setup (cells(i,:))
        END DO

        me%firstcall    = .FALSE.

        END SUBROUTINE unstruct_grid_setup
END MODULE vtk_datasets
