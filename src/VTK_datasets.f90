MODULE vtk_datasets
    USE Precision
    USE vtk_cells, ONLY : vtkcell
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
    ! 4) Polygonal data
    ! 5) Unstructured grid
    !

    PRIVATE
    PUBLIC :: dataset, struct_pts, struct_grid, rectlnr_grid, polygonal_data, unstruct_grid

    TYPE :: coordinates
        CHARACTER(LEN=:), ALLOCATABLE :: datatype
        REAL(r8k),        DIMENSION(:), ALLOCATABLE :: coord
    END TYPE coordinates

    TYPE, ABSTRACT :: dataset
        PRIVATE
        CHARACTER(LEN=:), ALLOCATABLE  :: name
        INTEGER(i4k),     DIMENSION(3) :: dimensions
        LOGICAL, PUBLIC                :: firstcall = .TRUE.
    CONTAINS
        PROCEDURE(abs_read),  DEFERRED, PUBLIC :: read
        PROCEDURE(abs_write), DEFERRED, PUBLIC :: write
        PROCEDURE(abs_setup), DEFERRED, PUBLIC :: setup
        PROCEDURE, PRIVATE :: check_for_diffs
        GENERIC, PUBLIC :: OPERATOR(.diff.) => check_for_diffs
    END TYPE dataset

    TYPE, EXTENDS(dataset) :: struct_pts
        !! Structured points
        REAL(r8k), DIMENSION(3) :: origin
        REAL(r8k), DIMENSION(3) :: spacing
    CONTAINS
        PROCEDURE :: read  => struct_pts_read
        PROCEDURE :: write => struct_pts_write
        PROCEDURE :: setup => struct_pts_setup
        PROCEDURE :: check_for_diffs => check_for_diffs_struct_pts
    END TYPE struct_pts

    TYPE, EXTENDS(dataset) :: struct_grid
        !! Structured grid
        INTEGER(i4k)                           :: n_points
        CHARACTER(LEN=:),          ALLOCATABLE :: datatype
        REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: points
    CONTAINS
        PROCEDURE :: read  => struct_grid_read
        PROCEDURE :: write => struct_grid_write
        PROCEDURE :: setup => struct_grid_setup
        PROCEDURE :: check_for_diffs => check_for_diffs_struct_grid
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
        PROCEDURE :: check_for_diffs => check_for_diffs_rectlnr_grid
    END TYPE rectlnr_grid

    TYPE, EXTENDS(dataset) :: polygonal_data
        !! Polygonal data
        INTEGER(i4k)                                :: n_points
        CHARACTER(LEN=:),               ALLOCATABLE :: datatype
        REAL(r8k),      DIMENSION(:,:), ALLOCATABLE :: points
        CLASS(vtkcell), DIMENSION(:),   ALLOCATABLE :: vertices
        CLASS(vtkcell), DIMENSION(:),   ALLOCATABLE :: lines
        CLASS(vtkcell), DIMENSION(:),   ALLOCATABLE :: polygons
        CLASS(vtkcell), DIMENSION(:),   ALLOCATABLE :: triangles
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
        CHARACTER(LEN=:),               ALLOCATABLE :: datatype
        REAL(r8k),      DIMENSION(:,:), ALLOCATABLE :: points
        CLASS(vtkcell), DIMENSION(:),   ALLOCATABLE :: cell
    CONTAINS
        PROCEDURE :: read  => unstruct_grid_read
        PROCEDURE :: write => unstruct_grid_write
        PROCEDURE :: setup => unstruct_grid_setup
    END TYPE unstruct_grid

    CONTAINS
! ****************
! Abstract dataset
! ****************
        SUBROUTINE abs_read (me, unit)
        !>@brief
        !> Reads the dataset information from the .vtk file
        CLASS(dataset), INTENT(OUT) :: me
        INTEGER(i4k),   INTENT(IN)  :: unit
        SELECT TYPE (me)
        END SELECT
        END SUBROUTINE abs_read

        SUBROUTINE abs_write (me, unit)
        !>@brief
        !> Writes the dataset information to the .vtk file
        CLASS(dataset), INTENT(IN) :: me
        INTEGER(i4k),   INTENT(IN) :: unit
        SELECT TYPE (me)
        END SELECT
        END SUBROUTINE abs_write

        SUBROUTINE abs_setup (me, datatype, dims, origin, spacing, points, cells, &
          &                   x_coords, y_coords, z_coords, vertices, lines, polygons, triangles)
        !>@brief
        !> Sets up the dataset with information
        CLASS (dataset),              INTENT(OUT)          :: me
        CLASS(vtkcell), DIMENSION(:), INTENT(IN), OPTIONAL :: vertices, lines, polygons, triangles, cells
        CHARACTER(LEN=*),             INTENT(IN), OPTIONAL :: datatype
        INTEGER(i4k), DIMENSION(3),   INTENT(IN), OPTIONAL :: dims
        REAL(r8k),    DIMENSION(3),   INTENT(IN), OPTIONAL :: origin, spacing
        REAL(r8k),    DIMENSION(:),   INTENT(IN), OPTIONAL :: x_coords, y_coords, z_coords
        REAL(r8k),    DIMENSION(:,:), INTENT(IN), OPTIONAL :: points

        IF (PRESENT(datatype) .OR. PRESENT(dims)     .OR. PRESENT(origin) .OR. &
            PRESENT(spacing)  .OR. PRESENT(points)   .OR. PRESENT(cells)  .OR. &
            PRESENT(x_coords) .OR. PRESENT(y_coords) .OR. PRESENT(z_coords)) THEN
            !! This is done only to avoid compiler warnings. A deferred abstract subroutine should never be called.'
            WRITE(*,*) 'Warning: More information provided to abs_setup than needed.'
        END IF
        END SUBROUTINE abs_setup

        FUNCTION check_for_diffs (me, you) RESULT (diffs)
        !>@brief
        !> Function checks for differences in a dataset
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 12/18/2017
        CLASS(dataset), INTENT(IN) :: me, you
        LOGICAL :: diffs

        diffs = .FALSE.
        IF      (.NOT. SAME_TYPE_AS(me,you))  THEN
            diffs = .TRUE.
        ELSE IF (me%name          /= you%name) THEN
            diffs = .TRUE.
        ELSE IF (me%dimensions(1) /= you%dimensions(1) .OR. &
          &      me%dimensions(2) /= you%dimensions(2) .OR. &
          &      me%dimensions(3) /= you%dimensions(3)) THEN
            diffs = .TRUE.
        END IF

        END FUNCTION check_for_diffs
! *****************
! Structured Points
! *****************
        SUBROUTINE struct_pts_read (me, unit)
        USE Misc, ONLY : interpret_string, def_len
        !>@brief
        !> Reads the structured points dataset information from the .vtk file
        CLASS(struct_pts), INTENT(OUT) :: me
        INTEGER(i4k),      INTENT(IN)  :: unit
        INTEGER(i4k)                   :: iostat
        CHARACTER(LEN=def_len)         :: line
        INTEGER(i4k),     DIMENSION(:), ALLOCATABLE :: ints
        REAL(r8k),        DIMENSION(:), ALLOCATABLE :: reals
        CHARACTER(LEN=:), DIMENSION(:), ALLOCATABLE :: chars

        READ(unit,100,iostat=iostat) line
        CALL interpret_string (line=line, datatype=(/ 'C' /),         ignore='DATASET ',    separator=' ', chars=chars)
        me%name = TRIM(chars(1))

        READ(unit,100,iostat=iostat) line
        CALL interpret_string (line=line, datatype=(/ 'I','I','I' /), ignore='DIMENSIONS ', separator=' ', ints=ints)
        me%dimensions = ints(1:3)

        READ(unit,100,iostat=iostat) line
        CALL interpret_string (line=line, datatype=(/ 'R','R','R' /), ignore='ORIGIN ',     separator=' ', reals=reals)
        me%origin = reals(1:3)

        READ(unit,100,iostat=iostat) line
        CALL interpret_string (line=line, datatype=(/ 'R','R','R' /), ignore='SPACING ',    separator=' ', reals=reals)
        me%spacing = reals(1:3)

100     FORMAT((a))
        END SUBROUTINE struct_pts_read

        SUBROUTINE struct_pts_write (me, unit)
        !>@brief
        !> Writes the structured points dataset information to the .vtk file
        CLASS(struct_pts), INTENT(IN) :: me
        INTEGER(i4k),      INTENT(IN) :: unit

        WRITE(unit,100) me%name
        WRITE(unit,101) me%dimensions
        WRITE(unit,102) me%origin
        WRITE(unit,103) me%spacing
100     FORMAT ('DATASET ',(a))
101     FORMAT ('DIMENSIONS ',*(i0,' '))
102     FORMAT ('ORIGIN ',*(es13.6))
103     FORMAT ('SPACING ',*(es13.6))

        END SUBROUTINE struct_pts_write

        SUBROUTINE struct_pts_setup (me, datatype, dims, origin, spacing, points, cells, &
          &                          x_coords, y_coords, z_coords, vertices, lines, polygons, triangles)
        !>@brief
        !> Sets up the structured points dataset with information
        CLASS (struct_pts),           INTENT(OUT)          :: me
        CLASS(vtkcell), DIMENSION(:), INTENT(IN), OPTIONAL :: vertices, lines, polygons, triangles, cells
        CHARACTER(LEN=*),             INTENT(IN), OPTIONAL :: datatype
        INTEGER(i4k), DIMENSION(3),   INTENT(IN), OPTIONAL :: dims
        REAL(r8k),    DIMENSION(3),   INTENT(IN), OPTIONAL :: origin, spacing
        REAL(r8k),    DIMENSION(:),   INTENT(IN), OPTIONAL :: x_coords, y_coords, z_coords
        REAL(r8k),    DIMENSION(:,:), INTENT(IN), OPTIONAL :: points

        IF (.NOT. PRESENT(dims) .OR. .NOT. PRESENT(origin) .OR. .NOT. PRESENT(spacing)) THEN
            ERROR STOP 'Bad inputs for struct_pts_setup'
        ELSE IF (PRESENT(datatype)) THEN
            WRITE(*,*) 'Warning: More information provided to struct_pts_setup than required. Some info not used.'
        END IF

        me%name       = 'STRUCTURED_POINTS'
        me%dimensions = dims
        me%origin     = origin
        me%spacing    = spacing
        me%firstcall  = .FALSE.

        END SUBROUTINE struct_pts_setup

        FUNCTION check_for_diffs_struct_pts (me, you) RESULT (diffs)
        !>@brief
        !> Function checks for differences in a structured points dataset
        CLASS(struct_pts), INTENT(IN) :: me
        CLASS(dataset),    INTENT(IN) :: you
        LOGICAL :: diffs

        diffs = .FALSE.
        IF      (.NOT. SAME_TYPE_AS(me,you))  THEN
            diffs = .TRUE.
        ELSE IF (me%name          /= you%name) THEN
            diffs = .TRUE.
        ELSE IF (me%dimensions(1) /= you%dimensions(1) .OR. &
          &      me%dimensions(2) /= you%dimensions(2) .OR. &
          &      me%dimensions(3) /= you%dimensions(3)) THEN
            diffs = .TRUE.
        ELSE
            SELECT TYPE (you)
            CLASS IS (struct_pts)
                IF      (me%origin(1)  /= you%origin(1)  .OR. &
                  &      me%origin(2)  /= you%origin(2)  .OR. &
                  &      me%origin(3)  /= you%origin(3))  THEN
                    diffs = .TRUE.
                ELSE IF (me%spacing(1) /= you%spacing(1) .OR. &
                  &      me%spacing(2) /= you%spacing(2) .OR. &
                  &      me%spacing(3) /= you%spacing(3)) THEN
                    diffs = .TRUE.
                END IF
            END SELECT
        END IF

        END FUNCTION check_for_diffs_struct_pts
! ***************
! Structured Grid
! ***************
        SUBROUTINE struct_grid_read (me, unit)
        USE Misc, ONLY : interpret_string, def_len
        !>@brief
        !> Reads the structured grid dataset information from the .vtk file
        CLASS(struct_grid), INTENT(OUT) :: me
        INTEGER(i4k),       INTENT(IN)  :: unit
        INTEGER(i4k)                    :: i, iostat
        INTEGER(i4k), PARAMETER         :: dim = 3
        LOGICAL                         :: end_of_File
        CHARACTER(LEN=def_len)          :: line
        INTEGER(i4k),     DIMENSION(:),   ALLOCATABLE :: ints
        REAL(r8k),        DIMENSION(:),   ALLOCATABLE :: reals
        CHARACTER(LEN=:), DIMENSION(:),   ALLOCATABLE :: chars

        READ(unit,100,iostat=iostat) line
        CALL interpret_string (line=line, datatype=(/ 'C' /),         ignore='DATASET ',    separator=' ', chars=chars)
        me%name = TRIM(chars(1))

        READ(unit,100,iostat=iostat) line
        CALL interpret_string (line=line, datatype=(/ 'I','I','I' /), ignore='DIMENSIONS ', separator=' ', ints=ints)
        me%dimensions = ints(1:3)

        READ(unit,100,iostat=iostat) line
        CALL interpret_string (line=line, datatype=(/ 'I','C' /),     ignore='POINTS ',     separator=' ', ints=ints, chars=chars)
        me%n_points= ints(1); me%datatype = TRIM(chars(1))

        ALLOCATE(me%points(1:dim,1:me%n_points)); end_of_file  = .FALSE.

        get_points: DO i = 1, me%n_points
            READ(unit,100,iostat=iostat) line
            end_of_file = (iostat < 0)
            IF (end_of_file) THEN
                EXIT get_points
            ELSE IF (TRIM(line) == '') THEN
                CYCLE     !! Skip blank lines
            ELSE
                CALL interpret_string (line=line, datatype=(/ 'R','R','R' /), separator=' ', reals=reals)
                me%points(1:dim,i) = reals(1:dim)
            END IF
        END DO get_points

100     FORMAT((a))
        END SUBROUTINE struct_grid_read

        SUBROUTINE struct_grid_write (me, unit)
        !>@brief
        !> Writes the structured grid dataset information to the .vtk file
        CLASS(struct_grid), INTENT(IN) :: me
        INTEGER(i4k),       INTENT(IN) :: unit
        INTEGER(i4k) :: i

        WRITE(unit,100) me%name
        WRITE(unit,101) me%dimensions
        WRITE(unit,102) me%n_points, me%datatype
        DO i = 1, me%n_points
            WRITE(unit,103) me%points(1:3,i)
        END DO

100     FORMAT ('DATASET ',(a))
101     FORMAT ('DIMENSIONS ',*(i0,' '))
102     FORMAT ('POINTS ',(i0),' ',(a))
103     FORMAT (*(es13.6))

        END SUBROUTINE struct_grid_write

        SUBROUTINE struct_grid_setup (me, datatype, dims, origin, spacing, points, cells, &
          &                           x_coords, y_coords, z_coords, vertices, lines, polygons, triangles)
        !>@brief
        !> Sets up the structured grid dataset with information
        CLASS (struct_grid),          INTENT(OUT)          :: me
        CLASS(vtkcell), DIMENSION(:), INTENT(IN), OPTIONAL :: vertices, lines, polygons, triangles, cells
        CHARACTER(LEN=*),             INTENT(IN), OPTIONAL :: datatype
        INTEGER(i4k), DIMENSION(3),   INTENT(IN), OPTIONAL :: dims
        REAL(r8k),    DIMENSION(3),   INTENT(IN), OPTIONAL :: origin, spacing
        REAL(r8k),    DIMENSION(:),   INTENT(IN), OPTIONAL :: x_coords, y_coords, z_coords
        REAL(r8k),    DIMENSION(:,:), INTENT(IN), OPTIONAL :: points
        IF (.NOT. PRESENT(points) .OR. .NOT. PRESENT(dims)) THEN
            ERROR STOP 'Bad inputs for struct_grid_setup'
        END IF

        me%name       = 'STRUCTURED_GRID'
        me%dimensions = dims
        IF (PRESENT(datatype)) THEN
            me%datatype = datatype
        ELSE
            me%datatype = 'double'
        END IF
        me%n_points   = SIZE(points,DIM=2)
        me%points     = points
        me%firstcall  = .FALSE.

        END SUBROUTINE struct_grid_setup

        FUNCTION check_for_diffs_struct_grid (me, you) RESULT (diffs)
        !>@brief
        !> Function checks for differences in a structured grid dataset
        CLASS(struct_grid), INTENT(IN) :: me
        CLASS(dataset),     INTENT(IN) :: you
        INTEGER(i4k) :: i, j
        LOGICAL      :: diffs

        diffs = .FALSE.
        IF      (.NOT. SAME_TYPE_AS(me,you))  THEN
            diffs = .TRUE.
        ELSE IF (me%name          /= you%name) THEN
            diffs = .TRUE.
        ELSE IF (me%dimensions(1) /= you%dimensions(1) .OR. &
          &      me%dimensions(2) /= you%dimensions(2) .OR. &
          &      me%dimensions(3) /= you%dimensions(3)) THEN
            diffs = .TRUE.
        ELSE
            SELECT TYPE (you)
            CLASS IS (struct_grid)
                IF      (me%n_points /= you%n_points) THEN
                    diffs = .TRUE.
                ELSE IF (me%datatype /= you%datatype) THEN
                    diffs = .TRUE.
                ELSE IF (SIZE(me%points,DIM=1) /= SIZE(you%points,DIM=1) .OR. &
                  &      SIZE(me%points,DIM=2) /= SIZE(you%points,DIM=2)) THEN
                    diffs = .TRUE.
                ELSE
                    DO i = 1, SIZE(me%points,DIM=1)
                        DO j = 1, SIZE(me%points,DIM=2)
                            IF (me%points(i,j) /= you%points(i,j)) THEN
                                diffs = .TRUE.
                            END IF
                        END DO
                    END DO
                END IF
            END SELECT
        END IF

        END FUNCTION check_for_diffs_struct_grid
! ****************
! Rectilinear Grid
! ****************
        SUBROUTINE rectlnr_grid_read (me, unit)
        USE Misc, ONLY : interpret_string, def_len
        !>@brief
        !> Reads the rectilinear grid dataset information from the .vtk file
        CLASS(rectlnr_grid), INTENT(OUT) :: me
        INTEGER(i4k),        INTENT(IN)  :: unit
        INTEGER(i4k)                     :: i, j, iostat
        INTEGER(i4k),        PARAMETER   :: dim = 3
        LOGICAL                          :: end_of_File
        CHARACTER(LEN=def_len)           :: line
        INTEGER(i4k),      DIMENSION(:),   ALLOCATABLE :: ints
        REAL(r8k),         DIMENSION(:),   ALLOCATABLE :: reals
        CHARACTER(LEN=:),  DIMENSION(:),   ALLOCATABLE :: chars
        CHARACTER(LEN=13), DIMENSION(3),   PARAMETER   :: descr_coord = &
          & [ 'X_COORDINATES', 'Y_COORDINATES', 'Z_COORDINATES' ]

        READ(unit,100,iostat=iostat) line
        CALL interpret_string (line=line, datatype=(/ 'C' /),         ignore='DATASET ',     separator=' ', chars=chars)
        me%name = TRIM(chars(1))

        READ(unit,100,iostat=iostat) line
        CALL interpret_string (line=line, datatype=(/ 'I','I','I' /), ignore='DIMENSIONS ',  separator=' ', ints=ints)
        me%dimensions = ints(1:3); ALLOCATE(me%x%coord(1:ints(1)), me%y%coord(1:ints(2)), me%z%coord(1:ints(3)))

        end_of_file = .FALSE.; i = 0

        get_coords: DO
            i = i + 1
            READ(unit,100,iostat=iostat) line
            CALL interpret_string (line=line, datatype=(/ 'I','C' /), ignore=descr_coord(i), separator=' ', ints=ints, chars=chars)
            SELECT CASE (i)
            CASE (1)
                me%x%datatype = TRIM(chars(1))
            CASE (2)
                me%y%datatype = TRIM(chars(1))
            CASE (3)
                me%z%datatype = TRIM(chars(1))
            END SELECT

            READ(unit,100,iostat=iostat) line
            end_of_file = (iostat < 0)
            IF (end_of_file) THEN
                EXIT get_coords
            ELSE
                j = 0
                get_vals: DO
                    j = j + 1
                    CALL interpret_string (line=line, datatype=(/ 'R' /), separator=' ', reals=reals)
                    SELECT CASE (i)
                    CASE (1)
                        me%x%coord(j) = reals(1)
                    CASE (2)
                        me%y%coord(j) = reals(1)
                    CASE (3)
                        me%z%coord(j) = reals(1)
                    END SELECT
                    IF (line == '') EXIT get_vals
                END DO get_vals
            END IF
            IF (i == dim) EXIT get_coords  !! Filled up array points
        END DO get_coords

100     FORMAT((a))
        END SUBROUTINE rectlnr_grid_read

        SUBROUTINE rectlnr_grid_write (me, unit)
        !>@brief
        !> Writes the rectilinear grid dataset information to the .vtk file
        CLASS(rectlnr_grid), INTENT(IN) :: me
        INTEGER(i4k),        INTENT(IN) :: unit

        WRITE(unit,100) me%name
        WRITE(unit,101) me%dimensions
        WRITE(unit,102) me%dimensions(1), me%x%datatype
        WRITE(unit,110) me%x%coord
        WRITE(unit,103) me%dimensions(2), me%y%datatype
        WRITE(unit,110) me%y%coord
        WRITE(unit,104) me%dimensions(3), me%z%datatype
        WRITE(unit,110) me%z%coord

100     FORMAT ('DATASET ',(a))
101     FORMAT ('DIMENSIONS ',*(i0,' '))
102     FORMAT ('X_COORDINATES ',i0,' ',(a))
103     FORMAT ('Y_COORDINATES ',i0,' ',(a))
104     FORMAT ('Z_COORDINATES ',i0,' ',(a))
110     FORMAT (*(es13.6))

        END SUBROUTINE rectlnr_grid_write

        SUBROUTINE rectlnr_grid_setup (me, datatype, dims, origin, spacing, points, cells, &
          &                            x_coords, y_coords, z_coords, vertices, lines, polygons, triangles)
        !>@brief
        !> Sets up the rectilinear grid dataset with information
        CLASS (rectlnr_grid),         INTENT(OUT)          :: me
        CLASS(vtkcell), DIMENSION(:), INTENT(IN), OPTIONAL :: vertices, lines, polygons, triangles, cells
        CHARACTER(LEN=*),             INTENT(IN), OPTIONAL :: datatype
        INTEGER(i4k), DIMENSION(3),   INTENT(IN), OPTIONAL :: dims
        REAL(r8k),    DIMENSION(3),   INTENT(IN), OPTIONAL :: origin, spacing
        REAL(r8k),    DIMENSION(:),   INTENT(IN), OPTIONAL :: x_coords, y_coords, z_coords
        REAL(r8k),    DIMENSION(:,:), INTENT(IN), OPTIONAL :: points

        IF (.NOT. PRESENT(dims)     .OR. .NOT. PRESENT(x_coords) .OR. &
          & .NOT. PRESENT(y_coords) .OR. .NOT. PRESENT(z_coords)) THEN
            ERROR STOP 'Bad inputs for rectlnr_grid_setup. Not enough information provided.'
        ELSE IF (dims(1) /= SIZE(x_coords) .OR. dims(2) /= SIZE(y_coords) .OR. dims(3) /= SIZE(z_coords)) THEN
            ERROR STOP 'Bad inputs for rectlnr_grid_setup. Dims is not equal to size of coords.'
        END IF

        me%name       = 'RECTILINEAR_GRID'
        me%dimensions = dims
        IF (PRESENT(datatype)) THEN
            me%x%datatype = datatype
        ELSE
            me%x%datatype = 'double'
        END IF
        me%y%datatype = me%x%datatype; me%z%datatype = me%x%datatype
        me%x%coord    = x_coords
        me%y%coord    = y_coords
        me%z%coord    = z_coords
        me%firstcall  = .FALSE.

        END SUBROUTINE rectlnr_grid_setup

        FUNCTION check_for_diffs_rectlnr_grid (me, you) RESULT (diffs)
        !>@brief
        !> Function checks for differences in a rectilinear grid dataset
        CLASS(rectlnr_grid), INTENT(IN) :: me
        CLASS(dataset),      INTENT(IN) :: you
        INTEGER(i4k) :: i
        LOGICAL      :: diffs

        diffs = .FALSE.
        IF      (.NOT. SAME_TYPE_AS(me,you))  THEN
            diffs = .TRUE.
        ELSE IF (me%name          /= you%name) THEN
            diffs = .TRUE.
        ELSE IF (me%dimensions(1) /= you%dimensions(1) .OR. &
          &      me%dimensions(2) /= you%dimensions(2) .OR. &
          &      me%dimensions(3) /= you%dimensions(3)) THEN
            diffs = .TRUE.
        ELSE
            SELECT TYPE (you)
            CLASS IS (rectlnr_grid)
                IF      (me%x%datatype /= you%x%datatype .OR. &
                  &      me%y%datatype /= you%y%datatype .OR. &
                  &      me%z%datatype /= you%z%datatype) THEN
                    diffs = .TRUE.
                ELSE IF (SIZE(me%x%coord) /= SIZE(you%x%coord) .OR. &
                  &      SIZE(me%y%coord) /= SIZE(you%y%coord) .OR. &
                  &      SIZE(me%z%coord) /= SIZE(you%z%coord)) THEN
                    diffs = .TRUE.
                ELSE
                    DO i = 1, SIZE(me%x%coord)
                        IF (me%x%coord(i) /= you%x%coord(i)) THEN
                            diffs = .TRUE.
                        END IF
                    END DO
                    DO i = 1, SIZE(me%y%coord)
                        IF (me%y%coord(i) /= you%y%coord(i)) THEN
                            diffs = .TRUE.
                        END IF
                    END DO
                    DO i = 1, SIZE(me%z%coord)
                        IF (me%z%coord(i) /= you%z%coord(i)) THEN
                            diffs = .TRUE.
                        END IF
                    END DO
                END IF
            END SELECT
        END IF

        END FUNCTION check_for_diffs_rectlnr_grid
! **************
! Polygonal Data
! **************
        SUBROUTINE polygonal_data_read (me, unit)
        USE Misc,      ONLY : interpret_string, def_len
        USE vtk_cells, ONLY : poly_vertex, poly_line, polygon, triangle_strip
        !>@brief
        !> Reads the polygonal data dataset information from the .vtk file
        CLASS(polygonal_data), INTENT(OUT) :: me
        INTEGER(i4k),          INTENT(IN)  :: unit
        INTEGER(i4k)                       :: i, j, iostat, n, descr_size, n_points
        INTEGER(i4k), PARAMETER            :: dim = 3
        LOGICAL                            :: end_of_File
        CHARACTER(LEN=def_len)             :: line
        CHARACTER(LEN=:), ALLOCATABLE      :: descr
        INTEGER(i4k),      DIMENSION(:), ALLOCATABLE :: ints, dummy, points
        REAL(r8k),         DIMENSION(:), ALLOCATABLE :: reals
        CHARACTER(LEN=:),  DIMENSION(:), ALLOCATABLE :: chars

        READ(unit,100,iostat=iostat) line
        CALL interpret_string (line=line, datatype=(/ 'C' /),         ignore='DATASET ',     separator=' ', chars=chars)
        me%name = TRIM(chars(1))

        READ(unit,100,iostat=iostat) line
        CALL interpret_string (line=line, datatype=(/ 'I','C' /),     ignore='POINTS ',     separator=' ', ints=ints, chars=chars)
        me%n_points= ints(1); me%datatype = TRIM(chars(1))

        ALLOCATE(me%points(1:3,1:me%n_points)); end_of_file = .FALSE.; i = 0

        get_points: DO
            READ(unit,100,iostat=iostat) line
            end_of_file = (iostat < 0)
            IF (end_of_file) THEN
                EXIT get_points
            ELSE IF (TRIM(line) == '') THEN
                CYCLE     !! Skip blank lines
            ELSE
                i = i + 1
                CALL interpret_string (line=line, datatype=(/ 'R','R','R' /), separator=' ', reals=reals)
                me%points(1:dim,i) = reals(1:dim)
            END IF
            IF (i == SIZE(me%points,DIM=2)) EXIT get_points  !! Filled up array points
        END DO get_points

        end_of_file = .FALSE.; i = 0

        get_keywords: DO
            READ(unit,100,iostat=iostat) line
            end_of_file = (iostat < 0)
            IF (end_of_file) THEN
                EXIT get_keywords
            ELSE IF (TRIM(line) == '') THEN
                CYCLE     !! Skip blank lines
            END IF

            CALL interpret_string (line=line, datatype=(/ 'C','I','I' /), separator=' ', ints=ints, chars=chars)
            descr = TRIM(chars(1)); n = ints(1); descr_size = ints(2)

            SELECT CASE (descr)
            CASE ('VERTICES')
                ALLOCATE(poly_vertex::me%vertices(1:n))
            CASE ('LINES')
                ALLOCATE(poly_line::me%lines(1:n))
            CASE ('POLYGONS')
                ALLOCATE(polygon::me%polygons(1:n))
            CASE ('TRIANGLE_STRIPS')
                ALLOCATE(triangle_strip::me%triangles(1:n))
            CASE ('POINT_DATA', 'CELL_DATA')
                EXIT get_keywords !! No additional information was provided for polygonal_data
            CASE DEFAULT
                ERROR STOP 'Bad value read for additional polygon information. Terminated in polygonal_data_read'
            END SELECT

            DO i = 1, n
                READ(unit,100,iostat=iostat) line
                end_of_file = (iostat < 0)
                IF (end_of_file) THEN
                    EXIT get_keywords
                ELSE
                    ! Extract values
                    j = 0; IF(ALLOCATED(points)) DEALLOCATE(points)
                    get_vals: DO
                        j = j + 1
                        CALL interpret_string (line=line, datatype=(/ 'I' /), separator=' ', ints=ints)
                        IF (j == 1) THEN
                            n_points = ints(1)
                        ELSE
                            ALLOCATE(dummy(1:j-1))
                            dummy(j-1) = ints(1)
                            IF (j > 2) dummy(1:j-2) = points
                            IF (ALLOCATED(points)) DEALLOCATE(points)
                            CALL MOVE_ALLOC(dummy, points)
                        END IF
                        IF (line == '') EXIT get_vals
                    END DO get_vals

                    ! Save values
                    SELECT CASE (descr)
                    CASE ('VERTICES')
                        me%vertices(i)%n_points  = n_points
                        me%vertices(i)%points    = points
                    CASE ('LINES')
                        me%lines(i)%n_points     = n_points
                        me%lines(i)%points       = points
                    CASE ('POLYGONS')
                        me%polygons(i)%n_points  = n_points
                        me%polygons(i)%points    = points
                    CASE ('TRIANGLE_STRIPS')
                        me%triangles(i)%n_points = n_points
                        me%triangles(i)%points   = points
                    END SELECT
                END IF
            END DO
        END DO get_keywords

100     FORMAT((a))
        END SUBROUTINE polygonal_data_read

        SUBROUTINE polygonal_data_write (me, unit)
        !>@brief
        !> Writes the polygonal data dataset information to the .vtk file
        CLASS(polygonal_data), INTENT(IN) :: me
        INTEGER(i4k),          INTENT(IN) :: unit
        INTEGER(i4k) :: i, n, size_cnt

        WRITE(unit,100) me%name
        WRITE(unit,101) me%n_points, me%datatype

        DO i = 1, me%n_points
            WRITE(unit,102) me%points(1:3,i)
        END DO

        !! The following options are not required inputs
        IF (ALLOCATED(me%vertices)) THEN
            ! Calculate sizes
            n        = SIZE(me%vertices,DIM=1)
            size_cnt = n
            DO i = 1, n
                size_cnt = size_cnt + me%vertices(i)%n_points
            END DO
            ! Write values
            WRITE(unit,103) n, size_cnt
            DO i = 1, n
                CALL me%vertices(i)%write(unit)
            END DO
        END IF

        IF (ALLOCATED(me%lines)) THEN
            ! Calculate sizes
            n        = SIZE(me%lines,DIM=1)
            size_cnt = n
            DO i = 1, n
                size_cnt = size_cnt + me%lines(i)%n_points
            END DO
            ! Write values
            WRITE(unit,104) n, size_cnt
            DO i = 1, n
                CALL me%lines(i)%write(unit)
            END DO
        END IF

        IF (ALLOCATED(me%polygons)) THEN
            ! Calculate sizes
            n        = SIZE(me%polygons,DIM=1)
            size_cnt = n
            DO i = 1, n
                size_cnt = size_cnt + me%polygons(i)%n_points
            END DO
            ! Write values
            WRITE(unit,105) n, size_cnt
            DO i = 1, n
                CALL me%polygons(i)%write(unit)
            END DO
        END IF

        IF (ALLOCATED(me%triangles)) THEN
            ! Calculate sizes
            n        = SIZE(me%triangles,DIM=1)
            size_cnt = n
            DO i = 1, n
                size_cnt = size_cnt + me%triangles(i)%n_points
            END DO
            ! Write values
            WRITE(unit,106) n, size_cnt
            DO i = 1, n
                CALL me%triangles(i)%write(unit)
            END DO
        END IF

100     FORMAT ('DATASET ',(a))
101     FORMAT ('POINTS ',(i0),' ',(a))
102     FORMAT (*(es13.6))
103     FORMAT ('VERTICES ',(i0),' ',(i0))
104     FORMAT ('LINES ',(i0),' ',(i0))
105     FORMAT ('POLYGONS ',(i0),' ',(i0))
106     FORMAT ('TRIANGLE_STRIPS ',(i0),' ',(i0))

        END SUBROUTINE polygonal_data_write

        SUBROUTINE polygonal_data_setup (me, datatype, dims, origin, spacing, points, cells, &
          &                              x_coords, y_coords, z_coords, vertices, lines, polygons, triangles)
        !>@brief
        !> Sets up the polygonal data dataset with information
        CLASS (polygonal_data),       INTENT(OUT)          :: me
        CLASS(vtkcell), DIMENSION(:), INTENT(IN), OPTIONAL :: vertices, lines, polygons, triangles, cells
        CHARACTER(LEN=*),             INTENT(IN), OPTIONAL :: datatype
        INTEGER(i4k), DIMENSION(3),   INTENT(IN), OPTIONAL :: dims
        REAL(r8k),    DIMENSION(3),   INTENT(IN), OPTIONAL :: origin, spacing
        REAL(r8k),    DIMENSION(:),   INTENT(IN), OPTIONAL :: x_coords, y_coords, z_coords
        REAL(r8k),    DIMENSION(:,:), INTENT(IN), OPTIONAL :: points

        me%name       = 'POLYDATA'
        IF (PRESENT(datatype)) THEN
            me%datatype = datatype
        ELSE
            me%datatype = 'double'
        END IF
        me%n_points   = SIZE(points,DIM=2)
        me%points     = points
        me%firstcall  = .FALSE.
        IF (PRESENT(vertices )) ALLOCATE(me%vertices, source=vertices )
        IF (PRESENT(lines    )) ALLOCATE(me%lines,    source=lines    )
        IF (PRESENT(polygons )) ALLOCATE(me%polygons, source=polygons )
        IF (PRESENT(triangles)) ALLOCATE(me%triangles,source=triangles)

        END SUBROUTINE polygonal_data_setup
! *****************
! Unstructured Grid
! *****************
        SUBROUTINE unstruct_grid_read (me, unit)
        USE Misc,      ONLY : interpret_string, def_len
        USE vtk_cells, ONLY : vtkcell, poly_vertex, set_cell_type
        !>@brief
        !> Reads the unstructured grid dataset information from the .vtk file
        CLASS(unstruct_grid), INTENT(OUT) :: me
        INTEGER(i4k),         INTENT(IN)  :: unit
        CLASS(vtkcell), ALLOCATABLE       :: dummy_cell
        INTEGER(i4k)                      :: i, iostat
        INTEGER(i4k), PARAMETER           :: dim = 3
        LOGICAL                           :: end_of_File
        CHARACTER(LEN=def_len)            :: line
        INTEGER(i4k),      DIMENSION(:), ALLOCATABLE :: ints, points
        REAL(r8k),         DIMENSION(:), ALLOCATABLE :: reals
        CHARACTER(LEN=:),  DIMENSION(:), ALLOCATABLE :: chars
        TYPE temp_points
            INTEGER(i4k), DIMENSION(:), ALLOCATABLE  :: points
        END TYPE temp_points
        TYPE (temp_points), DIMENSION(:), ALLOCATABLE :: read_points

        READ(unit,100,iostat=iostat) line
        CALL interpret_string (line=line, datatype=(/ 'C' /),         ignore='DATASET ',     separator=' ', chars=chars)
        me%name = TRIM(chars(1))

        READ(unit,100,iostat=iostat) line
        CALL interpret_string (line=line, datatype=(/ 'I','C' /),     ignore='POINTS ',     separator=' ', ints=ints, chars=chars)
        me%n_points= ints(1); me%datatype = TRIM(chars(1))

        ALLOCATE(me%points(1:3,1:me%n_points)); end_of_file = .FALSE.

        get_points: DO i = 1, me%n_points
            READ(unit,100,iostat=iostat) line
            end_of_file = (iostat < 0)
            IF (end_of_file) THEN
                EXIT get_points
            ELSE IF (TRIM(line) == '') THEN
                CYCLE     !! Skip blank lines
            ELSE
                CALL interpret_string (line=line, datatype=(/ 'R','R','R' /), separator=' ', reals=reals)
                me%points(1:dim,i) = reals(1:dim)
            END IF
        END DO get_points

        ! Get the cell information
        READ(unit,100,iostat=iostat) line
        CALL interpret_string (line=line, datatype=(/ 'I','I' /), ignore='POINTS ', separator=' ', ints=ints)
        me%n_cells = ints(1); me%size = ints(2); ALLOCATE(read_points(1:ints(1)))

        end_of_file = .FALSE.
        get_cells: DO i = 1, me%n_cells
            !! Temporary work around
            IF (ALLOCATED(dummy_cell)) DEALLOCATE(dummy_cell)
            ALLOCATE(poly_vertex::dummy_cell)
            CALL dummy_cell%read(unit)
            read_points(i)%points = dummy_cell%points
        END DO get_cells

        ! Get the cell type information
        READ(unit,100,iostat=iostat) line
        CALL interpret_string (line=line, datatype=(/ 'I' /), ignore='CELL_TYPES ', separator=' ', ints=ints)
        me%n_cell_types = ints(1)

        end_of_file = .FALSE.
        get_cell_type: DO i = 1, me%n_cell_types
            READ(unit,100,iostat=iostat) line
            end_of_file = (iostat < 0)
            IF (end_of_file) THEN
                EXIT get_cell_type
            ELSE IF (TRIM(line) == '') THEN
                CYCLE     !! Skip blank lines
            ELSE
                !! TODO: This is temporary. Need to make each cell be able to be allocated independently
                CALL interpret_string (line=line, datatype=(/ 'I' /), separator=' ', ints=ints)
                !CALL set_cell_type (me%cell(i), ints(1))
                CALL set_cell_type (dummy_cell, ints(1))
                IF (.NOT. ALLOCATED(me%cell)) ALLOCATE(me%cell(1:me%n_cells),mold=dummy_cell)
                CALL me%cell(i)%setup (read_points(i)%points)
                DEALLOCATE(dummy_cell)
            END IF
        END DO get_cell_type

100     FORMAT((a))
        END SUBROUTINE unstruct_grid_read

        SUBROUTINE unstruct_grid_write (me, unit)
        !>@brief
        !> Writes the unstructured grid dataset information from the .vtk file
        CLASS(unstruct_grid), INTENT(IN) :: me
        INTEGER(i4k),         INTENT(IN) :: unit
        INTEGER(i4k) :: i

        WRITE(unit,100) me%name
        WRITE(unit,101) me%n_points, me%datatype

        DO i = 1, me%n_points
            WRITE(unit,102) me%points(1:3,i)
        END DO

        WRITE(unit,103) me%n_cells, me%size
        DO i = 1, me%n_cells
            CALL me%cell(i)%write(unit)
        END DO

        WRITE(unit,104) me%n_cell_types
        DO i = 1, me%n_cell_types
            WRITE(unit,105) me%cell(i)%type
        END DO

100     FORMAT ('DATASET ',(a))
101     FORMAT ('POINTS ',(i0),' ',(a))
102     FORMAT (*(es13.6))
103     FORMAT ('CELLS ',(i0),' ',(i0))
104     FORMAT ('CELL_TYPES ',(i0))
105     FORMAT (' ',i0)

        END SUBROUTINE unstruct_grid_write

        SUBROUTINE unstruct_grid_setup (me, datatype, dims, origin, spacing, points, cells, &
          &                             x_coords, y_coords, z_coords, vertices, lines, polygons, triangles)
        !>@brief
        !> Sets up the unstructured grid dataset with information
        CLASS (unstruct_grid),        INTENT(OUT)          :: me
        CLASS(vtkcell), DIMENSION(:), INTENT(IN), OPTIONAL :: vertices, lines, polygons, triangles, cells
        CHARACTER(LEN=*),             INTENT(IN), OPTIONAL :: datatype
        INTEGER(i4k), DIMENSION(3),   INTENT(IN), OPTIONAL :: dims
        REAL(r8k),    DIMENSION(3),   INTENT(IN), OPTIONAL :: origin, spacing
        REAL(r8k),    DIMENSION(:),   INTENT(IN), OPTIONAL :: x_coords, y_coords, z_coords
        REAL(r8k),    DIMENSION(:,:), INTENT(IN), OPTIONAL :: points
        INTEGER(i4k) :: i, size_cnt

        IF (.NOT. PRESENT(points) .OR. .NOT. PRESENT(cells)) THEN
            ERROR STOP 'Bad inputs for unstruct_grid_setup'
        END IF

        me%name         = 'UNSTRUCTURED_GRID'
        IF (PRESENT(datatype)) THEN
            me%datatype = datatype
        ELSE
            me%datatype = 'double'
        END IF
        me%n_points     = SIZE(points, DIM=2)
        me%n_cells      = SIZE(cells,  DIM=1)
        me%n_cell_types = SIZE(cells,  DIM=1)
        me%points       = points
        ALLOCATE(me%cell,source=cells)
        size_cnt = me%n_cells
        DO i = 1, me%n_cells
            size_cnt = size_cnt + me%cell(i)%n_points
        END DO
        me%size         = size_cnt
        me%firstcall    = .FALSE.

        END SUBROUTINE unstruct_grid_setup
END MODULE vtk_datasets
