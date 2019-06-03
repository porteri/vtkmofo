SUBMODULE (vtk_datasets) vtk_datasets_implementation
    USE Precision, ONLY : i4k, r8k
    USE vtk_cells, ONLY : vtkcell
    IMPLICIT NONE
    !! author: Ian Porter
    !! date: 12/1/2017
    !!
    !! This module contains the dataset formats for vtk format
    !!
    !! The following dataset formats are available:
    !! 1) Structured points
    !! 2) Structured grid
    !! 3) Rectilinear grid
    !! 4) Polygonal data
    !! 5) Unstructured grid
    !!
    CONTAINS
!! ****************
!! Abstract dataset
!! ****************

        MODULE PROCEDURE init
        USE Misc, ONLY : to_lowercase
        IMPLICIT NONE
        !!
        !! Initializes the dataset with information
        CHARACTER(LEN=:), ALLOCATABLE :: data_type  !! Internal variable for datatype

        IF (PRESENT(datatype)) THEN
            ALLOCATE(data_type, source=datatype)
        ELSE
            ALLOCATE(data_type, source='double')
        END IF

        SELECT TYPE (me)
        CLASS IS (struct_pts)
            CALL me%setup(dims, origin, spacing)
        CLASS IS (struct_grid)
            CALL me%setup(dims, points)
        CLASS IS (rectlnr_grid)
            CALL me%setup(dims, x_coords, y_coords, z_coords, data_type)
        CLASS IS (polygonal_data)
            CALL me%setup(points, vertices, lines, polygons, triangles)
        CLASS IS (unstruct_grid)
            IF (PRESENT(cell_list)) THEN
                CALL me%setup(points, cell_list)
            ELSE
                CALL me%setup(points, cells)
            END IF
        CLASS DEFAULT
            ERROR STOP 'Generic class not defined for vtkmofo class dataset'
        END SELECT

        me%datatype = to_lowercase(data_type)

        END PROCEDURE init

        MODULE PROCEDURE check_for_diffs
        !! author: Ian Porter
        !! date: 12/18/2017
        !!
        !! Function checks for differences in a dataset
        !!
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

        END PROCEDURE check_for_diffs
! *****************
! Structured Points
! *****************
        MODULE PROCEDURE struct_pts_read
        USE Misc, ONLY : interpret_string, def_len, char_dt
        !!
        !! Reads the structured points dataset information from the .vtk file
        INTEGER(i4k)                   :: iostat
        CHARACTER(LEN=def_len)         :: line
        INTEGER(i4k),  DIMENSION(:), ALLOCATABLE :: ints
        REAL(r8k),     DIMENSION(:), ALLOCATABLE :: reals
        TYPE(char_dt), DIMENSION(:), ALLOCATABLE :: chars

        READ(unit,100,iostat=iostat) line
        CALL interpret_string (line=line, datatype=[ 'C' ],         ignore='DATASET ',    separator=' ', chars=chars)
        me%name = TRIM(chars(1)%text)

        READ(unit,100,iostat=iostat) line
        CALL interpret_string (line=line, datatype=[ 'I','I','I' ], ignore='DIMENSIONS ', separator=' ', ints=ints)
        me%dimensions = ints(1:3)

        READ(unit,100,iostat=iostat) line
        CALL interpret_string (line=line, datatype=[ 'R','R','R' ], ignore='ORIGIN ',     separator=' ', reals=reals)
        me%origin = reals(1:3)

        READ(unit,100,iostat=iostat) line
        CALL interpret_string (line=line, datatype=[ 'R','R','R' ], ignore='SPACING ',    separator=' ', reals=reals)
        me%spacing = reals(1:3)

100     FORMAT((a))
        END PROCEDURE struct_pts_read

        MODULE PROCEDURE struct_pts_write
        !!
        !! Writes the structured points dataset information to the .vtk file

        WRITE(unit,100) me%name
        WRITE(unit,101) me%dimensions
        WRITE(unit,102) me%origin
        WRITE(unit,103) me%spacing

100     FORMAT ('DATASET ',(a))
101     FORMAT ('DIMENSIONS ',*(i0,' '))
102     FORMAT ('ORIGIN ',*(es13.6))
103     FORMAT ('SPACING ',*(es13.6))

        END PROCEDURE struct_pts_write

        MODULE PROCEDURE struct_pts_setup
        !!
        !! Sets up the structured points dataset with information

        me%name       = 'STRUCTURED_POINTS'
        me%dimensions = dims
        me%origin     = origin
        me%spacing    = spacing
        me%firstcall  = .FALSE.

        END PROCEDURE struct_pts_setup

        MODULE PROCEDURE check_for_diffs_struct_pts
        !!
        !! Function checks for differences in a structured points dataset

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

        END PROCEDURE check_for_diffs_struct_pts
! ***************
! Structured Grid
! ***************
        MODULE PROCEDURE struct_grid_read
        USE Misc, ONLY : interpret_string, def_len, char_dt
        !!
        !! Reads the structured grid dataset information from the .vtk file
        INTEGER(i4k)                    :: i, iostat
        INTEGER(i4k), PARAMETER         :: dim = 3
        LOGICAL                         :: end_of_File
        CHARACTER(LEN=def_len)          :: line
        INTEGER(i4k),  DIMENSION(:),   ALLOCATABLE :: ints
        REAL(r8k),     DIMENSION(:),   ALLOCATABLE :: reals
        TYPE(char_dt), DIMENSION(:),   ALLOCATABLE :: chars

        READ(unit,100,iostat=iostat) line
        CALL interpret_string (line=line, datatype=[ 'C' ],         ignore='DATASET ',    separator=' ', chars=chars)
        me%name = TRIM(chars(1)%text)
        IF (ALLOCATED(chars)) DEALLOCATE(chars)

        READ(unit,100,iostat=iostat) line
        CALL interpret_string (line=line, datatype=[ 'I','I','I' ], ignore='DIMENSIONS ', separator=' ', ints=ints)
        me%dimensions = ints(1:3)

        READ(unit,100,iostat=iostat) line
        CALL interpret_string (line=line, datatype=[ 'I','C' ],     ignore='POINTS ',     separator=' ', ints=ints, chars=chars)
        me%n_points= ints(1); me%datatype = TRIM(chars(1)%text)

        ALLOCATE(me%points(1:dim,1:me%n_points)); end_of_file  = .FALSE.

        get_points: DO i = 1, me%n_points
            READ(unit,100,iostat=iostat) line
            end_of_file = (iostat < 0)
            IF (end_of_file) THEN
                EXIT get_points
            ELSE IF (TRIM(line) == '') THEN
                CYCLE     !! Skip blank lines
            ELSE
                CALL interpret_string (line=line, datatype=[ 'R','R','R' ], separator=' ', reals=reals)
                me%points(1:dim,i) = reals(1:dim)
            END IF
        END DO get_points

100     FORMAT((a))
        END PROCEDURE struct_grid_read

        MODULE PROCEDURE struct_grid_write
        !!
        !! Writes the structured grid dataset information to the .vtk file
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

        END PROCEDURE struct_grid_write

        MODULE PROCEDURE struct_grid_setup
        !!
        !! Sets up the structured grid dataset with information

        me%name       = 'STRUCTURED_GRID'
        me%dimensions = dims
        me%n_points   = SIZE(points,DIM=2)
        me%points     = points
        me%firstcall  = .FALSE.

        END PROCEDURE struct_grid_setup

        MODULE PROCEDURE check_for_diffs_struct_grid
        !!
        !! Function checks for differences in a structured grid dataset

        INTEGER(i4k) :: i, j

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

        END PROCEDURE check_for_diffs_struct_grid
! ****************
! Rectilinear Grid
! ****************
        MODULE PROCEDURE rectlnr_grid_read
        USE Misc, ONLY : interpret_string, def_len, char_dt, to_lowercase
        !!
        !! Reads the rectilinear grid dataset information from the .vtk file

        INTEGER(i4k)            :: i, j, iostat
        INTEGER(i4k), PARAMETER :: dim = 3
        LOGICAL                 :: end_of_File
        CHARACTER(LEN=def_len)  :: line
        INTEGER(i4k),      DIMENSION(:), ALLOCATABLE :: ints
        REAL(r8k),         DIMENSION(:), ALLOCATABLE :: reals
        TYPE(char_dt),     DIMENSION(:), ALLOCATABLE :: chars
        CHARACTER(LEN=13), DIMENSION(3), PARAMETER   :: descr_coord = &
          & [ 'X_COORDINATES', 'Y_COORDINATES', 'Z_COORDINATES' ]

        READ(unit,100,iostat=iostat) line
        CALL interpret_string (line=line, datatype=[ 'C' ],         ignore='DATASET ',     separator=' ', chars=chars)
        me%name = TRIM(chars(1)%text)
        IF (ALLOCATED(chars)) DEALLOCATE(chars)

        READ(unit,100,iostat=iostat) line
        CALL interpret_string (line=line, datatype=[ 'I','I','I' ], ignore='DIMENSIONS ',  separator=' ', ints=ints)
        me%dimensions = ints(1:3); ALLOCATE(me%x%coord(1:ints(1)), me%y%coord(1:ints(2)), me%z%coord(1:ints(3)))

        end_of_file = .FALSE.; i = 0

        get_coords: DO
            i = i + 1
            READ(unit,100,iostat=iostat) line
            CALL interpret_string (line=line, datatype=[ 'I','C' ], ignore=descr_coord(i), separator=' ', ints=ints, chars=chars)
            ASSOCIATE (my_datatype => to_lowercase(TRIM(chars(1)%text)))
                SELECT CASE (i)
                CASE (1)
                    me%x%datatype = my_datatype
                CASE (2)
                    me%y%datatype = my_datatype
                CASE (3)
                    me%z%datatype = my_datatype
                END SELECT
            END ASSOCIATE
            IF (ALLOCATED(chars)) DEALLOCATE(chars)
            READ(unit,100,iostat=iostat) line
            end_of_file = (iostat < 0)
            IF (end_of_file) THEN
                EXIT get_coords
            ELSE
                j = 0
                get_vals: DO
                    j = j + 1
                    CALL interpret_string (line=line, datatype=[ 'R' ], separator=' ', reals=reals)
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
        END PROCEDURE rectlnr_grid_read

        MODULE PROCEDURE rectlnr_grid_write
        !!
        !! Writes the rectilinear grid dataset information to the .vtk file

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

        END PROCEDURE rectlnr_grid_write

        MODULE PROCEDURE rectlnr_grid_setup
        !!
        !! Sets up the rectilinear grid dataset with information

        IF (dims(1) /= SIZE(x_coords) .OR. dims(2) /= SIZE(y_coords) .OR. dims(3) /= SIZE(z_coords)) THEN
            ERROR STOP 'Bad inputs for rectlnr_grid_setup. Dims is not equal to size of coords.'
        END IF

        me%name       = 'RECTILINEAR_GRID'
        me%dimensions = dims
        me%x%datatype = datatype
        me%y%datatype = datatype
        me%z%datatype = datatype
        me%x%coord    = x_coords
        me%y%coord    = y_coords
        me%z%coord    = z_coords
        me%firstcall  = .FALSE.

        END PROCEDURE rectlnr_grid_setup

        MODULE PROCEDURE check_for_diffs_rectlnr_grid
        !!
        !! Function checks for differences in a rectilinear grid dataset
        INTEGER(i4k) :: i

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

        END PROCEDURE check_for_diffs_rectlnr_grid
! **************
! Polygonal Data
! **************
        MODULE PROCEDURE polygonal_data_read
        USE Misc,      ONLY : interpret_string, def_len, char_dt, to_lowercase
        USE vtk_cells, ONLY : poly_vertex, poly_line, polygon, triangle_strip
        !!
        !! Reads the polygonal data dataset information from the .vtk file
        INTEGER(i4k)                       :: i, j, iostat, n, descr_size, n_points
        INTEGER(i4k), PARAMETER            :: dim = 3
        LOGICAL                            :: end_of_File
        CHARACTER(LEN=def_len)             :: line
        CHARACTER(LEN=:), ALLOCATABLE      :: descr
        INTEGER(i4k),  DIMENSION(:), ALLOCATABLE :: ints, dummy, points
        REAL(r8k),     DIMENSION(:), ALLOCATABLE :: reals
        TYPE(char_dt), DIMENSION(:), ALLOCATABLE :: chars

        READ(unit,100,iostat=iostat) line
        CALL interpret_string (line=line, datatype=[ 'C' ],         ignore='DATASET ',     separator=' ', chars=chars)
        me%name = TRIM(chars(1)%text)
        IF (ALLOCATED(chars)) DEALLOCATE(chars)

        READ(unit,100,iostat=iostat) line
        CALL interpret_string (line=line, datatype=[ 'I','C' ],     ignore='POINTS ',     separator=' ', ints=ints, chars=chars)
        me%n_points= ints(1); me%datatype = to_lowercase(TRIM(chars(1)%text))
        IF (ALLOCATED(chars)) DEALLOCATE(chars)

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
                CALL interpret_string (line=line, datatype=[ 'R','R','R' ], separator=' ', reals=reals)
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

            CALL interpret_string (line=line, datatype=[ 'C','I','I' ], separator=' ', ints=ints, chars=chars)
            descr = TRIM(chars(1)%text); n = ints(1); descr_size = ints(2)
            IF (ALLOCATED(chars)) DEALLOCATE(chars)

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
                        CALL interpret_string (line=line, datatype=[ 'I' ], separator=' ', ints=ints)
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
        END PROCEDURE polygonal_data_read

        MODULE PROCEDURE polygonal_data_write
        !!
        !! Writes the polygonal data dataset information to the .vtk file
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

        END PROCEDURE polygonal_data_write

        MODULE PROCEDURE polygonal_data_setup
        !!
        !! Sets up the polygonal data dataset with information

        me%name       = 'POLYDATA'
        me%n_points   = SIZE(points,DIM=2)
        me%points     = points
        me%firstcall  = .FALSE.
        IF (PRESENT(vertices )) ALLOCATE(me%vertices, source=vertices )
        IF (PRESENT(lines    )) ALLOCATE(me%lines,    source=lines    )
        IF (PRESENT(polygons )) ALLOCATE(me%polygons, source=polygons )
        IF (PRESENT(triangles)) ALLOCATE(me%triangles,source=triangles)

        END PROCEDURE polygonal_data_setup
! *****************
! Unstructured Grid
! *****************
        MODULE PROCEDURE unstruct_grid_read
        USE Misc,      ONLY : interpret_string, def_len, char_dt, to_lowercase
        USE vtk_cells, ONLY : vtkcell, poly_vertex, set_cell_type
        !!
        !! Reads the unstructured grid dataset information from the .vtk file
        CLASS(vtkcell), ALLOCATABLE :: dummy_cell
        INTEGER(i4k)                :: i, iostat
        INTEGER(i4k), PARAMETER     :: dim = 3
        LOGICAL                     :: end_of_File
        CHARACTER(LEN=def_len)      :: line
        INTEGER(i4k),  DIMENSION(:), ALLOCATABLE :: ints
        REAL(r8k),     DIMENSION(:), ALLOCATABLE :: reals
        TYPE(char_dt), DIMENSION(:), ALLOCATABLE :: chars
        TYPE temp_points
            INTEGER(i4k), DIMENSION(:), ALLOCATABLE  :: points
        END TYPE temp_points
        TYPE (temp_points), DIMENSION(:), ALLOCATABLE :: read_points

        READ(unit,100,iostat=iostat) line
        CALL interpret_string (line=line, datatype=[ 'C' ],         ignore='DATASET ',     separator=' ', chars=chars)
        me%name = TRIM(chars(1)%text)
        IF (ALLOCATED(chars)) DEALLOCATE(chars)

        READ(unit,100,iostat=iostat) line
        CALL interpret_string (line=line, datatype=[ 'I','C' ],     ignore='POINTS ',     separator=' ', ints=ints, chars=chars)
        me%n_points= ints(1); me%datatype = to_lowercase(TRIM(chars(1)%text))

        ALLOCATE(me%points(1:3,1:me%n_points)); end_of_file = .FALSE.

        get_points: DO i = 1, me%n_points
            READ(unit,100,iostat=iostat) line
            end_of_file = (iostat < 0)
            IF (end_of_file) THEN
                EXIT get_points
            ELSE IF (TRIM(line) == '') THEN
                CYCLE     !! Skip blank lines
            ELSE
                CALL interpret_string (line=line, datatype=[ 'R','R','R' ], separator=' ', reals=reals)
                me%points(1:dim,i) = reals(1:dim)
            END IF
        END DO get_points

        ! Get the cell information
        READ(unit,100,iostat=iostat) line
        CALL interpret_string (line=line, datatype=[ 'I','I' ], ignore='POINTS ', separator=' ', ints=ints)
        me%n_cells = ints(1); me%size = ints(2); ALLOCATE(read_points(1:ints(1)))
        ALLOCATE(me%cell_list(1:me%n_cells))

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
        CALL interpret_string (line=line, datatype=[ 'I' ], ignore='CELL_TYPES ', separator=' ', ints=ints)
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
                CALL interpret_string (line=line, datatype=[ 'I' ], separator=' ', ints=ints)
                dummy_cell = set_cell_type (ints(1))
                ALLOCATE(me%cell_list(i)%cell,mold=dummy_cell)
                CALL me%cell_list(i)%cell%setup (read_points(i)%points)
                DEALLOCATE(dummy_cell)
            END IF
        END DO get_cell_type

100     FORMAT((a))
        END PROCEDURE unstruct_grid_read

        MODULE PROCEDURE unstruct_grid_write
        !!
        !! Writes the unstructured grid dataset information from the .vtk file
        INTEGER(i4k) :: i

        WRITE(unit,100) me%name
        WRITE(unit,101) me%n_points, me%datatype

        DO i = 1, me%n_points
            WRITE(unit,102) me%points(1:3,i)
        END DO

        WRITE(unit,103) me%n_cells, me%size
        DO i = 1, me%n_cells
            CALL me%cell_list(i)%cell%write(unit)
        END DO

        WRITE(unit,104) me%n_cell_types
        DO i = 1, me%n_cell_types
            WRITE(unit,105) me%cell_list(i)%cell%type
        END DO

100     FORMAT ('DATASET ',(a))
101     FORMAT ('POINTS ',(i0),' ',(a))
102     FORMAT (*(es13.6))
103     FORMAT ('CELLS ',(i0),' ',(i0))
104     FORMAT ('CELL_TYPES ',(i0))
105     FORMAT (' ',i0)

        END PROCEDURE unstruct_grid_write

        MODULE PROCEDURE unstruct_grid_setup
        !!
        !! Sets up the unstructured grid dataset with information
        INTEGER(i4k) :: i, size_cnt

        me%name         = 'UNSTRUCTURED_GRID'
        me%n_points     = SIZE(points, DIM=2)
        me%n_cells      = SIZE(cells,  DIM=1)
        me%n_cell_types = SIZE(cells,  DIM=1)
        me%points       = points
        ALLOCATE(me%cell_list(1:me%n_cells))
        size_cnt = me%n_cells
        DO i = 1, me%n_cells
            ALLOCATE(me%cell_list(i)%cell,source=cells(i))
            size_cnt = size_cnt + me%cell_list(i)%cell%n_points
        END DO
        me%size         = size_cnt
        me%firstcall    = .FALSE.

        END PROCEDURE unstruct_grid_setup

        MODULE PROCEDURE unstruct_grid_setup_multiclass
        !!
        !! Sets up the unstructured grid dataset with information
        INTEGER(i4k) :: i, size_cnt

        me%name         = 'UNSTRUCTURED_GRID'
        me%n_points     = SIZE(points,    DIM=2)
        me%n_cells      = SIZE(cell_list, DIM=1)
        me%n_cell_types = SIZE(cell_list, DIM=1)
        me%points       = points
        me%cell_list    = cell_list
        size_cnt        = me%n_cells
        DO i = 1, me%n_cells
            size_cnt = size_cnt + me%cell_list(i)%cell%n_points
        END DO
        me%size         = size_cnt
        me%firstcall    = .FALSE.

        END PROCEDURE unstruct_grid_setup_multiclass

END SUBMODULE vtk_datasets_implementation
