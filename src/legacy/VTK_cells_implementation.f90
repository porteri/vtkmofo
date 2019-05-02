SUBMODULE (vtk_cells) vtk_cells_implementation
    !! author: Ian Porter
    !! date: 12/2/2017
    !!
    !! This module contains the types of cells used by VTK
    !!
    !! The following cells are available:
    !!  1) vertex
    !!  2) poly_vertex
    !!  3) line
    !!  4) poly_line
    !!  5) triangle
    !!  6) triangle_strip
    !!  7) polygon
    !!  8) pixel
    !!  9) quad
    !! 10) tetra
    !! 11) voxel
    !! 12) hexahedron
    !! 13) wedge
    !! 14) pyramid
    !! 15) quadratic_edge
    !! 16) quadratic_triangle
    !! 17) quadratic_quad
    !! 18) quadratic_tetra
    !! 19) quadratic_hexahedron
    !!
    CONTAINS

        MODULE PROCEDURE abs_read
        USE Misc, ONLY : interpret_string, def_len
        !!
        !! Subroutine performs the read for a cell
        INTEGER(i4k)                :: i, iostat
        LOGICAL                     :: end_of_file, ierr
        CHARACTER(LEN=def_len)      :: line
        INTEGER(i4k), DIMENSION(:), ALLOCATABLE :: ints, dummy, points

        ALLOCATE(me%points(0)); i = 0; end_of_file = .FALSE.

        READ(unit,100,iostat=iostat) line
        end_of_file = (iostat < 0)
        IF (end_of_file) THEN
            RETURN
        ELSE
            i = 0! IF(ALLOCATED(points)) DEALLOCATE(points)
            get_vals: DO
                i = i + 1
                CALL interpret_string (line=line, datatype=(/ 'I' /), separator=' ', ints=ints)
                IF (i == 1) THEN
                    CALL me%init(ints(1), ierr)
                ELSE
                    ALLOCATE(dummy(1:i-1))
                    dummy(i-1) = ints(1)
                    IF (i > 2) dummy(1:i-2) = points
                    IF (ALLOCATED(points)) DEALLOCATE(points)
                    CALL MOVE_ALLOC(dummy, points)
                END IF
                IF (line == '') EXIT get_vals
            END DO get_vals
            me%points = points
        END IF

100     FORMAT((a))
        END PROCEDURE abs_read

        MODULE PROCEDURE abs_write
        !!
        !! Writes the cell information to the .vtk file
        INTEGER(i4k)               :: i

        WRITE(unit,100) me%n_points, (me%points(i),i=1,me%n_points)
100     FORMAT ((i0,' '),*(i0,' '))
        END PROCEDURE abs_write

        MODULE PROCEDURE abs_setup
        !!
        !! Sets up the cell information
        LOGICAL :: ierr = .FALSE.

        CALL me%init(SIZE(points), ierr)     !! Initialize the cell
        IF (ierr) ERROR STOP 'Error initializing cell. Bad # of points.'
        me%points = points

        END PROCEDURE abs_setup

        MODULE PROCEDURE abs_init
        !!
        !! Initializes the cell with size and type information

        me%n_points = n
        ierr        = .FALSE.

        END PROCEDURE abs_init

        MODULE PROCEDURE check_for_diffs
        !! author: Ian Porter
        !! date: 01/05/2017
        !!
        !! Function checks for differences in an cell
        !!
        INTEGER(i4k) :: i

        diffs = .FALSE.
        IF       (.NOT. SAME_TYPE_AS(me,you))         THEN
            diffs = .TRUE.
        ELSE IF (me%n_points     /= you%n_points)     THEN
            diffs = .TRUE.
        ELSE IF (SIZE(me%points) /= SIZE(you%points)) THEN
            diffs = .TRUE.
        ELSE
            DO i = 1, SIZE(me%points)
                IF (me%points(i) /= you%points(i))    THEN
                    diffs = .TRUE.
                    EXIT
                END IF
            END DO
        END IF

        END PROCEDURE check_for_diffs

        MODULE PROCEDURE vertex_init
        !!
        !! Initializes a vertex cell

        me%n_points = 1
        me%type     = 1
        ierr        = (n /= me%n_points)

        END PROCEDURE vertex_init

        MODULE PROCEDURE poly_vertex_init
        !!
        !! Initializes a poly_vertex cell

        me%n_points = n
        me%type     = 2
        ierr        = .FALSE.

        END PROCEDURE poly_vertex_init

        MODULE PROCEDURE line_init
        !!
        !! Initializes a line cell

        me%n_points = 2
        me%type     = 3
        ierr        = (n /= me%n_points)

        END PROCEDURE line_init

        MODULE PROCEDURE poly_line_init
        !!
        !! Initializes a poly_line cell

        me%n_points = n
        me%type     = 4
        ierr        = .FALSE.

        END PROCEDURE poly_line_init

        MODULE PROCEDURE triangle_init
        !!
        !! Initializes a triangle cell

        me%n_points = 3
        me%type     = 5
        ierr        = (n /= me%n_points)

        END PROCEDURE triangle_init

        MODULE PROCEDURE triangle_strip_init
        !!
        !! Initializes a triangle_strip cell

        me%n_points = n
        me%type     = 6
        ierr        = .FALSE.

        END PROCEDURE triangle_strip_init

        MODULE PROCEDURE polygon_init
        !!
        !! Initializes a polygon cell

        me%n_points = n
        me%type     = 7
        ierr        = .FALSE.

        END PROCEDURE polygon_init

        MODULE PROCEDURE pixel_init
        !!
        !! Initializes a pixel cell

        me%n_points = 4
        me%type     = 8
        ierr        = (n /= me%n_points)

        END PROCEDURE pixel_init

        MODULE PROCEDURE quad_init
        !!
        !! Initializes a quad cell

        me%n_points = 4
        me%type     = 9
        ierr        = (n /= me%n_points)

        END PROCEDURE quad_init

        MODULE PROCEDURE tetra_init
        !!
        !! Initializes a tetra cell

        me%n_points = 4
        me%type     = 10
        ierr        = (n /= me%n_points)

        END PROCEDURE tetra_init

        MODULE PROCEDURE voxel_init
        !!
        !! Initializes a voxel cell

        me%n_points = 8
        me%type     = 11
        ierr        = (n /= me%n_points)

        END PROCEDURE voxel_init

        MODULE PROCEDURE hexahedron_init
        !!
        !! Initializes a hexahedron cell

        me%n_points = 8
        me%type     = 12
        ierr        = (n /= me%n_points)

        END PROCEDURE hexahedron_init

        MODULE PROCEDURE wedge_init
        !!
        !! Initializes a wedge cell

        me%n_points = 6
        me%type     = 13
        ierr        = (n /= me%n_points)

        END PROCEDURE wedge_init

        MODULE PROCEDURE pyramid_init
        !!
        !! Initializes a pyramid cell

        me%n_points = 5
        me%type     = 14
        ierr        = (n /= me%n_points)

        END PROCEDURE pyramid_init

        MODULE PROCEDURE quadratic_edge_init
        !!
        !! Initializes a quadratic_edge cell

        me%n_points = 3
        me%type     = 21
        ierr        = (n /= me%n_points)

        END PROCEDURE quadratic_edge_init

        MODULE PROCEDURE quadratic_triangle_init
        !!
        !! Initializes a quadratic_triangle cell

        me%n_points = 6
        me%type     = 22
        ierr        = (n /= me%n_points)

        END PROCEDURE quadratic_triangle_init

        MODULE PROCEDURE quadratic_quad_init
        !!
        !! Initializes a quadratic_quad cell

        me%n_points = 8
        me%type     = 23
        ierr        = (n /= me%n_points)

        END PROCEDURE quadratic_quad_init

        MODULE PROCEDURE quadratic_tetra_init
        !!
        !! Initializes a quadratic_tetra cell

        me%n_points = 10
        me%type     = 24
        ierr        = (n /= me%n_points)

        END PROCEDURE quadratic_tetra_init

        MODULE PROCEDURE quadratic_hexahedron_init
        !!
        !! Initializes a quadratic_hexahedron cell

        me%n_points = 20
        me%type     = 25
        ierr        = (n /= me%n_points)

        END PROCEDURE quadratic_hexahedron_init

        MODULE PROCEDURE set_cell_type
        !!
        !! Subroutine allocates the cell based on the type (called during a read)

        IF (ALLOCATED(me)) DEALLOCATE(me)

        SELECT CASE (type)
        CASE (1)
            ALLOCATE(vertex::me)
        CASE (2)
            ALLOCATE(poly_vertex::me)
        CASE (3)
            ALLOCATE(line::me)
        CASE (4)
            ALLOCATE(poly_line::me)
        CASE (5)
            ALLOCATE(triangle::me)
        CASE (6)
            ALLOCATE(triangle_strip::me)
        CASE (7)
            ALLOCATE(polygon::me)
        CASE (8)
            ALLOCATE(pixel::me)
        CASE (9)
            ALLOCATE(quad::me)
        CASE (10)
            ALLOCATE(tetra::me)
        CASE (11)
            ALLOCATE(voxel::me)
        CASE (12)
            ALLOCATE(hexahedron::me)
        CASE (13)
            ALLOCATE(wedge::me)
        CASE (14)
            ALLOCATE(pyramid::me)
        CASE (21)
            ALLOCATE(quadratic_edge::me)
        CASE (22)
            ALLOCATE(quadratic_triangle::me)
        CASE (23)
            ALLOCATE(quadratic_quad::me)
        CASE (24)
            ALLOCATE(quadratic_tetra::me)
        CASE (25)
            ALLOCATE(quadratic_hexahedron::me)
        CASE DEFAULT
            ERROR STOP 'Bad value for type. type is unidentified. Execution terminated in Subroutine: set_cell_type'
        END SELECT

        END PROCEDURE set_cell_type

END SUBMODULE vtk_cells_implementation
