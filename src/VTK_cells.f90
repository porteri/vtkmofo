MODULE vtk_cells
    USE Kinds
    IMPLICIT NONE
    !>@brief
    !> THis module contains the types of cells used by VTK
    !>@author
    !> Ian Porter
    !>@date
    !> 12/2/2017
    !
    ! The following cells are available:
    !  1) vertex
    !  2) poly_vertex
    !  3) line
    !  4) poly_line
    !  5) triangle
    !  6) triangle_strip
    !  7) polygon
    !  8) pixel
    !  9) quad
    ! 10) tetra
    ! 11) voxel
    ! 12) hexahedron
    ! 13) wedge
    ! 14) pyramid
    ! 15) quadratic_edge
    ! 16) quadratic_triangle
    ! 17) quadratic_quad
    ! 18) quadratic_tetra
    ! 19) quadratic_hexahedron
    !

    PRIVATE
    PUBLIC :: vtkcell, vertex, poly_vertex, line, poly_line, triangle, triangle_strip, polygon, pixel, quad, tetra, voxel
    PUBLIC :: hexahedron, wedge, pyramid, quadratic_edge, quadratic_triangle, quadratic_quad, quadratic_tetra, quadratic_hexahedron

    TYPE, ABSTRACT :: vtkcell
        INTEGER(i4k) :: n_points
        INTEGER(i4k) :: type
        INTEGER(i4k), DIMENSION(:), ALLOCATABLE :: points
    CONTAINS
        PROCEDURE(abs_init), DEFERRED, PRIVATE :: init
        PROCEDURE, PUBLIC :: setup => abs_setup
        PROCEDURE :: define => abs_define
        PROCEDURE, PUBLIC :: read   => abs_read
        PROCEDURE, PUBLIC :: write  => abs_write
    END TYPE vtkcell

    TYPE, EXTENDS(vtkcell) :: vertex
    CONTAINS
        PROCEDURE :: init => vertex_init
    END TYPE vertex

    TYPE, EXTENDS(vtkcell) :: poly_vertex
    CONTAINS
        PROCEDURE :: init => poly_vertex_init
    END TYPE poly_vertex

    TYPE, EXTENDS(vtkcell) :: line
    CONTAINS
        PROCEDURE :: init => line_init
    END TYPE line

    TYPE, EXTENDS(vtkcell) :: poly_line
    CONTAINS
        PROCEDURE :: init => poly_line_init
    END TYPE poly_line

    TYPE, EXTENDS(vtkcell) :: triangle
    CONTAINS
        PROCEDURE :: init => triangle_init
    END TYPE triangle

    TYPE, EXTENDS(vtkcell) :: triangle_strip
    CONTAINS
        PROCEDURE :: init => triangle_strip_init
    END TYPE triangle_strip

    TYPE, EXTENDS(vtkcell) :: polygon
    CONTAINS
        PROCEDURE :: init => polygon_init
    END TYPE polygon

    TYPE, EXTENDS(vtkcell) :: pixel
    CONTAINS
        PROCEDURE :: init => pixel_init
    END TYPE pixel

    TYPE, EXTENDS(vtkcell) :: quad
    CONTAINS
        PROCEDURE :: init => quad_init
    END TYPE quad

    TYPE, EXTENDS(vtkcell) :: tetra
    CONTAINS
        PROCEDURE :: init => tetra_init
    END TYPE tetra

    TYPE, EXTENDS(vtkcell) :: voxel
    CONTAINS
        PROCEDURE :: init => voxel_init
    END TYPE voxel

    TYPE, EXTENDS(vtkcell) :: hexahedron
    CONTAINS
        PROCEDURE :: init => hexahedron_init
    END TYPE hexahedron

    TYPE, EXTENDS(vtkcell) :: wedge
    CONTAINS
        PROCEDURE :: init => wedge_init
    END TYPE wedge

    TYPE, EXTENDS(vtkcell) :: pyramid
    CONTAINS
        PROCEDURE :: init => pyramid_init
    END TYPE pyramid

    TYPE, EXTENDS(vtkcell) :: quadratic_edge
    CONTAINS
        PROCEDURE :: init => quadratic_edge_init
    END TYPE quadratic_edge

    TYPE, EXTENDS(vtkcell) :: quadratic_triangle
    CONTAINS
        PROCEDURE :: init => quadratic_triangle_init
    END TYPE quadratic_triangle

    TYPE, EXTENDS(vtkcell) :: quadratic_quad
    CONTAINS
        PROCEDURE :: init => quadratic_quad_init
    END TYPE quadratic_quad

    TYPE, EXTENDS(vtkcell) :: quadratic_tetra
    CONTAINS
        PROCEDURE :: init => quadratic_tetra_init
    END TYPE quadratic_tetra

    TYPE, EXTENDS(vtkcell) :: quadratic_hexahedron
    CONTAINS
        PROCEDURE :: init => quadratic_hexahedron_init
    END TYPE quadratic_hexahedron

    CONTAINS
        PURE SUBROUTINE abs_init (me, n)
        CLASS(vtkcell), INTENT(OUT) :: me
        INTEGER(i4k),   INTENT(IN) :: n
        me%n_points = n
        END SUBROUTINE abs_init

        SUBROUTINE abs_setup (me, points)
        !>@brief
        !> Sets up the cell information
        CLASS(vtkcell), INTENT(OUT) :: me
        INTEGER(i4k), DIMENSION(:), INTENT(IN) :: points

        CALL me%init(SIZE(points))     !! Initialize the cell

        me%points = points

        END SUBROUTINE abs_setup

        SUBROUTINE abs_define (me, points)
        CLASS(vtkcell), INTENT(OUT) :: me
        INTEGER(i4k), DIMENSION(:), INTENT(IN) :: points

        me%points = points

        END SUBROUTINE abs_define

        SUBROUTINE abs_read (me, unit)
        USE Misc, ONLY : interpret_string, def_len
        !>@brief
        !> Subroutine performs the read for a cell
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 12/15/2017
        CLASS(vtkcell), INTENT(INOUT) :: me
        INTEGER(i4k),   INTENT(IN)    :: unit
        INTEGER(i4k)                  :: i, iostat
        LOGICAL                       :: end_of_file
        CHARACTER(LEN=def_len)        :: line
        INTEGER(i4k), DIMENSION(:), ALLOCATABLE :: ints, dummy

        ALLOCATE(me%points(0)); i = -1; end_of_file = .FALSE.

        READ(unit,100,iostat=iostat) line
        end_of_file = (iostat < 0)
        IF (end_of_file) THEN
            RETURN
        ELSE
            get_vals: DO
                ALLOCATE(dummy(1:UBOUND(me%points,DIM=1)+1),source=0)
                IF (i > 0) dummy(1:UBOUND(me%points,DIM=1)) = me%points
                CALL MOVE_ALLOC(dummy, me%points)

                i = i + 1
                CALL interpret_string (line=line, datatype=(/ 'I' /), separator=' ', ints=ints)
                IF (i == 0) THEN
                    CALL me%init(ints(1))
                    IF (ALLOCATED(me%points)) DEALLOCATE(me%points)
                    ALLOCATE(me%points(0))
                ELSE
                    me%points(i) = ints(1)
                END IF
                IF (line == '') EXIT
            END DO get_vals
        END IF

100     FORMAT((a))
        END SUBROUTINE abs_read

        SUBROUTINE abs_write (me, unit)
        CLASS(vtkcell), INTENT(IN) :: me
        INTEGER(i4k),   INTENT(IN) :: unit
        INTEGER(i4k) :: i

        WRITE(unit,100) me%n_points, (me%points(i),i=1,me%n_points)
100     FORMAT ((i0,' '),*(i0,' '))
        END SUBROUTINE abs_write

        PURE SUBROUTINE vertex_init (me, n)
        !>@brief
        !> Initializes a vertex cell
        CLASS(vertex), INTENT(OUT) :: me
        INTEGER(i4k),  INTENT(IN)  :: n

        me%n_points = 1
        me%type     = 1
        IF (n /= me%n_points) THEN
            ERROR STOP 'Incorrect number of points provide to initialize vertex'
        END IF

        END SUBROUTINE vertex_init

        PURE SUBROUTINE poly_vertex_init (me, n)
        !>@brief
        !> Initializes a poly_vertex cell
        CLASS(poly_vertex), INTENT(OUT) :: me
        INTEGER(i4k),       INTENT(IN)  :: n

        me%n_points = 7 !! May be n instead?
        me%type     = 2
        IF (n /= me%n_points) THEN
            ERROR STOP 'Incorrect number of points provide to initialize poly_vertex'
        END IF

        END SUBROUTINE poly_vertex_init

        PURE SUBROUTINE line_init (me, n)
        !>@brief
        !> Initializes a line cell
        CLASS(line),  INTENT(OUT) :: me
        INTEGER(i4k), INTENT(IN)  :: n

        me%n_points = 2
        me%type     = 3
        IF (n /= me%n_points) THEN
            ERROR STOP 'Incorrect number of points provide to initialize line'
        END IF

        END SUBROUTINE line_init

        PURE SUBROUTINE poly_line_init (me, n)
        !>@brief
        !> Initializes a poly_line cell
        CLASS(poly_line), INTENT(OUT) :: me
        INTEGER(i4k),     INTENT(IN)  :: n

        me%n_points = n
        me%type     = 4

        END SUBROUTINE poly_line_init

        PURE SUBROUTINE triangle_init (me, n)
        !>@brief
        !> Initializes a triangle cell
        CLASS(triangle), INTENT(OUT) :: me
        INTEGER(i4k),    INTENT(IN)  :: n

        me%n_points = 3
        me%type     = 5
        IF (n /= me%n_points) THEN
            ERROR STOP 'Incorrect number of points provide to initialize triangle'
        END IF

        END SUBROUTINE triangle_init

        PURE SUBROUTINE triangle_strip_init (me, n)
        !>@brief
        !> Initializes a triangle_strip cell
        CLASS(triangle_strip), INTENT(OUT) :: me
        INTEGER(i4k),          INTENT(IN)  :: n

        me%n_points = n
        me%type     = 6

        END SUBROUTINE triangle_strip_init


        PURE SUBROUTINE polygon_init (me, n)
        !>@brief
        !> Initializes a polygon cell
        CLASS(polygon), INTENT(OUT) :: me
        INTEGER(i4k),   INTENT(IN)  :: n

        me%n_points = n
        me%type     = 7

        END SUBROUTINE polygon_init

        PURE SUBROUTINE pixel_init (me, n)
        !>@brief
        !> Initializes a pixel cell
        CLASS(pixel), INTENT(OUT) :: me
        INTEGER(i4k), INTENT(IN)  :: n

        me%n_points = 4
        me%type     = 8
        IF (n /= me%n_points) THEN
            ERROR STOP 'Incorrect number of points provide to initialize pixel'
        END IF

        END SUBROUTINE pixel_init

        PURE SUBROUTINE quad_init (me, n)
        !>@brief
        !> Initializes a quad cell
        CLASS(quad),  INTENT(OUT) :: me
        INTEGER(i4k), INTENT(IN)  :: n

        me%n_points = 4
        me%type     = 9
        IF (n /= me%n_points) THEN
            ERROR STOP 'Incorrect number of points provide to initialize quad'
        END IF

        END SUBROUTINE quad_init

        PURE SUBROUTINE tetra_init (me, n)
        !>@brief
        !> Initializes a tetra cell
        CLASS(tetra), INTENT(OUT) :: me
        INTEGER(i4k), INTENT(IN)  :: n

        me%n_points = 4
        me%type     = 10
        IF (n /= me%n_points) THEN
            ERROR STOP 'Incorrect number of points provide to initialize tetra'
        END IF

        END SUBROUTINE tetra_init

        PURE SUBROUTINE voxel_init (me, n)
        !>@brief
        !> Initializes a voxel cell
        CLASS(voxel), INTENT(OUT) :: me
        INTEGER(i4k), INTENT(IN)  :: n

        me%n_points = 8
        me%type     = 11
        IF (n /= me%n_points) THEN
            ERROR STOP 'Incorrect number of points provide to initialize voxel'
        END IF

        END SUBROUTINE voxel_init

        PURE SUBROUTINE hexahedron_init (me, n)
        !>@brief
        !> Initializes a hexahedron cell
        CLASS(hexahedron), INTENT(OUT) :: me
        INTEGER(i4k),    INTENT(IN)  :: n

        me%n_points = 8
        me%type     = 12
        IF (n /= me%n_points) THEN
            ERROR STOP 'Incorrect number of points provide to initialize hexahedron'
        END IF

        END SUBROUTINE hexahedron_init

        PURE SUBROUTINE wedge_init (me, n)
        !>@brief
        !> Initializes a wedge cell
        CLASS(wedge), INTENT(OUT) :: me
        INTEGER(i4k), INTENT(IN)  :: n

        me%n_points = 6
        me%type     = 13
        IF (n /= me%n_points) THEN
            ERROR STOP 'Incorrect number of points provide to initialize wedge'
        END IF

        END SUBROUTINE wedge_init

        PURE SUBROUTINE pyramid_init (me, n)
        !>@brief
        !> Initializes a pyramid cell
        CLASS(pyramid), INTENT(OUT) :: me
        INTEGER(i4k),   INTENT(IN)  :: n

        me%n_points = 5
        me%type     = 14
        IF (n /= me%n_points) THEN
            ERROR STOP 'Incorrect number of points provide to initialize pyramid'
        END IF

        END SUBROUTINE pyramid_init

        PURE SUBROUTINE quadratic_edge_init (me, n)
        !>@brief
        !> Initializes a quadratic_edge cell
        CLASS(quadratic_edge), INTENT(OUT) :: me
        INTEGER(i4k),          INTENT(IN)  :: n

        me%n_points = 3
        me%type     = 21
        IF (n /= me%n_points) THEN
            ERROR STOP 'Incorrect number of points provide to initialize quadratic_edge'
        END IF

        END SUBROUTINE quadratic_edge_init

        PURE SUBROUTINE quadratic_triangle_init (me, n)
        !>@brief
        !> Initializes a quadratic_triangle cell
        CLASS(quadratic_triangle), INTENT(OUT) :: me
        INTEGER(i4k),              INTENT(IN)  :: n

        me%n_points = 6
        me%type     = 22
        IF (n /= me%n_points) THEN
            ERROR STOP 'Incorrect number of points provide to initialize quadratic_triangle'
        END IF

        END SUBROUTINE quadratic_triangle_init

        PURE SUBROUTINE quadratic_quad_init (me, n)
        !>@brief
        !> Initializes a quadratic_quad cell
        CLASS(quadratic_quad), INTENT(OUT) :: me
        INTEGER(i4k),          INTENT(IN)  :: n

        me%n_points = 8
        me%type     = 23
        IF (n /= me%n_points) THEN
            ERROR STOP 'Incorrect number of points provide to initialize quadratic_quad'
        END IF

        END SUBROUTINE quadratic_quad_init

        PURE SUBROUTINE quadratic_tetra_init (me, n)
        !>@brief
        !> Initializes a quadratic_tetra cell
        CLASS(quadratic_tetra), INTENT(OUT) :: me
        INTEGER(i4k),           INTENT(IN)  :: n

        me%n_points = 10
        me%type     = 24
        IF (n /= me%n_points) THEN
            ERROR STOP 'Incorrect number of points provide to initialize quadratic_tetra'
        END IF

        END SUBROUTINE quadratic_tetra_init

        PURE SUBROUTINE quadratic_hexahedron_init (me, n)
        !>@brief
        !> Initializes a quadratic_hexahedron cell
        CLASS(quadratic_hexahedron), INTENT(OUT) :: me
        INTEGER(i4k),                INTENT(IN)  :: n

        me%n_points = 20
        me%type     = 25
        IF (n /= me%n_points) THEN
            ERROR STOP 'Incorrect number of points provide to initialize quadratic_hexahedron'
        END IF

        END SUBROUTINE quadratic_hexahedron_init
END MODULE vtk_cells
