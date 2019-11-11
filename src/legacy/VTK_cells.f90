MODULE vtk_cells
    USE Precision, ONLY : i4k
    IMPLICIT NONE
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
    PRIVATE
    PUBLIC :: vtkcell, vtkcell_list
    PUBLIC :: vertex, poly_vertex, line, poly_line, triangle, triangle_strip, polygon, pixel, quad, tetra, voxel
    PUBLIC :: hexahedron, wedge, pyramid, quadratic_edge, quadratic_triangle, quadratic_quad, quadratic_tetra
    PUBLIC :: quadratic_hexahedron, set_cell_type

    TYPE, ABSTRACT :: vtkcell
        !! Abstract DT for cell information
        INTEGER(i4k) :: n_points = 0
        INTEGER(i4k) :: type     = 0
        INTEGER(i4k), DIMENSION(:), ALLOCATABLE :: points
    CONTAINS
        PROCEDURE, PUBLIC :: read  => abs_read
        PROCEDURE, PUBLIC :: write => cell_legacy_write
        PROCEDURE, PUBLIC :: setup => abs_setup
        PROCEDURE(abs_init), DEFERRED, PRIVATE :: init
        PROCEDURE, PRIVATE :: check_for_diffs
        GENERIC :: OPERATOR(.diff.) => check_for_diffs
    END TYPE vtkcell

    TYPE vtkcell_list
        !! Workaround to allow for different cell classes in a list of cells
        CLASS(vtkcell), ALLOCATABLE :: cell
    END TYPE vtkcell_list

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

    INTERFACE

        MODULE SUBROUTINE abs_read (me, unit)
        IMPLICIT NONE
        !! Subroutine performs the read for a cell
        CLASS(vtkcell), INTENT(OUT) :: me
        INTEGER(i4k),   INTENT(IN)  :: unit

        END SUBROUTINE abs_read

        MODULE SUBROUTINE cell_legacy_write (me, unit)
        IMPLICIT NONE
        !! Writes the cell information to the .vtk file
        CLASS(vtkcell), INTENT(IN) :: me
        INTEGER(i4k),   INTENT(IN) :: unit

        END SUBROUTINE cell_legacy_write

        MODULE SUBROUTINE abs_setup (me, points)
        IMPLICIT NONE
        !! Sets up the cell information
        CLASS(vtkcell), INTENT(OUT) :: me
        INTEGER(i4k), DIMENSION(:), INTENT(IN) :: points

        END SUBROUTINE abs_setup

        MODULE SUBROUTINE abs_init (me, n, ierr)
        IMPLICIT NONE
        !! Initializes the cell with size and type information
        CLASS(vtkcell), INTENT(OUT) :: me
        INTEGER(i4k),   INTENT(IN)  :: n
        LOGICAL,        INTENT(OUT) :: ierr

        END SUBROUTINE abs_init

        MODULE FUNCTION check_for_diffs (me, you) RESULT (diffs)
        IMPLICIT NONE
        !! Function checks for differences in an cell
        CLASS(vtkcell), INTENT(IN) :: me, you
        LOGICAL      :: diffs

        END FUNCTION check_for_diffs

        MODULE SUBROUTINE vertex_init (me, n, ierr)
        IMPLICIT NONE
        !! Initializes a vertex cell
        CLASS(vertex), INTENT(OUT) :: me
        INTEGER(i4k),  INTENT(IN)  :: n
        LOGICAL,       INTENT(OUT) :: ierr

        END SUBROUTINE vertex_init

        MODULE SUBROUTINE poly_vertex_init (me, n, ierr)
        IMPLICIT NONE
        !! Initializes a poly_vertex cell
        CLASS(poly_vertex), INTENT(OUT) :: me
        INTEGER(i4k),       INTENT(IN)  :: n
        LOGICAL,            INTENT(OUT) :: ierr

        END SUBROUTINE poly_vertex_init

        MODULE SUBROUTINE line_init (me, n, ierr)
        IMPLICIT NONE
        !! Initializes a line cell
        CLASS(line),  INTENT(OUT) :: me
        INTEGER(i4k), INTENT(IN)  :: n
        LOGICAL,      INTENT(OUT) :: ierr

        END SUBROUTINE line_init

        MODULE SUBROUTINE poly_line_init (me, n, ierr)
        IMPLICIT NONE
        !! Initializes a poly_line cell
        CLASS(poly_line), INTENT(OUT) :: me
        INTEGER(i4k),     INTENT(IN)  :: n
        LOGICAL,          INTENT(OUT) :: ierr

        END SUBROUTINE poly_line_init

        MODULE SUBROUTINE triangle_init (me, n, ierr)
        IMPLICIT NONE
        !! Initializes a triangle cell
        CLASS(triangle), INTENT(OUT) :: me
        INTEGER(i4k),    INTENT(IN)  :: n
        LOGICAL,         INTENT(OUT) :: ierr

        END SUBROUTINE triangle_init

        MODULE SUBROUTINE triangle_strip_init (me, n, ierr)
        IMPLICIT NONE
        !! Initializes a triangle_strip cell
        CLASS(triangle_strip), INTENT(OUT) :: me
        INTEGER(i4k),          INTENT(IN)  :: n
        LOGICAL,               INTENT(OUT) :: ierr

        END SUBROUTINE triangle_strip_init


        MODULE SUBROUTINE polygon_init (me, n, ierr)
        IMPLICIT NONE
        !! Initializes a polygon cell
        CLASS(polygon), INTENT(OUT) :: me
        INTEGER(i4k),   INTENT(IN)  :: n
        LOGICAL,        INTENT(OUT) :: ierr

        END SUBROUTINE polygon_init

        MODULE SUBROUTINE pixel_init (me, n, ierr)
        IMPLICIT NONE
        !! Initializes a pixel cell
        CLASS(pixel),   INTENT(OUT) :: me
        INTEGER(i4k),   INTENT(IN)  :: n
        LOGICAL,        INTENT(OUT) :: ierr

        END SUBROUTINE pixel_init

        MODULE SUBROUTINE quad_init (me, n, ierr)
        IMPLICIT NONE
        !! Initializes a quad cell
        CLASS(quad),    INTENT(OUT) :: me
        INTEGER(i4k),   INTENT(IN)  :: n
        LOGICAL,        INTENT(OUT) :: ierr

        END SUBROUTINE quad_init

        MODULE SUBROUTINE tetra_init (me, n, ierr)
        IMPLICIT NONE
        !! Initializes a tetra cell
        CLASS(tetra),   INTENT(OUT) :: me
        INTEGER(i4k),   INTENT(IN)  :: n
        LOGICAL,        INTENT(OUT) :: ierr

        END SUBROUTINE tetra_init

        MODULE SUBROUTINE voxel_init (me, n, ierr)
        IMPLICIT NONE
        !! Initializes a voxel cell
        CLASS(voxel),   INTENT(OUT) :: me
        INTEGER(i4k),   INTENT(IN)  :: n
        LOGICAL,        INTENT(OUT) :: ierr

        END SUBROUTINE voxel_init

        MODULE SUBROUTINE hexahedron_init (me, n, ierr)
        IMPLICIT NONE
        !! Initializes a hexahedron cell
        CLASS(hexahedron), INTENT(OUT) :: me
        INTEGER(i4k),      INTENT(IN)  :: n
        LOGICAL,           INTENT(OUT) :: ierr

        END SUBROUTINE hexahedron_init

        MODULE SUBROUTINE wedge_init (me, n, ierr)
        IMPLICIT NONE
        !! Initializes a wedge cell
        CLASS(wedge),   INTENT(OUT) :: me
        INTEGER(i4k),   INTENT(IN)  :: n
        LOGICAL,        INTENT(OUT) :: ierr

        END SUBROUTINE wedge_init

        MODULE SUBROUTINE pyramid_init (me, n, ierr)
        IMPLICIT NONE
        !! Initializes a pyramid cell
        CLASS(pyramid), INTENT(OUT) :: me
        INTEGER(i4k),   INTENT(IN)  :: n
        LOGICAL,        INTENT(OUT) :: ierr

        END SUBROUTINE pyramid_init

        MODULE SUBROUTINE quadratic_edge_init (me, n, ierr)
        IMPLICIT NONE
        !! Initializes a quadratic_edge cell
        CLASS(quadratic_edge), INTENT(OUT) :: me
        INTEGER(i4k),          INTENT(IN)  :: n
        LOGICAL,               INTENT(OUT) :: ierr

        END SUBROUTINE quadratic_edge_init

        MODULE SUBROUTINE quadratic_triangle_init (me, n, ierr)
        IMPLICIT NONE
        !! Initializes a quadratic_triangle cell
        CLASS(quadratic_triangle), INTENT(OUT) :: me
        INTEGER(i4k),              INTENT(IN)  :: n
        LOGICAL,                   INTENT(OUT) :: ierr

        END SUBROUTINE quadratic_triangle_init

        MODULE SUBROUTINE quadratic_quad_init (me, n, ierr)
        IMPLICIT NONE
        !! Initializes a quadratic_quad cell
        CLASS(quadratic_quad), INTENT(OUT) :: me
        INTEGER(i4k),          INTENT(IN)  :: n
        LOGICAL,               INTENT(OUT) :: ierr

        END SUBROUTINE quadratic_quad_init

        MODULE SUBROUTINE quadratic_tetra_init (me, n, ierr)
        IMPLICIT NONE
        !! Initializes a quadratic_tetra cell
        CLASS(quadratic_tetra), INTENT(OUT) :: me
        INTEGER(i4k),           INTENT(IN)  :: n
        LOGICAL,                INTENT(OUT) :: ierr

        END SUBROUTINE quadratic_tetra_init

        MODULE SUBROUTINE quadratic_hexahedron_init (me, n, ierr)
        IMPLICIT NONE
        !! Initializes a quadratic_hexahedron cell
        CLASS(quadratic_hexahedron), INTENT(OUT) :: me
        INTEGER(i4k),                INTENT(IN)  :: n
        LOGICAL,                     INTENT(OUT) :: ierr

        END SUBROUTINE quadratic_hexahedron_init

        MODULE FUNCTION set_cell_type (type) RESULT(me)
        IMPLICIT NONE
        !! Subroutine allocates the cell based on the type (called during a read)
        INTEGER(i4k),   INTENT(IN)  :: type  !! Cell type ID
        CLASS(vtkcell), ALLOCATABLE :: me    !! DT

        END FUNCTION set_cell_type

    END INTERFACE

END MODULE vtk_cells
