module vtk_cells
    use precision, only : i4k
    implicit none
    !! author: Ian Porter
    !! date: 12/2/2017
    !!
    !! this module contains the types of cells used by vtk
    !!
    !! the following cells are available:
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
    private
    public :: vtkcell, vtkcell_list
    public :: vertex, poly_vertex, line, poly_line, triangle, triangle_strip, polygon, pixel, quad, tetra, voxel
    public :: hexahedron, wedge, pyramid, quadratic_edge, quadratic_triangle, quadratic_quad, quadratic_tetra
    public :: quadratic_hexahedron, set_cell_type

    type, abstract :: vtkcell
        !! abstract dt for cell information
        integer(i4k) :: n_points = 0
        integer(i4k) :: type     = 0
        integer(i4k), dimension(:), allocatable :: points
    contains
        procedure, public :: read  => abs_read
        procedure, public :: write => cell_legacy_write
        procedure, public :: setup => abs_setup
        procedure(abs_init), deferred, private :: init
        procedure, private :: check_for_diffs
        generic :: operator(.diff.) => check_for_diffs
    end type vtkcell

    type vtkcell_list
        !! workaround to allow for different cell classes in a list of cells
        class(vtkcell), allocatable :: cell
    end type vtkcell_list

    type, extends(vtkcell) :: vertex
    contains
        procedure :: init => vertex_init
    end type vertex

    type, extends(vtkcell) :: poly_vertex
    contains
        procedure :: init => poly_vertex_init
    end type poly_vertex

    type, extends(vtkcell) :: line
    contains
        procedure :: init => line_init
    end type line

    type, extends(vtkcell) :: poly_line
    contains
        procedure :: init => poly_line_init
    end type poly_line

    type, extends(vtkcell) :: triangle
    contains
        procedure :: init => triangle_init
    end type triangle

    type, extends(vtkcell) :: triangle_strip
    contains
        procedure :: init => triangle_strip_init
    end type triangle_strip

    type, extends(vtkcell) :: polygon
    contains
        procedure :: init => polygon_init
    end type polygon

    type, extends(vtkcell) :: pixel
    contains
        procedure :: init => pixel_init
    end type pixel

    type, extends(vtkcell) :: quad
    contains
        procedure :: init => quad_init
    end type quad

    type, extends(vtkcell) :: tetra
    contains
        procedure :: init => tetra_init
    end type tetra

    type, extends(vtkcell) :: voxel
    contains
        procedure :: init => voxel_init
    end type voxel

    type, extends(vtkcell) :: hexahedron
    contains
        procedure :: init => hexahedron_init
    end type hexahedron

    type, extends(vtkcell) :: wedge
    contains
        procedure :: init => wedge_init
    end type wedge

    type, extends(vtkcell) :: pyramid
    contains
        procedure :: init => pyramid_init
    end type pyramid

    type, extends(vtkcell) :: quadratic_edge
    contains
        procedure :: init => quadratic_edge_init
    end type quadratic_edge

    type, extends(vtkcell) :: quadratic_triangle
    contains
        procedure :: init => quadratic_triangle_init
    end type quadratic_triangle

    type, extends(vtkcell) :: quadratic_quad
    contains
        procedure :: init => quadratic_quad_init
    end type quadratic_quad

    type, extends(vtkcell) :: quadratic_tetra
    contains
        procedure :: init => quadratic_tetra_init
    end type quadratic_tetra

    type, extends(vtkcell) :: quadratic_hexahedron
    contains
        procedure :: init => quadratic_hexahedron_init
    end type quadratic_hexahedron

    interface

        module subroutine abs_read (me, unit)
            implicit none
            !! subroutine performs the read for a cell
            class(vtkcell), intent(out) :: me
            integer(i4k),   intent(in)  :: unit

        end subroutine abs_read

        module subroutine cell_legacy_write (me, unit)
            implicit none
            !! writes the cell information to the .vtk file
            class(vtkcell), intent(in) :: me
            integer(i4k),   intent(in) :: unit

        end subroutine cell_legacy_write

        module subroutine abs_setup (me, points)
            implicit none
            !! sets up the cell information
            class(vtkcell), intent(out) :: me
            integer(i4k), dimension(:), intent(in) :: points

        end subroutine abs_setup

        module subroutine abs_init (me, n, ierr)
            implicit none
            !! initializes the cell with size and type information
            class(vtkcell), intent(out) :: me
            integer(i4k),   intent(in)  :: n
            logical,        intent(out) :: ierr

        end subroutine abs_init

        module function check_for_diffs (me, you) result (diffs)
            implicit none
            !! function checks for differences in an cell
            class(vtkcell), intent(in) :: me
            class(vtkcell), intent(in) :: you
            logical      :: diffs

        end function check_for_diffs

        module subroutine vertex_init (me, n, ierr)
            implicit none
            !! initializes a vertex cell
            class(vertex), intent(out) :: me
            integer(i4k),  intent(in)  :: n
            logical,       intent(out) :: ierr

        end subroutine vertex_init

        module subroutine poly_vertex_init (me, n, ierr)
            implicit none
            !! initializes a poly_vertex cell
            class(poly_vertex), intent(out) :: me
            integer(i4k),       intent(in)  :: n
            logical,            intent(out) :: ierr

        end subroutine poly_vertex_init

        module subroutine line_init (me, n, ierr)
            implicit none
            !! initializes a line cell
            class(line),  intent(out) :: me
            integer(i4k), intent(in)  :: n
            logical,      intent(out) :: ierr

        end subroutine line_init

        module subroutine poly_line_init (me, n, ierr)
            implicit none
            !! initializes a poly_line cell
            class(poly_line), intent(out) :: me
            integer(i4k),     intent(in)  :: n
            logical,          intent(out) :: ierr

        end subroutine poly_line_init

        module subroutine triangle_init (me, n, ierr)
            implicit none
            !! initializes a triangle cell
            class(triangle), intent(out) :: me
            integer(i4k),    intent(in)  :: n
            logical,         intent(out) :: ierr

        end subroutine triangle_init

        module subroutine triangle_strip_init (me, n, ierr)
            implicit none
            !! initializes a triangle_strip cell
            class(triangle_strip), intent(out) :: me
            integer(i4k),          intent(in)  :: n
            logical,               intent(out) :: ierr

        end subroutine triangle_strip_init


        module subroutine polygon_init (me, n, ierr)
            implicit none
            !! initializes a polygon cell
            class(polygon), intent(out) :: me
            integer(i4k),   intent(in)  :: n
            logical,        intent(out) :: ierr

        end subroutine polygon_init

        module subroutine pixel_init (me, n, ierr)
            implicit none
            !! initializes a pixel cell
            class(pixel),   intent(out) :: me
            integer(i4k),   intent(in)  :: n
            logical,        intent(out) :: ierr

        end subroutine pixel_init

        module subroutine quad_init (me, n, ierr)
            implicit none
            !! initializes a quad cell
            class(quad),    intent(out) :: me
            integer(i4k),   intent(in)  :: n
            logical,        intent(out) :: ierr

        end subroutine quad_init

        module subroutine tetra_init (me, n, ierr)
            implicit none
            !! initializes a tetra cell
            class(tetra),   intent(out) :: me
            integer(i4k),   intent(in)  :: n
            logical,        intent(out) :: ierr

        end subroutine tetra_init

        module subroutine voxel_init (me, n, ierr)
            implicit none
            !! initializes a voxel cell
            class(voxel),   intent(out) :: me
            integer(i4k),   intent(in)  :: n
            logical,        intent(out) :: ierr

        end subroutine voxel_init

        module subroutine hexahedron_init (me, n, ierr)
            implicit none
            !! initializes a hexahedron cell
            class(hexahedron), intent(out) :: me
            integer(i4k),      intent(in)  :: n
            logical,           intent(out) :: ierr

        end subroutine hexahedron_init

        module subroutine wedge_init (me, n, ierr)
            implicit none
            !! initializes a wedge cell
            class(wedge),   intent(out) :: me
            integer(i4k),   intent(in)  :: n
            logical,        intent(out) :: ierr

        end subroutine wedge_init

        module subroutine pyramid_init (me, n, ierr)
            implicit none
            !! initializes a pyramid cell
            class(pyramid), intent(out) :: me
            integer(i4k),   intent(in)  :: n
            logical,        intent(out) :: ierr

        end subroutine pyramid_init

        module subroutine quadratic_edge_init (me, n, ierr)
            implicit none
            !! initializes a quadratic_edge cell
            class(quadratic_edge), intent(out) :: me
            integer(i4k),          intent(in)  :: n
            logical,               intent(out) :: ierr

        end subroutine quadratic_edge_init

        module subroutine quadratic_triangle_init (me, n, ierr)
            implicit none
            !! initializes a quadratic_triangle cell
            class(quadratic_triangle), intent(out) :: me
            integer(i4k),              intent(in)  :: n
            logical,                   intent(out) :: ierr

        end subroutine quadratic_triangle_init

        module subroutine quadratic_quad_init (me, n, ierr)
            implicit none
            !! initializes a quadratic_quad cell
            class(quadratic_quad), intent(out) :: me
            integer(i4k),          intent(in)  :: n
            logical,               intent(out) :: ierr

        end subroutine quadratic_quad_init

        module subroutine quadratic_tetra_init (me, n, ierr)
            implicit none
            !! initializes a quadratic_tetra cell
            class(quadratic_tetra), intent(out) :: me
            integer(i4k),           intent(in)  :: n
            logical,                intent(out) :: ierr

        end subroutine quadratic_tetra_init

        module subroutine quadratic_hexahedron_init (me, n, ierr)
            implicit none
            !! initializes a quadratic_hexahedron cell
            class(quadratic_hexahedron), intent(out) :: me
            integer(i4k),                intent(in)  :: n
            logical,                     intent(out) :: ierr

        end subroutine quadratic_hexahedron_init

        module function set_cell_type (type) result(me)
            implicit none
            !! subroutine allocates the cell based on the type (called during a read)
            integer(i4k),   intent(in)  :: type  !! cell type id
            class(vtkcell), allocatable :: me    !! dt

        end function set_cell_type

    end interface

end module vtk_cells
