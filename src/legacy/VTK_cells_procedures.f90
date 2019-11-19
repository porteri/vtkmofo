submodule (vtk_cells) vtk_cells_procedures
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
contains

    module procedure abs_read
        use misc, only : interpret_string, def_len
        implicit none
        !! subroutine performs the read for a cell
        integer(i4k)           :: i, iostat
        logical                :: end_of_file, ierr
        character(len=def_len) :: line
        integer(i4k), dimension(:), allocatable :: ints, dummy, points

        allocate(me%points(0)); i = 0; end_of_file = .false.

        read(unit,100,iostat=iostat) line
        end_of_file = (iostat < 0)
        if (end_of_file) then
            return
        else
            i = 0 ! if(allocated(points)) deallocate(points)
            get_vals: do
                i = i + 1
                call interpret_string (line=line, datatype=[ 'i' ], separator=' ', ints=ints)
                if (i == 1) then
                    call me%init(ints(1), ierr)
                else
                    allocate(dummy(1:i-1))
                    dummy(i-1) = ints(1)
                    if (i > 2) dummy(1:i-2) = points
                    if (allocated(points)) deallocate(points)
                    call move_alloc(dummy, points)
                end if
                if (line == '') exit get_vals
            end do get_vals
            me%points = points
        end if

100     format((a))
    end procedure abs_read

    module procedure cell_legacy_write
        implicit none
        !! writes the cell information to the .vtk file
        integer(i4k) :: i

        write(unit,100) me%n_points, (me%points(i),i=1,me%n_points)
100     format((i0,' '),*(i0,' '))
    end procedure cell_legacy_write

    module procedure abs_setup
        implicit none
        !! sets up the cell information
        logical :: ierr = .false.

        call me%init(size(points), ierr)     !! initialize the cell
        if (ierr) error stop 'error initializing cell. bad # of points.'
        me%points = points

    end procedure abs_setup

    module procedure abs_init
        implicit none
        !! initializes the cell with size and type information

        me%n_points = n
        ierr        = .false.

    end procedure abs_init

    module procedure check_for_diffs
        implicit none
        !! author: Ian Porter
        !! date: 01/05/2017
        !!
        !! function checks for differences in an cell
        !!
        integer(i4k) :: i

        diffs = .false.
        if       (.not. same_type_as(me,you))         then
            diffs = .true.
        else if (me%n_points     /= you%n_points)     then
            diffs = .true.
        else if (size(me%points) /= size(you%points)) then
            diffs = .true.
        else
            do i = 1, size(me%points)
                if (me%points(i) /= you%points(i))    then
                    diffs = .true.
                    exit
                end if
            end do
        end if

    end procedure check_for_diffs

    module procedure vertex_init
        implicit none
        !! initializes a vertex cell

        me%n_points = 1
        me%type     = 1
        ierr        = (n /= me%n_points)

    end procedure vertex_init

    module procedure poly_vertex_init
        implicit none
        !! initializes a poly_vertex cell

        me%n_points = n
        me%type     = 2
        ierr        = .false.

    end procedure poly_vertex_init

    module procedure line_init
        implicit none
        !! initializes a line cell

        me%n_points = 2
        me%type     = 3
        ierr        = (n /= me%n_points)

    end procedure line_init

    module procedure poly_line_init
        implicit none
        !! initializes a poly_line cell

        me%n_points = n
        me%type     = 4
        ierr        = .false.

    end procedure poly_line_init

    module procedure triangle_init
        implicit none
        !! initializes a triangle cell

        me%n_points = 3
        me%type     = 5
        ierr        = (n /= me%n_points)

    end procedure triangle_init

    module procedure triangle_strip_init
        implicit none
        !! initializes a triangle_strip cell

        me%n_points = n
        me%type     = 6
        ierr        = .false.

    end procedure triangle_strip_init

    module procedure polygon_init
        implicit none
        !! initializes a polygon cell

        me%n_points = n
        me%type     = 7
        ierr        = .false.

    end procedure polygon_init

    module procedure pixel_init
        implicit none
        !! initializes a pixel cell

        me%n_points = 4
        me%type     = 8
        ierr        = (n /= me%n_points)

    end procedure pixel_init

    module procedure quad_init
        implicit none
        !! initializes a quad cell

        me%n_points = 4
        me%type     = 9
        ierr        = (n /= me%n_points)

    end procedure quad_init

    module procedure tetra_init
        implicit none
        !! initializes a tetra cell

        me%n_points = 4
        me%type     = 10
        ierr        = (n /= me%n_points)

    end procedure tetra_init

    module procedure voxel_init
        implicit none
        !! initializes a voxel cell

        me%n_points = 8
        me%type     = 11
        ierr        = (n /= me%n_points)

    end procedure voxel_init

    module procedure hexahedron_init
        implicit none
        !! initializes a hexahedron cell

        me%n_points = 8
        me%type     = 12
        ierr        = (n /= me%n_points)

    end procedure hexahedron_init

    module procedure wedge_init
        implicit none
        !! initializes a wedge cell

        me%n_points = 6
        me%type     = 13
        ierr        = (n /= me%n_points)

    end procedure wedge_init

    module procedure pyramid_init
        implicit none
        !! initializes a pyramid cell

        me%n_points = 5
        me%type     = 14
        ierr        = (n /= me%n_points)

    end procedure pyramid_init

    module procedure quadratic_edge_init
        implicit none
        !! initializes a quadratic_edge cell

        me%n_points = 3
        me%type     = 21
        ierr        = (n /= me%n_points)

    end procedure quadratic_edge_init

    module procedure quadratic_triangle_init
        implicit none
        !! initializes a quadratic_triangle cell

        me%n_points = 6
        me%type     = 22
        ierr        = (n /= me%n_points)

    end procedure quadratic_triangle_init

    module procedure quadratic_quad_init
        implicit none
        !! initializes a quadratic_quad cell

        me%n_points = 8
        me%type     = 23
        ierr        = (n /= me%n_points)

    end procedure quadratic_quad_init

    module procedure quadratic_tetra_init
        implicit none
        !! initializes a quadratic_tetra cell

        me%n_points = 10
        me%type     = 24
        ierr        = (n /= me%n_points)

    end procedure quadratic_tetra_init

    module procedure quadratic_hexahedron_init
        implicit none
        !! initializes a quadratic_hexahedron cell

        me%n_points = 20
        me%type     = 25
        ierr        = (n /= me%n_points)

    end procedure quadratic_hexahedron_init

    module procedure set_cell_type
        implicit none
        !! subroutine allocates the cell based on the type (called during a read)

        if (allocated(me)) deallocate(me)

        select case (type)
        case (1)
            allocate(vertex::me)
        case (2)
            allocate(poly_vertex::me)
        case (3)
            allocate(line::me)
        case (4)
            allocate(poly_line::me)
        case (5)
            allocate(triangle::me)
        case (6)
            allocate(triangle_strip::me)
        case (7)
            allocate(polygon::me)
        case (8)
            allocate(pixel::me)
        case (9)
            allocate(quad::me)
        case (10)
            allocate(tetra::me)
        case (11)
            allocate(voxel::me)
        case (12)
            allocate(hexahedron::me)
        case (13)
            allocate(wedge::me)
        case (14)
            allocate(pyramid::me)
        case (21)
            allocate(quadratic_edge::me)
        case (22)
            allocate(quadratic_triangle::me)
        case (23)
            allocate(quadratic_quad::me)
        case (24)
            allocate(quadratic_tetra::me)
        case (25)
            allocate(quadratic_hexahedron::me)
        case default
            error stop 'bad value for type. type is unidentified. execution terminated in subroutine: set_cell_type'
        end select

    end procedure set_cell_type

end submodule vtk_cells_procedures
