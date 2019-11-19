module vtk_cells_unit_tests
    use precision, only : i4k
    implicit none
    !! author: Ian Porter
    !! date: 12/16/2017
    !!
    !! unit testing for cells
    !!
    private
    public :: vtk_cells_unit
    ! generic information
    integer(i4k), parameter :: n_types = 19
    integer(i4k), parameter :: vtk_unit = 20
    integer(i4k), parameter :: n = 11
    integer(i4k),      dimension(n_types), parameter :: type       = &
        & [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 21, 22, 23, 24, 25 ]
    integer(i4k),      dimension(n_types), parameter :: n_points   = &
        & [ 1, 7, 2, n, 3, n, n, 4, 4,  4,  8,  8,  6,  5,  3,  6,  8, 10, 20 ]
    integer(i4k),      dimension(*),       parameter :: point_vals = &
        & [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20 ]
    character(len=25), dimension(n_types), parameter :: filename   = &
        & [ 'vertex.vtk               ', &
        &   'poly_vertex.vtk          ', &
        &   'line.vtk                 ', &
        &   'poly_line.vtk            ', &
        &   'triangle.vtk             ', &
        &   'triangle_strip.vtk       ', &
        &   'polygon.vtk              ', &
        &   'pixel.vtk                ', &
        &   'quad.vtk                 ', &
        &   'tetra.vtk                ', &
        &   'voxel.vtk                ', &
        &   'hexahedron.vtk           ', &
        &   'wedge.vtk                ', &
        &   'pyramid.vtk              ', &
        &   'quadratic_edge.vtk       ', &
        &   'quadratic_triangle.vtk   ', &
        &   'quadratic_quad.vtk       ', &
        &   'quadratic_tetra.vtk      ', &
        &   'quadratic_hexahedron.vtk ' ]

contains

    subroutine vtk_cells_unit (test_pass)
        use vtk_cells, only : vtkcell, vertex, poly_vertex, line, poly_line, triangle, triangle_strip, polygon, pixel, quad, &
            &                 tetra, voxel, hexahedron, wedge, pyramid, quadratic_edge, quadratic_triangle, quadratic_quad,  &
            &                 quadratic_tetra, quadratic_hexahedron
        implicit none
        !!
        !! loops over each cell type, performs a write, then performs a read on a different cell
        !! and compares the two to make sure they are identical
        class(vtkcell), allocatable :: vtk_cell_1, vtk_cell_2
        integer(i4k)                :: i
        logical, intent(out)        :: test_pass
        logical, dimension(n_types) :: individual_tests_pass

        do i = 1, n_types
            if (allocated(vtk_cell_1)) deallocate(vtk_cell_1)
            if (allocated(vtk_cell_2)) deallocate(vtk_cell_2)
            select case (type(i))
            case (1)
                !! vertex
                allocate(vertex::vtk_cell_1, vtk_cell_2)
            case (2)
                !! poly_vertex
                allocate(poly_vertex::vtk_cell_1, vtk_cell_2)
            case (3)
                !! line
                allocate(line::vtk_cell_1, vtk_cell_2)
            case (4)
                !! poly_line
                allocate(poly_line::vtk_cell_1, vtk_cell_2)
            case (5)
                !! triangle
                allocate(triangle::vtk_cell_1, vtk_cell_2)
            case (6)
                !! triangle_strip
                allocate(triangle_strip::vtk_cell_1, vtk_cell_2)
            case (7)
                !! polygon
                allocate(polygon::vtk_cell_1, vtk_cell_2)
            case (8)
                !! pixel
                allocate(pixel::vtk_cell_1, vtk_cell_2)
            case (9)
                !! quad
                allocate(quad::vtk_cell_1, vtk_cell_2)
            case (10)
                !! tetra
                allocate(tetra::vtk_cell_1, vtk_cell_2)
            case (11)
                !! vine
                allocate(voxel::vtk_cell_1, vtk_cell_2)
            case (12)
                !! hexahedron
                allocate(hexahedron::vtk_cell_1, vtk_cell_2)
            case (13)
                !! wedge
                allocate(wedge::vtk_cell_1, vtk_cell_2)
            case (14)
                !! pyramid
                allocate(pyramid::vtk_cell_1, vtk_cell_2)
            case (21)
                !! quadratic_edge
                allocate(quadratic_edge::vtk_cell_1, vtk_cell_2)
            case (22)
                !! quadratic_triangle
                allocate(quadratic_triangle::vtk_cell_1, vtk_cell_2)
            case (23)
                !! quadratic_quad
                allocate(quadratic_quad::vtk_cell_1, vtk_cell_2)
            case (24)
                !! quadratic_tetra
                allocate(quadratic_tetra::vtk_cell_1, vtk_cell_2)
            case (25)
                !! quadratic_hexahedron
                allocate(quadratic_hexahedron::vtk_cell_1, vtk_cell_2)
            end select

            !! data type is generated from the defined values above
            call vtk_cell_1%setup(points=point_vals(1:n_points(i)))
            open (unit=vtk_unit, file=filename(i), form='formatted')
            call vtk_cell_1%write(vtk_unit)
            close(unit=vtk_unit)

            !! data type is generated from the read
            open (unit=vtk_unit, file=filename(i), status='old', form='formatted')
            call vtk_cell_2%read(vtk_unit)
            close(unit=vtk_unit)

            !! compare the read file and the written/read file to ensure both types are the same
            individual_tests_pass(i) = .not. (vtk_cell_1 .diff. vtk_cell_2)
        end do

        !! compare the read file and the written/read file to ensure both types are the same
        test_pass = all(individual_tests_pass)

    end subroutine vtk_cells_unit

end module vtk_cells_unit_tests

program vtk_cells_test
    use vtk_cells_unit_tests, only : vtk_cells_unit
    use vtkmofopassfail,      only : all_tests_pass
    implicit none
    !! author: Ian Porter
    !! date: 12/15/2017
    !!
    !! driver testing subroutine for the cells information
    !!
    logical :: test_passes = .false.

    call vtk_cells_unit (test_passes)

    if (test_passes) call all_tests_pass()

end program vtk_cells_test
