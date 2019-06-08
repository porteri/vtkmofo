MODULE vtk_cells_unit_tests
    USE Precision, ONLY : i4k
    IMPLICIT NONE
    !! author: Ian Porter
    !! date: 12/16/2017
    !!
    !! Unit testing for cells
    !!
    PRIVATE
    PUBLIC :: vtk_cells_unit
! Generic information
    INTEGER(i4k), PARAMETER :: n_types = 19
    INTEGER(i4k), PARAMETER :: vtk_unit = 20
    INTEGER(i4k), PARAMETER :: n = 11
    INTEGER(i4k),      DIMENSION(n_types), PARAMETER :: type       = &
        & [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 21, 22, 23, 24, 25 ]
    INTEGER(i4k),      DIMENSION(n_types), PARAMETER :: n_points   = &
        & [ 1, 7, 2, n, 3, n, n, 4, 4,  4,  8,  8,  6,  5,  3,  6,  8, 10, 20 ]
    INTEGER(i4k),      DIMENSION(*),       PARAMETER :: point_vals = &
        & [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20 ]
    CHARACTER(LEN=25), DIMENSION(n_types), PARAMETER :: filename   = &
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

    CONTAINS

        SUBROUTINE vtk_cells_unit (test_pass)
        USE vtk_cells, ONLY : vtkcell, vertex, poly_vertex, line, poly_line, triangle, triangle_strip, polygon, pixel, quad, &
          &                   tetra, voxel, hexahedron, wedge, pyramid, quadratic_edge, quadratic_triangle, quadratic_quad,  &
          &                   quadratic_tetra, quadratic_hexahedron
        IMPLICIT NONE
        !!
        !! Loops over each cell type, performs a write, then performs a read on a different cell
        !! and compares the two to make sure they are identical
        CLASS(vtkcell), ALLOCATABLE :: vtk_cell_1, vtk_cell_2
        INTEGER(i4k)                :: i
        LOGICAL, INTENT(OUT)        :: test_pass
        LOGICAL, DIMENSION(n_types) :: individual_tests_pass

        DO i = 1, n_types
            IF (ALLOCATED(vtk_cell_1)) DEALLOCATE(vtk_cell_1)
            IF (ALLOCATED(vtk_cell_2)) DEALLOCATE(vtk_cell_2)
            SELECT CASE (type(i))
            CASE (1)
                !! Vertex
                ALLOCATE(vertex::vtk_cell_1, vtk_cell_2)
            CASE (2)
                !! Poly_vertex
                ALLOCATE(poly_vertex::vtk_cell_1, vtk_cell_2)
            CASE (3)
                !! Line
                ALLOCATE(line::vtk_cell_1, vtk_cell_2)
            CASE (4)
                !! Poly_line
                ALLOCATE(poly_line::vtk_cell_1, vtk_cell_2)
            CASE (5)
                !! Triangle
                ALLOCATE(triangle::vtk_cell_1, vtk_cell_2)
            CASE (6)
                !! Triangle_strip
                ALLOCATE(triangle_strip::vtk_cell_1, vtk_cell_2)
            CASE (7)
                !! Polygon
                ALLOCATE(polygon::vtk_cell_1, vtk_cell_2)
            CASE (8)
                !! Pixel
                ALLOCATE(pixel::vtk_cell_1, vtk_cell_2)
            CASE (9)
                !! Quad
                ALLOCATE(quad::vtk_cell_1, vtk_cell_2)
            CASE (10)
                !! Tetra
                ALLOCATE(tetra::vtk_cell_1, vtk_cell_2)
            CASE (11)
                !! Vine
                ALLOCATE(voxel::vtk_cell_1, vtk_cell_2)
            CASE (12)
                !! Hexahedron
                ALLOCATE(hexahedron::vtk_cell_1, vtk_cell_2)
            CASE (13)
                !! Wedge
                ALLOCATE(wedge::vtk_cell_1, vtk_cell_2)
            CASE (14)
                !! pyramid
                ALLOCATE(pyramid::vtk_cell_1, vtk_cell_2)
            CASE (21)
                !! Quadratic_edge
                ALLOCATE(quadratic_edge::vtk_cell_1, vtk_cell_2)
            CASE (22)
                !! Quadratic_triangle
                ALLOCATE(quadratic_triangle::vtk_cell_1, vtk_cell_2)
            CASE (23)
                !! Quadratic_quad
                ALLOCATE(quadratic_quad::vtk_cell_1, vtk_cell_2)
            CASE (24)
                !! Quadratic_tetra
                ALLOCATE(quadratic_tetra::vtk_cell_1, vtk_cell_2)
            CASE (25)
                !! Quadratic_hexahedron
                ALLOCATE(quadratic_hexahedron::vtk_cell_1, vtk_cell_2)
            END SELECT

            !! Data type is generated from the defined values above
            CALL vtk_cell_1%setup(points=point_vals(1:n_points(i)))
            OPEN (unit=vtk_unit, file=filename(i), form='formatted')
            CALL vtk_cell_1%write(vtk_unit)
            CLOSE(unit=vtk_unit)

            !! Data type is generated from the read
            OPEN (unit=vtk_unit, file=filename(i), status='old', form='formatted')
            CALL vtk_cell_2%read(vtk_unit)
            CLOSE(unit=vtk_unit)

            !! Compare the read file and the written/read file to ensure both types are the same
            individual_tests_pass(i) = .NOT. (vtk_cell_1 .diff. vtk_cell_2)
        END DO

        !! Compare the read file and the written/read file to ensure both types are the same
        test_pass = ALL(individual_tests_pass)

        END SUBROUTINE vtk_cells_unit
END MODULE vtk_cells_unit_tests

PROGRAM vtk_cells_test
    USE vtk_cells_unit_tests, ONLY : vtk_cells_unit
    USE VTKmofoPassFail,      ONLY : all_tests_pass
    IMPLICIT NONE
    !! author: Ian Porter
    !! date: 12/15/2017
    !!
    !! Driver testing subroutine for the cells information
    !!
    LOGICAL :: test_passes = .FALSE.

    CALL vtk_cells_unit (test_passes)

    IF (test_passes) CALL all_tests_pass()

END PROGRAM vtk_cells_test
