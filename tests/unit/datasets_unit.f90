module vtk_datasets_unit_tests
    use precision, only : i4k, r8k
    use vtk_vars,  only : ascii, binary
    use vtk_cells, only : polygon, voxel
    implicit none
    !! author: Ian Porter
    !! date: 12/17/2017
    !!
    !! unit testing for datasets
    !!
    private
    public :: vtk_datasets_unit
    ! generic information
    integer(i4k), parameter :: n_types  = 5
    integer(i4k), parameter :: vtk_unit = 20
    integer(i4k), parameter :: n_x = 11, n_y = 6, n_z = 3
    character(len=*), dimension(n_types), parameter :: filename = &
        & [ 'struct_pts.vtk      ', &
        &   'struct_grid.vtk     ', &
        &   'rectlnr_grid.vtk    ', &
        &   'polygonal_data.vtk  ', &
        &   'unstruct_grid.vtk   ' ]
    ! structured points
    integer(i4k), dimension(3), parameter :: dims    = &
        & [ n_x, n_y, n_z ]
    real(r8k), dimension(3), parameter    :: origin  = &
        & [ 0.0_r8k, 0.0_r8k, 0.0_r8k ]
    real(r8k), dimension(3), parameter    :: spacing = &
        & [ 0.1_r8k, 0.2_r8k, 0.5_r8k ]
    ! structured grid
    real(r8k), dimension(3,n_x*n_y*n_z), parameter :: points = reshape ( & !! [x, y, z]
        & [ 0.0_r8k, 0.0_r8k, 0.0_r8k, &
        &   0.0_r8k, 0.0_r8k, 0.5_r8k, &
        &   0.0_r8k, 0.0_r8k, 1.0_r8k, &
        &   0.0_r8k, 0.2_r8k, 0.0_r8k, &
        &   0.0_r8k, 0.2_r8k, 0.5_r8k, &
        &   0.0_r8k, 0.2_r8k, 1.0_r8k, &
        &   0.0_r8k, 0.4_r8k, 0.0_r8k, &
        &   0.0_r8k, 0.4_r8k, 0.5_r8k, &
        &   0.0_r8k, 0.4_r8k, 1.0_r8k, &
        &   0.0_r8k, 0.6_r8k, 0.0_r8k, &
        &   0.0_r8k, 0.6_r8k, 0.5_r8k, &
        &   0.0_r8k, 0.6_r8k, 1.0_r8k, &
        &   0.0_r8k, 0.8_r8k, 0.0_r8k, &
        &   0.0_r8k, 0.8_r8k, 0.5_r8k, &
        &   0.0_r8k, 0.8_r8k, 1.0_r8k, &
        &   0.0_r8k, 1.0_r8k, 0.0_r8k, &
        &   0.0_r8k, 1.0_r8k, 0.5_r8k, &
        &   0.0_r8k, 1.0_r8k, 1.0_r8k, &
        &   0.1_r8k, 0.0_r8k, 0.0_r8k, &
        &   0.1_r8k, 0.0_r8k, 0.5_r8k, &
        &   0.1_r8k, 0.0_r8k, 1.0_r8k, &
        &   0.1_r8k, 0.2_r8k, 0.0_r8k, &
        &   0.1_r8k, 0.2_r8k, 0.5_r8k, &
        &   0.1_r8k, 0.2_r8k, 1.0_r8k, &
        &   0.1_r8k, 0.4_r8k, 0.0_r8k, &
        &   0.1_r8k, 0.4_r8k, 0.5_r8k, &
        &   0.1_r8k, 0.4_r8k, 1.0_r8k, &
        &   0.1_r8k, 0.6_r8k, 0.0_r8k, &
        &   0.1_r8k, 0.6_r8k, 0.5_r8k, &
        &   0.1_r8k, 0.6_r8k, 1.0_r8k, &
        &   0.1_r8k, 0.8_r8k, 0.0_r8k, &
        &   0.1_r8k, 0.8_r8k, 0.5_r8k, &
        &   0.1_r8k, 0.8_r8k, 1.0_r8k, &
        &   0.1_r8k, 1.0_r8k, 0.0_r8k, &
        &   0.1_r8k, 1.0_r8k, 0.5_r8k, &
        &   0.1_r8k, 1.0_r8k, 1.0_r8k, &
        &   0.2_r8k, 0.0_r8k, 0.0_r8k, &
        &   0.2_r8k, 0.0_r8k, 0.5_r8k, &
        &   0.2_r8k, 0.0_r8k, 1.0_r8k, &
        &   0.2_r8k, 0.2_r8k, 0.0_r8k, &
        &   0.2_r8k, 0.2_r8k, 0.5_r8k, &
        &   0.2_r8k, 0.2_r8k, 1.0_r8k, &
        &   0.2_r8k, 0.4_r8k, 0.0_r8k, &
        &   0.2_r8k, 0.4_r8k, 0.5_r8k, &
        &   0.2_r8k, 0.4_r8k, 1.0_r8k, &
        &   0.2_r8k, 0.6_r8k, 0.0_r8k, &
        &   0.2_r8k, 0.6_r8k, 0.5_r8k, &
        &   0.2_r8k, 0.6_r8k, 1.0_r8k, &
        &   0.2_r8k, 0.8_r8k, 0.0_r8k, &
        &   0.2_r8k, 0.8_r8k, 0.5_r8k, &
        &   0.2_r8k, 0.8_r8k, 1.0_r8k, &
        &   0.2_r8k, 1.0_r8k, 0.0_r8k, &
        &   0.2_r8k, 1.0_r8k, 0.5_r8k, &
        &   0.2_r8k, 1.0_r8k, 1.0_r8k, &
        &   0.3_r8k, 0.0_r8k, 0.0_r8k, &
        &   0.3_r8k, 0.0_r8k, 0.5_r8k, &
        &   0.3_r8k, 0.0_r8k, 1.0_r8k, &
        &   0.3_r8k, 0.2_r8k, 0.0_r8k, &
        &   0.3_r8k, 0.2_r8k, 0.5_r8k, &
        &   0.3_r8k, 0.2_r8k, 1.0_r8k, &
        &   0.3_r8k, 0.4_r8k, 0.0_r8k, &
        &   0.3_r8k, 0.4_r8k, 0.5_r8k, &
        &   0.3_r8k, 0.4_r8k, 1.0_r8k, &
        &   0.3_r8k, 0.6_r8k, 0.0_r8k, &
        &   0.3_r8k, 0.6_r8k, 0.5_r8k, &
        &   0.3_r8k, 0.6_r8k, 1.0_r8k, &
        &   0.3_r8k, 0.8_r8k, 0.0_r8k, &
        &   0.3_r8k, 0.8_r8k, 0.5_r8k, &
        &   0.3_r8k, 0.8_r8k, 1.0_r8k, &
        &   0.3_r8k, 1.0_r8k, 0.0_r8k, &
        &   0.3_r8k, 1.0_r8k, 0.5_r8k, &
        &   0.3_r8k, 1.0_r8k, 1.0_r8k, &
        &   0.4_r8k, 0.0_r8k, 0.0_r8k, &
        &   0.4_r8k, 0.0_r8k, 0.5_r8k, &
        &   0.4_r8k, 0.0_r8k, 1.0_r8k, &
        &   0.4_r8k, 0.2_r8k, 0.0_r8k, &
        &   0.4_r8k, 0.2_r8k, 0.5_r8k, &
        &   0.4_r8k, 0.2_r8k, 1.0_r8k, &
        &   0.4_r8k, 0.4_r8k, 0.0_r8k, &
        &   0.4_r8k, 0.4_r8k, 0.5_r8k, &
        &   0.4_r8k, 0.4_r8k, 1.0_r8k, &
        &   0.4_r8k, 0.6_r8k, 0.0_r8k, &
        &   0.4_r8k, 0.6_r8k, 0.5_r8k, &
        &   0.4_r8k, 0.6_r8k, 1.0_r8k, &
        &   0.4_r8k, 0.8_r8k, 0.0_r8k, &
        &   0.4_r8k, 0.8_r8k, 0.5_r8k, &
        &   0.4_r8k, 0.8_r8k, 1.0_r8k, &
        &   0.4_r8k, 1.0_r8k, 0.0_r8k, &
        &   0.4_r8k, 1.0_r8k, 0.5_r8k, &
        &   0.4_r8k, 1.0_r8k, 1.0_r8k, &
        &   0.5_r8k, 0.0_r8k, 0.0_r8k, &
        &   0.5_r8k, 0.0_r8k, 0.5_r8k, &
        &   0.5_r8k, 0.0_r8k, 1.0_r8k, &
        &   0.5_r8k, 0.2_r8k, 0.0_r8k, &
        &   0.5_r8k, 0.2_r8k, 0.5_r8k, &
        &   0.5_r8k, 0.2_r8k, 1.0_r8k, &
        &   0.5_r8k, 0.4_r8k, 0.0_r8k, &
        &   0.5_r8k, 0.4_r8k, 0.5_r8k, &
        &   0.5_r8k, 0.4_r8k, 1.0_r8k, &
        &   0.5_r8k, 0.6_r8k, 0.0_r8k, &
        &   0.5_r8k, 0.6_r8k, 0.5_r8k, &
        &   0.5_r8k, 0.6_r8k, 1.0_r8k, &
        &   0.5_r8k, 0.8_r8k, 0.0_r8k, &
        &   0.5_r8k, 0.8_r8k, 0.5_r8k, &
        &   0.5_r8k, 0.8_r8k, 1.0_r8k, &
        &   0.5_r8k, 1.0_r8k, 0.0_r8k, &
        &   0.5_r8k, 1.0_r8k, 0.5_r8k, &
        &   0.5_r8k, 1.0_r8k, 1.0_r8k, &
        &   0.6_r8k, 0.0_r8k, 0.0_r8k, &
        &   0.6_r8k, 0.0_r8k, 0.5_r8k, &
        &   0.6_r8k, 0.0_r8k, 1.0_r8k, &
        &   0.6_r8k, 0.2_r8k, 0.0_r8k, &
        &   0.6_r8k, 0.2_r8k, 0.5_r8k, &
        &   0.6_r8k, 0.2_r8k, 1.0_r8k, &
        &   0.6_r8k, 0.4_r8k, 0.0_r8k, &
        &   0.6_r8k, 0.4_r8k, 0.5_r8k, &
        &   0.6_r8k, 0.4_r8k, 1.0_r8k, &
        &   0.6_r8k, 0.6_r8k, 0.0_r8k, &
        &   0.6_r8k, 0.6_r8k, 0.5_r8k, &
        &   0.6_r8k, 0.6_r8k, 1.0_r8k, &
        &   0.6_r8k, 0.8_r8k, 0.0_r8k, &
        &   0.6_r8k, 0.8_r8k, 0.5_r8k, &
        &   0.6_r8k, 0.8_r8k, 1.0_r8k, &
        &   0.6_r8k, 1.0_r8k, 0.0_r8k, &
        &   0.6_r8k, 1.0_r8k, 0.5_r8k, &
        &   0.6_r8k, 1.0_r8k, 1.0_r8k, &
        &   0.7_r8k, 0.0_r8k, 0.0_r8k, &
        &   0.7_r8k, 0.0_r8k, 0.5_r8k, &
        &   0.7_r8k, 0.0_r8k, 1.0_r8k, &
        &   0.7_r8k, 0.2_r8k, 0.0_r8k, &
        &   0.7_r8k, 0.2_r8k, 0.5_r8k, &
        &   0.7_r8k, 0.2_r8k, 1.0_r8k, &
        &   0.7_r8k, 0.4_r8k, 0.0_r8k, &
        &   0.7_r8k, 0.4_r8k, 0.5_r8k, &
        &   0.7_r8k, 0.4_r8k, 1.0_r8k, &
        &   0.7_r8k, 0.6_r8k, 0.0_r8k, &
        &   0.7_r8k, 0.6_r8k, 0.5_r8k, &
        &   0.7_r8k, 0.6_r8k, 1.0_r8k, &
        &   0.7_r8k, 0.8_r8k, 0.0_r8k, &
        &   0.7_r8k, 0.8_r8k, 0.5_r8k, &
        &   0.7_r8k, 0.8_r8k, 1.0_r8k, &
        &   0.7_r8k, 1.0_r8k, 0.0_r8k, &
        &   0.7_r8k, 1.0_r8k, 0.5_r8k, &
        &   0.7_r8k, 1.0_r8k, 1.0_r8k, &
        &   0.8_r8k, 0.0_r8k, 0.0_r8k, &
        &   0.8_r8k, 0.0_r8k, 0.5_r8k, &
        &   0.8_r8k, 0.0_r8k, 1.0_r8k, &
        &   0.8_r8k, 0.2_r8k, 0.0_r8k, &
        &   0.8_r8k, 0.2_r8k, 0.5_r8k, &
        &   0.8_r8k, 0.2_r8k, 1.0_r8k, &
        &   0.8_r8k, 0.4_r8k, 0.0_r8k, &
        &   0.8_r8k, 0.4_r8k, 0.5_r8k, &
        &   0.8_r8k, 0.4_r8k, 1.0_r8k, &
        &   0.8_r8k, 0.6_r8k, 0.0_r8k, &
        &   0.8_r8k, 0.6_r8k, 0.5_r8k, &
        &   0.8_r8k, 0.6_r8k, 1.0_r8k, &
        &   0.8_r8k, 0.8_r8k, 0.0_r8k, &
        &   0.8_r8k, 0.8_r8k, 0.5_r8k, &
        &   0.8_r8k, 0.8_r8k, 1.0_r8k, &
        &   0.8_r8k, 1.0_r8k, 0.0_r8k, &
        &   0.8_r8k, 1.0_r8k, 0.5_r8k, &
        &   0.8_r8k, 1.0_r8k, 1.0_r8k, &
        &   0.9_r8k, 0.0_r8k, 0.0_r8k, &
        &   0.9_r8k, 0.0_r8k, 0.5_r8k, &
        &   0.9_r8k, 0.0_r8k, 1.0_r8k, &
        &   0.9_r8k, 0.2_r8k, 0.0_r8k, &
        &   0.9_r8k, 0.2_r8k, 0.5_r8k, &
        &   0.9_r8k, 0.2_r8k, 1.0_r8k, &
        &   0.9_r8k, 0.4_r8k, 0.0_r8k, &
        &   0.9_r8k, 0.4_r8k, 0.5_r8k, &
        &   0.9_r8k, 0.4_r8k, 1.0_r8k, &
        &   0.9_r8k, 0.6_r8k, 0.0_r8k, &
        &   0.9_r8k, 0.6_r8k, 0.5_r8k, &
        &   0.9_r8k, 0.6_r8k, 1.0_r8k, &
        &   0.9_r8k, 0.8_r8k, 0.0_r8k, &
        &   0.9_r8k, 0.8_r8k, 0.5_r8k, &
        &   0.9_r8k, 0.8_r8k, 1.0_r8k, &
        &   0.9_r8k, 1.0_r8k, 0.0_r8k, &
        &   0.9_r8k, 1.0_r8k, 0.5_r8k, &
        &   0.9_r8k, 1.0_r8k, 1.0_r8k, &
        &   1.0_r8k, 0.0_r8k, 0.0_r8k, &
        &   1.0_r8k, 0.0_r8k, 0.5_r8k, &
        &   1.0_r8k, 0.0_r8k, 1.0_r8k, &
        &   1.0_r8k, 0.2_r8k, 0.0_r8k, &
        &   1.0_r8k, 0.2_r8k, 0.5_r8k, &
        &   1.0_r8k, 0.2_r8k, 1.0_r8k, &
        &   1.0_r8k, 0.4_r8k, 0.0_r8k, &
        &   1.0_r8k, 0.4_r8k, 0.5_r8k, &
        &   1.0_r8k, 0.4_r8k, 1.0_r8k, &
        &   1.0_r8k, 0.6_r8k, 0.0_r8k, &
        &   1.0_r8k, 0.6_r8k, 0.5_r8k, &
        &   1.0_r8k, 0.6_r8k, 1.0_r8k, &
        &   1.0_r8k, 0.8_r8k, 0.0_r8k, &
        &   1.0_r8k, 0.8_r8k, 0.5_r8k, &
        &   1.0_r8k, 0.8_r8k, 1.0_r8k, &
        &   1.0_r8k, 1.0_r8k, 0.0_r8k, &
        &   1.0_r8k, 1.0_r8k, 0.5_r8k, &
        &   1.0_r8k, 1.0_r8k, 1.0_r8k ], [3,n_x*n_y*n_z] )
    ! rectilinear grid
    real(r8k), dimension(n_x), parameter :: x_coords = &
        & [ 0.1_r8k, 0.2_r8k, 0.3_r8k, 0.4_r8k, 0.5_r8k, 0.6_r8k, 0.7_r8k, 0.8_r8k, 0.9_r8k, 1.0_r8k, 1.1_r8k ]
    real(r8k), dimension(n_y), parameter :: y_coords = &
        & [ 0.2_r8k, 0.4_r8k, 0.6_r8k, 0.8_r8k, 1.0_r8k, 1.2_r8k ]
    real(r8k), dimension(n_z), parameter :: z_coords = &
        & [ 0.5_r8k, 1.0_r8k, 1.5_r8k ]
    ! polygonal data
    integer(i4k), parameter :: n_x_poly = 3, n_y_poly = 3, n_z_poly = 3, n_faces = 20
    real(r8k), dimension(3,n_x_poly*n_y_poly*n_z_poly), parameter :: points_poly = reshape ( &
        & [ 0.0_r8k,   0.0_r8k,   0.0_r8k,  &
        &   0.5_r8k,   0.0_r8k,   0.0_r8k,  &
        &   1.0_r8k,   0.0_r8k,   0.0_r8k,  &
        &   0.0_r8k,   0.5_r8k,   0.0_r8k,  &
        &   0.5_r8k,   0.5_r8k,   0.0_r8k,  &
        &   1.0_r8k,   0.5_r8k,   0.0_r8k,  &
        &   0.0_r8k,   1.0_r8k,   0.0_r8k,  &
        &   0.5_r8k,   1.0_r8k,   0.0_r8k,  &
        &   1.0_r8k,   1.0_r8k,   0.0_r8k,  &
        &   0.25_r8k,  0.25_r8k,  0.5_r8k,  &
        &   0.50_r8k,  0.25_r8k,  0.5_r8k,  &
        &   0.75_r8k,  0.25_r8k,  0.5_r8k,  &
        &   0.25_r8k,  0.50_r8k,  0.5_r8k,  &
        &   0.50_r8k,  0.50_r8k,  0.5_r8k,  &
        &   0.75_r8k,  0.50_r8k,  0.5_r8k,  &
        &   0.25_r8k,  0.75_r8k,  0.5_r8k,  &
        &   0.50_r8k,  0.75_r8k,  0.5_r8k,  &
        &   0.75_r8k,  0.75_r8k,  0.5_r8k,  &
        &   0.375_r8k, 0.375_r8k, 0.75_r8k, &
        &   0.50_r8k,  0.375_r8k, 0.75_r8k, &
        &   0.625_r8k, 0.375_r8k, 0.75_r8k, &
        &   0.375_r8k, 0.50_r8k,  0.75_r8k, &
        &   0.50_r8k,  0.50_r8k,  0.75_r8k, &
        &   0.625_r8k, 0.50_r8k,  0.75_r8k, &
        &   0.375_r8k, 0.625_r8k, 0.75_r8k, &
        &   0.50_r8k,  0.625_r8k, 0.75_r8k, &
        &   0.625_r8k, 0.625_r8k, 0.75_r8k ], [3,n_x_poly*n_y_poly*n_z_poly] )
    type(polygon), dimension(n_faces) :: polygon_faces
    ! unstructured grid
    integer(i4k), parameter :: n_unstr_grid = 24, n_unstr_cells = 5
    real(r8k), dimension(3,n_unstr_grid), parameter :: points_unstr_grid = reshape ( &
        & [ 0.5_r8k, 0.0_r8k, 0.0_r8k, &
        &   1.0_r8k, 0.0_r8k, 0.0_r8k, &
        &   0.5_r8k, 0.5_r8k, 0.0_r8k, &
        &   1.0_r8k, 0.5_r8k, 0.0_r8k, &
        &   0.5_r8k, 0.0_r8k, 0.5_r8k, &
        &   1.0_r8k, 0.0_r8k, 0.5_r8k, &
        &   0.5_r8k, 0.5_r8k, 0.5_r8k, &
        &   1.0_r8k, 0.5_r8k, 0.5_r8k, &
        &   0.0_r8k, 0.0_r8k, 1.0_r8k, &
        &   0.5_r8k, 0.0_r8k, 1.0_r8k, &
        &   1.0_r8k, 0.0_r8k, 1.0_r8k, &
        &   1.5_r8k, 0.0_r8k, 1.0_r8k, &
        &   0.0_r8k, 0.5_r8k, 1.0_r8k, &
        &   0.5_r8k, 0.5_r8k, 1.0_r8k, &
        &   1.0_r8k, 0.5_r8k, 1.0_r8k, &
        &   1.5_r8k, 0.5_r8k, 1.0_r8k, &
        &   0.0_r8k, 0.0_r8k, 1.5_r8k, &
        &   0.5_r8k, 0.0_r8k, 1.5_r8k, &
        &   1.0_r8k, 0.0_r8k, 1.5_r8k, &
        &   1.5_r8k, 0.0_r8k, 1.5_r8k, &
        &   0.0_r8k, 0.5_r8k, 1.5_r8k, &
        &   0.5_r8k, 0.5_r8k, 1.5_r8k, &
        &   1.0_r8k, 0.5_r8k, 1.5_r8k, &
        &   1.5_r8k, 0.5_r8k, 1.5_r8k ], [3,n_unstr_grid] )
    type(voxel), dimension(n_unstr_cells) :: cells

contains

    subroutine vtk_datasets_unit (test_pass)
        use vtk_datasets, only : dataset, struct_pts, struct_grid, rectlnr_grid, polygonal_data, unstruct_grid
        use vtk,          only : vtk_legacy_write
        implicit none
        !!
        !! loops over each dataset type, performs a write, then performs a read on a different dataset
        !! and compares the two to make sure they are identical
        class(dataset), allocatable :: vtk_dataset_1, vtk_dataset_2
        integer(i4k)                :: i
        logical, intent(out)        :: test_pass
        logical, dimension(n_types) :: individual_tests_pass

        do i = 1, n_types
            if (allocated(vtk_dataset_1)) deallocate (vtk_dataset_1)
            if (allocated(vtk_dataset_2)) deallocate (vtk_dataset_2)
            select case (i)
            case (1)
                !! structured points
                allocate(struct_pts::vtk_dataset_1, vtk_dataset_2)
                call vtk_dataset_1%init(dims=dims, origin=origin, spacing=spacing)
            case (2)
                !! structured grid
                allocate(struct_grid::vtk_dataset_1, vtk_dataset_2)
                call vtk_dataset_1%init(dims=dims, points=points)
            case (3)
                !! rectilinear grid
                allocate(rectlnr_grid::vtk_dataset_1, vtk_dataset_2)
                call vtk_dataset_1%init(dims=dims, x_coords=x_coords, y_coords=y_coords, z_coords=z_coords)
            case (4)
                !! polygonal data
                call polygon_faces(1)%setup ( [ 0, 1, 4, 3 ] )
                call polygon_faces(2)%setup ( [ 1, 2, 5, 4 ] )
                call polygon_faces(3)%setup ( [ 3, 4, 7, 6 ] )
                call polygon_faces(4)%setup ( [ 4, 5, 8, 7 ] )
                call polygon_faces(5)%setup ( [ 9, 10, 13, 12 ] )
                call polygon_faces(6)%setup ( [ 10, 11, 14, 13 ] )
                call polygon_faces(7)%setup ( [ 12, 13, 16, 15 ] )
                call polygon_faces(8)%setup ( [ 13, 14, 17, 16 ] )
                call polygon_faces(9)%setup ( [ 18, 19, 22, 21 ] )
                call polygon_faces(10)%setup ( [ 19, 20, 23, 22 ] )
                call polygon_faces(11)%setup ( [ 21, 22, 25, 24 ] )
                call polygon_faces(12)%setup ( [ 22, 23, 26, 25 ] )
                call polygon_faces(13)%setup ( [ 0, 1, 10, 19, 18, 9 ] )
                call polygon_faces(14)%setup ( [ 1, 2, 11, 20, 19, 10 ] )
                call polygon_faces(15)%setup ( [ 0, 3, 12, 21, 18, 9 ] )
                call polygon_faces(16)%setup ( [ 3, 6, 15, 24, 21, 12 ] )
                call polygon_faces(17)%setup ( [ 6, 7, 16, 25, 24, 15 ] )
                call polygon_faces(18)%setup ( [ 7, 8, 17, 26, 25, 16 ] )
                call polygon_faces(19)%setup ( [ 8, 5, 14, 23, 26, 17 ] )
                call polygon_faces(20)%setup ( [ 5, 2, 11, 20, 23, 14 ] )
                allocate(polygonal_data::vtk_dataset_1, vtk_dataset_2)
                call vtk_dataset_1%init(dims=dims, points=points_poly, polygons=polygon_faces)
            case (5)
                !! unstructured grid
                call cells(1)%setup ( [ 0, 1, 2, 3, 4, 5, 6, 7 ] )
                call cells(2)%setup ( [ 4, 5, 6, 7, 9, 10, 13, 14 ] )
                call cells(3)%setup ( [ 8, 9, 12, 13, 16, 17, 20, 21 ] )
                call cells(4)%setup ( [ 9, 10, 13, 14, 17, 18, 21, 22 ] )
                call cells(5)%setup ( [ 10, 11, 14, 15, 18, 19, 22, 23 ] )
                allocate(unstruct_grid::vtk_dataset_1, vtk_dataset_2)
                call vtk_dataset_1%init(points=points_unstr_grid, cells=cells)
            end select

            !! data type is generated from the defined values above
            open (unit=vtk_unit, file=filename(i), form='formatted')
            call vtk_dataset_1%write(vtk_unit)
            close(unit=vtk_unit)

            !! data type is generated from the read
            open (unit=vtk_unit, file=filename(i), status='old', form='formatted')
            call vtk_dataset_2%read(vtk_unit)
            close(unit=vtk_unit)

            !! compare the read file and the written/read file to ensure both types are the same
            individual_tests_pass(i) = .not. (vtk_dataset_1 .diff. vtk_dataset_2)

        end do

        test_pass = all(individual_tests_pass)

    end subroutine vtk_datasets_unit

end module vtk_datasets_unit_tests

program vtk_datasets_test
    use vtk_datasets_unit_tests, only : vtk_datasets_unit
    use vtkmofopassfail,         only : all_tests_pass
    implicit none
    !! author: Ian Porter
    !! date: 12/14/2017
    !!
    !! driver testing subroutine for the attributes information
    !!
    logical :: test_passes = .false.

    call vtk_datasets_unit (test_passes)

    if (test_passes) call all_tests_pass()

end program vtk_datasets_test
