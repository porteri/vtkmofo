MODULE vtk_datasets_unit_tests
    USE Precision
    USE vtk_cells, ONLY : polygon, voxel
    IMPLICIT NONE
    !! author: Ian Porter
    !! date: 12/17/2017
    !!
    !! Unit testing for datasets
    !!
    PRIVATE
    PUBLIC :: vtk_datasets_unit
! Generic information
    INTEGER(i4k), PARAMETER :: n_types  = 5
    INTEGER(i4k), PARAMETER :: vtk_unit = 20
    INTEGER(i4k), PARAMETER :: n_x = 11, n_y = 6, n_z = 3
    CHARACTER(LEN=*), DIMENSION(n_types), PARAMETER :: filename = &
      & [ 'struct_pts.vtk      ', &
      &   'struct_grid.vtk     ', &
      &   'rectlnr_grid.vtk    ', &
      &   'polygonal_data.vtk  ', &
      &   'unstruct_grid.vtk   ' ]
! Structured points
    INTEGER(i4k), DIMENSION(3), PARAMETER :: dims    = &
      & [ n_x, n_y, n_z ]
    REAL(r8k), DIMENSION(3), PARAMETER    :: origin  = &
      & [ 0.0_r8k, 0.0_r8k, 0.0_r8k ]
    REAL(r8k), DIMENSION(3), PARAMETER    :: spacing = &
      & [ 0.1_r8k, 0.2_r8k, 0.5_r8k ]
! Structured grid
    REAL(r8k), DIMENSION(3,n_x*n_y*n_z), PARAMETER :: points = RESHAPE ( & !! [x, y, z]
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
! Rectilinear grid
    REAL(r8k), DIMENSION(n_x), PARAMETER :: x_coords = &
      & [ 0.1_r8k, 0.2_r8k, 0.3_r8k, 0.4_r8k, 0.5_r8k, 0.6_r8k, 0.7_r8k, 0.8_r8k, 0.9_r8k, 1.0_r8k, 1.1_r8k ]
    REAL(r8k), DIMENSION(n_y), PARAMETER :: y_coords = &
      & [ 0.2_r8k, 0.4_r8k, 0.6_r8k, 0.8_r8k, 1.0_r8k, 1.2_r8k ]
    REAL(r8k), DIMENSION(n_z), PARAMETER :: z_coords = &
      & [ 0.5_r8k, 1.0_r8k, 1.5_r8k ]
! Polygonal data
    INTEGER(i4k), PARAMETER :: n_x_poly = 3, n_y_poly = 3, n_z_poly = 3, n_faces = 20
    REAL(r8k), DIMENSION(3,n_x_poly*n_y_poly*n_z_poly), PARAMETER :: points_poly = RESHAPE ( &
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
    TYPE(polygon), DIMENSION(n_faces) :: polygon_faces
! Unstructured grid
    INTEGER(i4k), PARAMETER :: n_unstr_grid = 24, n_unstr_cells = 5
    REAL(r8k), DIMENSION(3,n_unstr_grid), PARAMETER :: points_unstr_grid = RESHAPE ( &
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
    TYPE(voxel), DIMENSION(n_unstr_cells) :: cells
    CONTAINS
        SUBROUTINE vtk_datasets_unit (test_pass)
        USE Precision
        USE vtk_datasets, ONLY : dataset, struct_pts, struct_grid, rectlnr_grid, polygonal_data, unstruct_grid
        USE VTK,          ONLY : vtk_legacy_write
        IMPLICIT NONE
        !!
        !! Loops over each dataset type, performs a write, then performs a read on a different dataset
        !! and compares the two to make sure they are identical
        CLASS(dataset), ALLOCATABLE :: vtk_dataset_1, vtk_dataset_2
        INTEGER(i4k)                :: i
        LOGICAL, INTENT(OUT)        :: test_pass
        LOGICAL, DIMENSION(n_types) :: individual_tests_pass

        DO i = 1, n_types
            write(0,*) 'i= ',i
            IF (ALLOCATED(vtk_dataset_1)) DEALLOCATE (vtk_dataset_1)
            IF (ALLOCATED(vtk_dataset_2)) DEALLOCATE (vtk_dataset_2)
            SELECT CASE (i)
            CASE (1)
                !! Structured points
                ALLOCATE(struct_pts::vtk_dataset_1, vtk_dataset_2)
                CALL vtk_dataset_1%init(dims=dims, origin=origin, spacing=spacing)
            CASE (2)
                !! Structured grid
                ALLOCATE(struct_grid::vtk_dataset_1, vtk_dataset_2)
                CALL vtk_dataset_1%init(dims=dims, points=points)
            CASE (3)
                !! Rectilinear grid
                ALLOCATE(rectlnr_grid::vtk_dataset_1, vtk_dataset_2)
                CALL vtk_dataset_1%init(dims=dims, x_coords=x_coords, y_coords=y_coords, z_coords=z_coords)
            CASE (4)
                !! Polygonal data
                CALL polygon_faces(1)%setup ( [ 0, 1, 4, 3 ] )
                CALL polygon_faces(2)%setup ( [ 1, 2, 5, 4 ] )
                CALL polygon_faces(3)%setup ( [ 3, 4, 7, 6 ] )
                CALL polygon_faces(4)%setup ( [ 4, 5, 8, 7 ] )
                CALL polygon_faces(5)%setup ( [ 9, 10, 13, 12 ] )
                CALL polygon_faces(6)%setup ( [ 10, 11, 14, 13 ] )
                CALL polygon_faces(7)%setup ( [ 12, 13, 16, 15 ] )
                CALL polygon_faces(8)%setup ( [ 13, 14, 17, 16 ] )
                CALL polygon_faces(9)%setup ( [ 18, 19, 22, 21 ] )
                CALL polygon_faces(10)%setup ( [ 19, 20, 23, 22 ] )
                CALL polygon_faces(11)%setup ( [ 21, 22, 25, 24 ] )
                CALL polygon_faces(12)%setup ( [ 22, 23, 26, 25 ] )
                CALL polygon_faces(13)%setup ( [ 0, 1, 10, 19, 18, 9 ] )
                CALL polygon_faces(14)%setup ( [ 1, 2, 11, 20, 19, 10 ] )
                CALL polygon_faces(15)%setup ( [ 0, 3, 12, 21, 18, 9 ] )
                CALL polygon_faces(16)%setup ( [ 3, 6, 15, 24, 21, 12 ] )
                CALL polygon_faces(17)%setup ( [ 6, 7, 16, 25, 24, 15 ] )
                CALL polygon_faces(18)%setup ( [ 7, 8, 17, 26, 25, 16 ] )
                CALL polygon_faces(19)%setup ( [ 8, 5, 14, 23, 26, 17 ] )
                CALL polygon_faces(20)%setup ( [ 5, 2, 11, 20, 23, 14 ] )
                ALLOCATE(polygonal_data::vtk_dataset_1, vtk_dataset_2)
                CALL vtk_dataset_1%init(dims=dims, points=points_poly, polygons=polygon_faces)
            CASE (5)
                !! Unstructured grid
                CALL cells(1)%setup ( [ 0, 1, 2, 3, 4, 5, 6, 7 ] )
                CALL cells(2)%setup ( [ 4, 5, 6, 7, 9, 10, 13, 14 ] )
                CALL cells(3)%setup ( [ 8, 9, 12, 13, 16, 17, 20, 21 ] )
                CALL cells(4)%setup ( [ 9, 10, 13, 14, 17, 18, 21, 22 ] )
                CALL cells(5)%setup ( [ 10, 11, 14, 15, 18, 19, 22, 23 ] )
                ALLOCATE(unstruct_grid::vtk_dataset_1, vtk_dataset_2)
                CALL vtk_dataset_1%init(points=points_unstr_grid, cells=cells)
            END SELECT
write(0,*) 'before write'
            !! Data type is generated from the defined values above
            OPEN (unit=vtk_unit, file=filename(i), form='formatted', access='SEQUENTIAL')
            WRITE(unit=vtk_unit,FMT='(DT)') vtk_dataset_1
            CLOSE(unit=vtk_unit)
write(0,*) 'before read'
            !! Data type is generated from the read
            OPEN (unit=vtk_unit, file=filename(i), status='old', form='formatted', access='SEQUENTIAL')
            READ (unit=vtk_unit,FMT='(DT)') vtk_dataset_2
            CLOSE(unit=vtk_unit)
write(0,*) 'after read'
            !! Compare the read file and the written/read file to ensure both types are the same
            individual_tests_pass(i) = .NOT. (vtk_dataset_1 .diff. vtk_dataset_2)

        END DO

        test_pass = ALL(individual_tests_pass)

        END SUBROUTINE vtk_datasets_unit
END MODULE vtk_datasets_unit_tests

PROGRAM vtk_datasets_test
    USE vtk_datasets_unit_tests, ONLY : vtk_datasets_unit
    USE VTKmofoPassFail,         ONLY : all_tests_pass
    IMPLICIT NONE
    !! author: Ian Porter
    !! date: 12/14/2017
    !!
    !! Driver testing subroutine for the attributes information
    !!
    LOGICAL :: test_passes = .FALSE.

    CALL vtk_datasets_unit (test_passes)

    IF (test_passes) CALL all_tests_pass()

END PROGRAM vtk_datasets_test
