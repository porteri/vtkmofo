PROGRAM vtk_test
    USE Kinds
    USE VTK_Types, ONLY : ASCII, Binary, vtk_datastruct, struct_pts, struct_grid, unstruct_grid
    USE VTK,       ONLY : vtk_legacy_init
    IMPLICIT NONE
    INTEGER(i4k) :: i
    INTEGER(i4k), PARAMETER :: n_types = 1
    INTEGER(i4k), PARAMETER :: n_x = 11, n_y = 6, n_z = 3
    INTEGER(i4k), PARAMETER :: vtk_unit = 20
    INTEGER(i4k), DIMENSION(n_types), PARAMETER :: filetype = &
      &  [ ASCII ]
    CHARACTER(LEN=*), DIMENSION(n_types), PARAMETER :: filename = &
      & [ 'struct_grid.vtk' ]
    INTEGER(i4k), DIMENSION(3), PARAMETER :: dims    = &
      & [ n_x, n_y, n_z ]
    REAL(r8k), DIMENSION(3), PARAMETER    :: origin  = &
      & [ 0.0_r8k, 0.0_r8k, 0.0_r8k ]
    REAL(r8k), DIMENSION(3), PARAMETER    :: spacing = &
      & [ 0.1_r8k, 0.2_r8k, 0.5_r8k ]
    REAL(r8k), DIMENSION(3,n_x*n_y*n_z), PARAMETER :: points  = & !! [x, y, z]
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
      &   1.0_r8k, 1.0_r8k, 1.0_r8k ]

    CLASS(vtk_datastruct), DIMENSION(:), ALLOCATABLE :: vtk_type

    ALLOCATE(struct_pts::vtk_type(1:n_types))

    DO i = 1, n_types
        !! Loop over all possible data types
        IF (vtk_type(i)%firstcall) CALL vtk_type(i)%setup(unit=vtk_unit, dims=dims, origin=origin, spacing=spacing, points=points)

        CALL vtk_legacy_init (vtk_type(i), filetype(i), filename(i))
    END DO

END PROGRAM vtk_test