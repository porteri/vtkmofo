PROGRAM T_shape_test
    USE Precision
    USE vtk_datasets,   ONLY : unstruct_grid
    USE vtk_attributes, ONLY : scalar, attributes
    USE vtk_cells,      ONLY : voxel, hexahedron, vtkcell_list
    USE vtk,            ONLY : vtk_legacy_write
    IMPLICIT NONE
    !>@brief
    !> This is a test of an unstructured grid (T-shape) geometry
    !>@author
    !> Ian Porter
    !>@date
    !> 01/04/2018

    INTEGER(i4k), PARAMETER     :: n_params_to_write = 1
    TYPE (unstruct_grid)        :: t_shape
    TYPE (attributes), DIMENSION(n_params_to_write) :: point_vals_to_write, cell_vals_to_write
    INTEGER(i4k)                :: i, t
    INTEGER(i4k),     PARAMETER :: n_points = 24, n_cells = 5, unit = 20, n_steps = 10
    CHARACTER(LEN=*), PARAMETER :: filename = 't_shape.vtk'
    CHARACTER(LEN=*), PARAMETER :: title    = 'Testing of T-shape unstructured grid geometry'
    CHARACTER(LEN=8)            :: t_char
    REAL(r8k), DIMENSION(n_cells, 1:n_params_to_write) :: cell_vals
    REAL(r8k), DIMENSION(n_points,1:n_params_to_write) :: point_vals
    REAL(r8k), DIMENSION(3,n_points), PARAMETER        :: points = RESHAPE ( &
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
      &   1.5_r8k, 0.5_r8k, 1.5_r8k ], [3,n_points] )
    REAL(r8k), PARAMETER :: temp_default = 100.0_r8k, temp_increment = 10.0_r8k
    REAL(r8k), DIMENSION(n_points), PARAMETER :: temp_norm = &
      & [ 1.0_r8k, 1.0_r8k, 1.0_r8k, 1.0_r8k, 2.0_r8k, 2.0_r8k, 2.0_r8k, 2.0_r8k, 1.0_r8k, &
      &   3.0_r8k, 3.0_r8k, 1.0_r8k, 1.0_r8k, 4.0_r8k, 4.0_r8k, 1.0_r8k, 1.0_r8k, 2.0_r8k, &
      &   2.0_r8k, 1.0_r8k, 1.0_r8k, 3.0_r8k, 3.0_r8k, 1.0_r8k ]
    INTEGER(i4k), DIMENSION(n_cells), PARAMETER :: cellID = &
      & [ 11, 11, 11, 11, 12 ]
    TYPE(voxel),        DIMENSION(n_cells-1) :: voxel_cells     !! Voxel cell type
    TYPE(hexahedron)                         :: hexahedron_cell !! Hexahedron cell type
    TYPE(vtkcell_list), DIMENSION(n_cells)   :: cell_list       !! Full list of all cells
    CHARACTER(LEN=10), DIMENSION(n_params_to_write), PARAMETER :: cell_dataname = &
      & [ 'cellIDs   ' ]
    CHARACTER(LEN=15), DIMENSION(n_params_to_write), PARAMETER :: point_dataname = &
      & [ 'Temperature(K) ' ]

    CALL voxel_cells(1)%setup ( (/ 0, 1, 2, 3, 4, 5, 6, 7 /) )
    CALL voxel_cells(2)%setup ( (/ 4, 5, 6, 7, 9, 10, 13, 14 /) )
    CALL voxel_cells(3)%setup ( (/ 8, 9, 12, 13, 16, 17, 20, 21 /) )
    CALL voxel_cells(4)%setup ( (/ 9, 10, 13, 14, 17, 18, 21, 22 /) )
    CALL hexahedron_cell%setup ( (/ 10, 11, 15, 14, 18, 19, 23, 22 /) )
    cell_list(1)%cell = voxel_cells(1)
    cell_list(2)%cell = voxel_cells(2)
    cell_list(3)%cell = voxel_cells(3)
    cell_list(4)%cell = voxel_cells(4)
    cell_list(5)%cell   = hexahedron_cell

    CALL t_shape%init (points=points, cell_list=cell_list)

    DO t = 1, n_steps
        cell_vals(:,1)  = REAL(cellID(:)); WRITE(t_char,'(i0)') t
        point_vals(:,1) = temp_default + (t-1) * temp_increment * temp_norm

        DO i = 1, n_params_to_write
            ! Cell values
            IF (.NOT. ALLOCATED(cell_vals_to_write(i)%attribute))THEN
                ALLOCATE(scalar::cell_vals_to_write(i)%attribute)
                cell_vals_to_write(1)%n = SIZE(cell_vals(:,1))
            END IF
            CALL cell_vals_to_write(i)%attribute%init (TRIM(cell_dataname(i)), numcomp=1, values1d=cell_vals(:,i))
            ! Point values
            IF (.NOT. ALLOCATED(point_vals_to_write(i)%attribute))THEN
                ALLOCATE(scalar::point_vals_to_write(i)%attribute)
                point_vals_to_write(1)%n = SIZE(point_vals(:,1))
            END IF
            CALL point_vals_to_write(i)%attribute%init (TRIM(point_dataname(i)), numcomp=1, values1d=point_vals(:,i))
        END DO

        CALL vtk_legacy_write (unit, t_shape, celldatasets=cell_vals_to_write, pointdatasets=point_vals_to_write, &
          &                    filename=filename, title=title, multiple_io=.TRUE.)
    END DO

    WRITE(*,*) 'Finished'

END PROGRAM T_shape_test
