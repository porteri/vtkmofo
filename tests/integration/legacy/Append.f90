PROGRAM Append_test
    USE Precision,      ONLY : i4k, r8k
    USE vtk_datasets,   ONLY : unstruct_grid
    USE vtk_attributes, ONLY : scalar, attributes
    USE vtk_cells,      ONLY : voxel, hexahedron, vtkcell_list
    USE vtk,            ONLY : vtk_legacy_write
    IMPLICIT NONE
    !! author: Ian Porter
    !! date: 06/03/2019
    !!
    !! This is a test for appending to a file
    !!
    TYPE (unstruct_grid)        :: I_shape
    TYPE (attributes), DIMENSION(:), ALLOCATABLE :: point_vals_to_write, cell_vals_to_write
    TYPE (scalar)               :: cell_val_1, cell_val_2, point_val_1, point_val_2, point_val_3
    INTEGER(i4k)                :: t
    INTEGER(i4k),     PARAMETER :: n_points = 32, n_cells = 7, unit = 20, n_steps = 10
    CHARACTER(LEN=*), PARAMETER :: filename = 'legacy_append_unstruct_grid'
    CHARACTER(LEN=*), PARAMETER :: title    = 'Testing of appending to a VTK file'
    CHARACTER(LEN=8)            :: t_char
    REAL(r8k), DIMENSION(n_points,1:3) :: point_vals = 0.0_r8k
    REAL(r8k), DIMENSION(3,n_points), PARAMETER :: points = RESHAPE ( &
      & [ 0.0_r8k, 0.0_r8k, 0.0_r8k, &
      &   1.0_r8k, 0.0_r8k, 0.0_r8k, &
      &   2.0_r8k, 0.0_r8k, 0.0_r8k, &
      &   3.0_r8k, 0.0_r8k, 0.0_r8k, &
      &   0.0_r8k, 1.0_r8k, 0.0_r8k, &
      &   1.0_r8k, 1.0_r8k, 0.0_r8k, &
      &   2.0_r8k, 1.0_r8k, 0.0_r8k, &
      &   3.0_r8k, 1.0_r8k, 0.0_r8k, &
      &   0.0_r8k, 0.0_r8k, 1.0_r8k, &
      &   1.0_r8k, 0.0_r8k, 1.0_r8k, &
      &   2.0_r8k, 0.0_r8k, 1.0_r8k, &
      &   3.0_r8k, 0.0_r8k, 1.0_r8k, &
      &   0.0_r8k, 1.0_r8k, 1.0_r8k, &
      &   1.0_r8k, 1.0_r8k, 1.0_r8k, &
      &   2.0_r8k, 1.0_r8k, 1.0_r8k, &
      &   3.0_r8k, 1.0_r8k, 1.0_r8k, &
      &   0.0_r8k, 0.0_r8k, 2.0_r8k, &
      &   1.0_r8k, 0.0_r8k, 2.0_r8k, &
      &   2.0_r8k, 0.0_r8k, 2.0_r8k, &
      &   3.0_r8k, 0.0_r8k, 2.0_r8k, &
      &   0.0_r8k, 1.0_r8k, 2.0_r8k, &
      &   1.0_r8k, 1.0_r8k, 2.0_r8k, &
      &   2.0_r8k, 1.0_r8k, 2.0_r8k, &
      &   3.0_r8k, 1.0_r8k, 2.0_r8k, &
      &   0.0_r8k, 0.0_r8k, 3.0_r8k, &
      &   1.0_r8k, 0.0_r8k, 3.0_r8k, &
      &   2.0_r8k, 0.0_r8k, 3.0_r8k, &
      &   3.0_r8k, 0.0_r8k, 3.0_r8k, &
      &   0.0_r8k, 1.0_r8k, 3.0_r8k, &
      &   1.0_r8k, 1.0_r8k, 3.0_r8k, &
      &   2.0_r8k, 1.0_r8k, 3.0_r8k, &
      &   3.0_r8k, 1.0_r8k, 3.0_r8k ], [3,n_points] )
    REAL(r8k), PARAMETER :: temp_default = 100.0_r8k, temp_increment = 10.0_r8k
    REAL(r8k), DIMENSION(n_points), PARAMETER :: temp_norm = &
      & [ 0.8_r8k, &
      &   1.0_r8k, &
      &   1.2_r8k, &
      &   0.8_r8k, &
      &   1.0_r8k, &
      &   1.2_r8k, &
      &   0.8_r8k, &
      &   1.0_r8k, &
      &   1.2_r8k, &
      &   0.8_r8k, &
      &   1.0_r8k, &
      &   1.2_r8k, &
      &   0.8_r8k, &
      &   1.0_r8k, &
      &   1.2_r8k, &
      &   0.8_r8k, &
      &   1.0_r8k, &
      &   1.2_r8k, &
      &   0.8_r8k, &
      &   1.0_r8k, &
      &   1.2_r8k, &
      &   0.8_r8k, &
      &   1.0_r8k, &
      &   1.2_r8k, &
      &   0.8_r8k, &
      &   1.0_r8k, &
      &   1.2_r8k, &
      &   0.8_r8k, &
      &   1.0_r8k, &
      &   1.2_r8k, &
      &   0.8_r8k, &
      &   1.0_r8k ]
    INTEGER(i4k), DIMENSION(n_cells), PARAMETER :: cellID  = &
      & [ 1, 2, 3, 4, 5, 6, 7 ]
    INTEGER(i4k), DIMENSION(n_cells), PARAMETER :: matType = &
      & [ 10, 10, 10, 20, 10, 10, 10 ]
    TYPE(voxel),        DIMENSION(n_cells-1) :: voxel_cells     !! Voxel cell type
    TYPE(hexahedron)                         :: hexahedron_cell !! Hexahedron cell type
    TYPE(vtkcell_list), DIMENSION(n_cells)   :: cell_list       !! Full list of all cells
    CHARACTER(LEN=15), DIMENSION(*), PARAMETER :: point_dataname = &
      & [ 'Temperature(K) ','Stress (Pa)    ' ]

    CALL voxel_cells(1)%setup ( [  0,  1,  4,  5,  8,  9, 12, 13 ] )
    CALL voxel_cells(2)%setup ( [  1,  2,  5,  6,  9, 10, 13, 14 ] )
    CALL voxel_cells(3)%setup ( [  2,  3,  6,  7, 10, 11, 14, 15 ] )
    CALL voxel_cells(4)%setup ( [ 16, 17, 20, 21, 24, 25, 28, 29 ] )
    CALL voxel_cells(5)%setup ( [ 17, 18, 21, 22, 25, 26, 29, 30 ] )
    CALL voxel_cells(6)%setup ( [ 18, 19, 22, 23, 26, 27, 30, 31 ] )
    CALL hexahedron_cell%setup ( [ 9, 10, 14, 13, 17, 18, 22, 21 ] )

    ALLOCATE(cell_list(1)%cell,source=voxel_cells(1)) !! Workaround for gfortran-8.2
    ALLOCATE(cell_list(2)%cell,source=voxel_cells(2))
    ALLOCATE(cell_list(3)%cell,source=voxel_cells(3))
    ALLOCATE(cell_list(4)%cell,source=hexahedron_cell)
    ALLOCATE(cell_list(5)%cell,source=voxel_cells(4))
    ALLOCATE(cell_list(6)%cell,source=voxel_cells(5))
    ALLOCATE(cell_list(7)%cell,source=voxel_cells(6))

    CALL I_shape%init (points=points, cell_list=cell_list)

    DO t = 1, n_steps
        WRITE(t_char,'(i0)') t
        point_vals(:,1) = temp_default + (t-1) * temp_increment * temp_norm

        CALL vtk_legacy_write (I_shape, unit=unit, filename=filename, title=title, multiple_io=.TRUE.)
                                   !! Initialize the new file with just geometry information
        ! Cell values
        CALL cell_val_1%init (dataname='cellID', numcomp=1, int1d=cellID)
        CALL cell_val_2%init (dataname='matType', numcomp=1, int1d=matType)
        IF (ALLOCATED(cell_vals_to_write)) DEALLOCATE(cell_vals_to_write)
        ALLOCATE(cell_vals_to_write(1))
        ALLOCATE(cell_vals_to_write(1)%attribute,source=cell_val_2)

        CALL vtk_legacy_write (celldata=cell_val_1)
                                !! Append cell information
        CALL vtk_legacy_write (celldatasets=cell_vals_to_write)
                                !! Append cell information
        ! Point values
        CALL point_val_1%init (dataname='Temperature_(K)', numcomp=1, real1d=point_vals(:,1))
        CALL point_val_2%init (dataname='Stress_(Pa)', numcomp=1, real1d=point_vals(:,2))
        CALL point_val_3%init (dataname='Displacement_(m)', numcomp=1, real1d=point_vals(:,3))
        IF (ALLOCATED(point_vals_to_write)) DEALLOCATE(point_vals_to_write)
        ALLOCATE(point_vals_to_write(2))
        ALLOCATE(point_vals_to_write(1)%attribute,source=point_val_2)
        ALLOCATE(point_vals_to_write(2)%attribute,source=point_val_3)

        CALL vtk_legacy_write (pointdata=point_val_1)
                                !! Append point information
        CALL vtk_legacy_write (pointdatasets=point_vals_to_write)
                                !! Append point information
        CALL vtk_legacy_write (finished=.TRUE.)
                                !! Append point information
    END DO

    WRITE(*,*) 'Finished'

END PROGRAM Append_test
