    program append_test
    use precision,      only : i4k, r8k
    use vtk_datasets,   only : unstruct_grid
    use vtk_attributes, only : scalar, attributes
    use vtk_cells,      only : voxel, hexahedron, vtkcell_list
    use vtk,            only : vtk_legacy_write
    implicit none
    !! author: Ian Porter
    !! date: 06/03/2019
    !!
    !! this is a test for appending to a file
    !!
    type (unstruct_grid)        :: i_shape
    type (attributes), dimension(:), allocatable :: point_vals_to_write, cell_vals_to_write
    type (scalar)               :: cell_val_1, cell_val_2, point_val_1, point_val_2, point_val_3
    integer(i4k)                :: t
    integer(i4k),     parameter :: n_points = 32, n_cells = 7, unit = 20, n_steps = 10
    character(len=*), parameter :: filename = 'legacy_append_unstruct_grid'
    character(len=*), parameter :: title    = 'Testing of appending to a VTK file'
    character(len=8)            :: t_char
    real(r8k), dimension(n_points,1:3) :: point_vals = 0.0_r8k
    real(r8k), dimension(3,n_points), parameter :: points = reshape ( &
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
    real(r8k), parameter :: temp_default = 100.0_r8k, temp_increment = 10.0_r8k
    real(r8k), dimension(n_points), parameter :: temp_norm = &
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
    integer(i4k), dimension(n_cells), parameter :: cellid  = &
        & [ 1, 2, 3, 4, 5, 6, 7 ]
    integer(i4k), dimension(n_cells), parameter :: mattype = &
        & [ 10, 10, 10, 20, 10, 10, 10 ]
    type(voxel),        dimension(n_cells-1) :: voxel_cells     !! voxel cell type
    type(hexahedron)                         :: hexahedron_cell !! hexahedron cell type
    type(vtkcell_list), dimension(n_cells)   :: cell_list       !! full list of all cells
    character(len=15), dimension(*), parameter :: point_dataname = &
        & [ 'Temperature(K) ','Stress (Pa)    ' ]

    call voxel_cells(1)%setup ( [  0,  1,  4,  5,  8,  9, 12, 13 ] )
    call voxel_cells(2)%setup ( [  1,  2,  5,  6,  9, 10, 13, 14 ] )
    call voxel_cells(3)%setup ( [  2,  3,  6,  7, 10, 11, 14, 15 ] )
    call voxel_cells(4)%setup ( [ 16, 17, 20, 21, 24, 25, 28, 29 ] )
    call voxel_cells(5)%setup ( [ 17, 18, 21, 22, 25, 26, 29, 30 ] )
    call voxel_cells(6)%setup ( [ 18, 19, 22, 23, 26, 27, 30, 31 ] )
    call hexahedron_cell%setup ( [ 9, 10, 14, 13, 17, 18, 22, 21 ] )

    allocate(cell_list(1)%cell,source=voxel_cells(1)) !! workaround for gfortran-8.2
    allocate(cell_list(2)%cell,source=voxel_cells(2))
    allocate(cell_list(3)%cell,source=voxel_cells(3))
    allocate(cell_list(4)%cell,source=hexahedron_cell)
    allocate(cell_list(5)%cell,source=voxel_cells(4))
    allocate(cell_list(6)%cell,source=voxel_cells(5))
    allocate(cell_list(7)%cell,source=voxel_cells(6))

    call i_shape%init (points=points, cell_list=cell_list)

    do t = 1, n_steps
        write(t_char,'(i0)') t
        point_vals(:,1) = temp_default + (t-1) * temp_increment * temp_norm

        call vtk_legacy_write (i_shape, unit=unit, filename=filename, title=title, multiple_io=.true.)
        !! initialize the new file with just geometry information
        ! cell values
        call cell_val_1%init (dataname='cellID', numcomp=1, int1d=cellid)
        call cell_val_2%init (dataname='matType', numcomp=1, int1d=mattype)
        if (allocated(cell_vals_to_write)) deallocate(cell_vals_to_write)
        allocate(cell_vals_to_write(1))
        allocate(cell_vals_to_write(1)%attribute,source=cell_val_2)

        call vtk_legacy_write (celldata=cell_val_1)
        !! append cell information
        call vtk_legacy_write (celldatasets=cell_vals_to_write)
        !! append cell information
        ! point values
        call point_val_1%init (dataname='Temperature_(K)', numcomp=1, real1d=point_vals(:,1))
        call point_val_2%init (dataname='Stress_(Pa)', numcomp=1, real1d=point_vals(:,2))
        call point_val_3%init (dataname='Displacement_(m)', numcomp=1, real1d=point_vals(:,3))
        if (allocated(point_vals_to_write)) deallocate(point_vals_to_write)
        allocate(point_vals_to_write(2))
        allocate(point_vals_to_write(1)%attribute,source=point_val_2)
        allocate(point_vals_to_write(2)%attribute,source=point_val_3)

        call vtk_legacy_write (pointdata=point_val_1)
        !! append point information
        call vtk_legacy_write (pointdatasets=point_vals_to_write)
        !! append point information
        call vtk_legacy_write (finished=.true.)
        !! append point information
    end do

    write(*,*) 'Finished'

end program append_test
