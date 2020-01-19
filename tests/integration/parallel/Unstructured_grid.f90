program parallel_t_shape_test
    use precision,      only : i4k, r8k
    use vtk_datasets,   only : unstruct_grid
    use vtk_attributes, only : scalar, attributes
    use vtk_cells,      only : voxel, hexahedron, vtkcell_list
    use vtk,            only : vtk_parallel_write
    implicit none
    !! author: Ian Porter
    !! date: 01/04/2018
    !!
    !! this is a test of an unstructured grid (t-shape) geometry
    !!
    integer(i4k), parameter     :: n_params_to_write = 1
    type (unstruct_grid)        :: t_shape
    type (attributes), dimension(n_params_to_write) :: point_vals_to_write, cell_vals_to_write
    integer(i4k)                :: i, t
    integer(i4k),     parameter :: n_points = 24, n_cells = 5, unit = 20, n_steps = 10
    character(len=*), parameter :: filename = 'parallel_unstruct_grid'
    character(len=8)            :: t_char
    real(r8k), dimension(n_cells, 1:n_params_to_write) :: cell_vals
    real(r8k), dimension(n_points,1:n_params_to_write) :: point_vals
    real(r8k), dimension(3,n_points), parameter        :: points = reshape ( &
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
    real(r8k), parameter :: temp_default = 100.0_r8k, temp_increment = 10.0_r8k
    real(r8k), dimension(n_points), parameter :: temp_norm = &
        & [ 1.0_r8k, 1.0_r8k, 1.0_r8k, 1.0_r8k, 2.0_r8k, 2.0_r8k, 2.0_r8k, 2.0_r8k, 1.0_r8k, &
        &   3.0_r8k, 3.0_r8k, 1.0_r8k, 1.0_r8k, 4.0_r8k, 4.0_r8k, 1.0_r8k, 1.0_r8k, 2.0_r8k, &
        &   2.0_r8k, 1.0_r8k, 1.0_r8k, 3.0_r8k, 3.0_r8k, 1.0_r8k ]
    integer(i4k), dimension(n_cells), parameter :: cellid = &
        & [ 11, 11, 11, 11, 12 ]
    type(voxel),        dimension(n_cells-1) :: voxel_cells     !! voxel cell type
    type(hexahedron)                         :: hexahedron_cell !! hexahedron cell type
    type(vtkcell_list), dimension(n_cells)   :: cell_list       !! full list of all cells
    character(len=10), dimension(n_params_to_write), parameter :: cell_dataname = &
        & [ 'cellids   ' ]
    character(len=15), dimension(n_params_to_write), parameter :: point_dataname = &
        & [ 'temperature(k) ' ]

    call voxel_cells(1)%setup ( [ 0, 1, 2, 3, 4, 5, 6, 7 ] )
    call voxel_cells(2)%setup ( [ 4, 5, 6, 7, 9, 10, 13, 14 ] )
    call voxel_cells(3)%setup ( [ 8, 9, 12, 13, 16, 17, 20, 21 ] )
    call voxel_cells(4)%setup ( [ 9, 10, 13, 14, 17, 18, 21, 22 ] )
    call hexahedron_cell%setup ( [ 10, 11, 15, 14, 18, 19, 23, 22 ] )

    ! cell_list(1)%cell = voxel_cells(1) !! works for intel 18.5, not for gfortran-8.2
    ! cell_list(2)%cell = voxel_cells(2)
    ! cell_list(3)%cell = voxel_cells(3)
    ! cell_list(4)%cell = voxel_cells(4)
    ! cell_list(5)%cell = hexahedron_cell
    allocate(cell_list(1)%cell,source=voxel_cells(1)) !! workaround for gfortran-8.2
    allocate(cell_list(2)%cell,source=voxel_cells(2))
    allocate(cell_list(3)%cell,source=voxel_cells(3))
    allocate(cell_list(4)%cell,source=voxel_cells(4))
    allocate(cell_list(5)%cell,source=hexahedron_cell)

    call t_shape%init (points=points, cell_list=cell_list)

    do t = 1, n_steps
        cell_vals(:,1)  = real(cellid(:)); write(t_char,'(i0)') t
        point_vals(:,1) = temp_default + (t-1) * temp_increment * temp_norm

        do i = 1, n_params_to_write
            ! cell values
            if (.not. allocated(cell_vals_to_write(i)%attribute))then
                allocate(scalar::cell_vals_to_write(i)%attribute)
            end if
            call cell_vals_to_write(i)%attribute%init (trim(cell_dataname(i)), numcomp=1, real1d=cell_vals(:,i))
            ! point values
            if (.not. allocated(point_vals_to_write(i)%attribute))then
                allocate(scalar::point_vals_to_write(i)%attribute)
            end if
            call point_vals_to_write(i)%attribute%init (trim(point_dataname(i)), numcomp=1, real1d=point_vals(:,i))
        end do

        call vtk_parallel_write (t_shape, image=this_image(), celldatasets=cell_vals_to_write, pointdatasets=point_vals_to_write, &
            &                    unit=unit, filename=filename, multiple_io=.true.)
        call vtk_parallel_write (this_image())
    end do

    write(*,*) 'Finished'

end program parallel_t_shape_test
