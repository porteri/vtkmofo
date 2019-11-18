program pyramid_test
    use precision,      only : i4k, r8k
    use vtk_datasets,   only : polygonal_data
    use vtk_attributes, only : scalar, attributes
    use vtk_cells,      only : polygon
    use vtk,            only : vtk_legacy_write
    implicit none
    !! author: Ian Porter
    !! date: 12/31/2017
    !!
    !! this is a test of a pyramid geometry using polygonal data
    !!
    integer(i4k), parameter     :: n_params_to_write = 1
    type (polygonal_data)       :: pyramid
    type (attributes), dimension(n_params_to_write) :: point_vals_to_write, cell_vals_to_write
    integer(i4k)                :: i = 0_i4k
    integer(i4k),     parameter :: n_x = 3, n_y = 3, n_z = 3, n_faces = 20
    !character(len=*), parameter :: filename = 'legacy_polygonal_data' !! will use default out.vtk filename
    integer(i4k), dimension(3)  :: dims
    real(r8k), dimension(n_faces,1:n_params_to_write)     :: cell_vals
    real(r8k), dimension(n_x*n_y*n_z,1:n_params_to_write) :: point_vals
    real(r8k), dimension(3,n_x*n_y*n_z), parameter :: points = reshape ( &
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
        &   0.625_r8k, 0.625_r8k, 0.75_r8k ], [3,n_x*n_y*n_z] )
    real(r8k), dimension(n_x*n_y*n_z), parameter :: power = &
        & [ 100.0_r8k, 100.0_r8k, 100.0_r8k, 100.0_r8k, 100.0_r8k, 100.0_r8k, 100.0_r8k, 100.0_r8k, 100.0_r8k, &
        &   100.0_r8k, 100.0_r8k, 100.0_r8k, 100.0_r8k, 200.0_r8k, 100.0_r8k, 100.0_r8k, 100.0_r8k, 100.0_r8k, &
        &   100.0_r8k, 100.0_r8k, 100.0_r8k, 100.0_r8k, 100.0_r8k, 100.0_r8k, 100.0_r8k, 100.0_r8k, 100.0_r8k ]
    integer(i4k), dimension(n_faces), parameter :: cellid = &
        & [ 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3 ]
    type(polygon), dimension(n_faces) :: polygon_faces
    character(len=10), dimension(n_params_to_write), parameter :: cell_dataname = &
        & [ 'cellids   ' ]
    character(len=10), dimension(n_params_to_write), parameter :: point_dataname = &
        & [ 'power_(w) ' ]

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

    cell_vals(:,1)  = real(cellid(:))
    point_vals(:,1) = power(:)

    dims = [ n_x, n_y, n_z ]
    call pyramid%init (points=points, polygons=polygon_faces)
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

    call vtk_legacy_write (pyramid, celldatasets=cell_vals_to_write, pointdatasets=point_vals_to_write)

    write(*,*) 'Finished'

end program pyramid_test
