program cylinder_test
    use precision,      only : i4k, r8k
    use vtk_datasets,   only : struct_grid
    use vtk_attributes, only : scalar, attributes
    use vtk,            only : vtk_legacy_write
    implicit none
    !! author: Ian Porter
    !! date: 12/20/2017
    !!
    !! this is a test of a cylindrical geometry using a rectilinear grid
    !!
    integer(i4k), parameter     :: n_params_to_write = 1
    type (struct_grid)          :: cylinder
    type (attributes), dimension(n_params_to_write) :: vals_to_write
    integer(i4k)                :: i, j, k, cnt = 1
    integer(i4k),     parameter :: n_x = 19, n_y = 1, n_z = 4, unit = 20
    character(len=*), parameter :: filename = 'legacy_struct_grid.vtk' !! the .vtk should get stripped by vtkmofo
    character(len=*), parameter :: title    = 'testing of cylindrical geometry'
    integer(i4k), dimension(3)  :: dims
    real(r8k), dimension(n_x), parameter :: x_vals = &
        & [ 0.00e+00_r8k, 8.03e-04_r8k, 1.51e-03_r8k, 2.12e-03_r8k, 2.64e-03_r8k, &
        &   3.08e-03_r8k, 3.45e-03_r8k, 3.75e-03_r8k, 3.99e-03_r8k, 4.18e-03_r8k, &
        &   4.32e-03_r8k, 4.42e-03_r8k, 4.49e-03_r8k, 4.53e-03_r8k, 4.56e-03_r8k, &
        &   4.56e-03_r8k, 4.56e-03_r8k, 4.65e-03_r8k, 5.38e-03_r8k ]
    real(r8k), dimension(n_y), parameter :: y_vals = &
        & [ 0.00e+00_r8k ]
    real(r8k), dimension(n_z), parameter :: z_vals = &
        & [ 2.50e-03_r8k, 5.00e-03_r8k, 7.50e-03_r8k, 1.00e-03_r8k ]
    real(r8k), dimension(n_x*n_y*n_z), parameter :: temp = &
        & [ 1.27e+03_r8k, 1.25e+03_r8k, 1.21e+03_r8k, 1.15e+03_r8k, 1.08e+03_r8k, &
        &   1.02e+03_r8k, 9.63e+02_r8k, 9.11e+02_r8k, 8.67e+02_r8k, 8.32e+02_r8k, &
        &   8.04e+02_r8k, 7.84e+02_r8k, 7.70e+02_r8k, 7.61e+02_r8k, 7.56e+02_r8k, &
        &   7.55e+02_r8k, 7.54e+02_r8k, 5.68e+02_r8k, 5.28e+02_r8k, 1.40e+03_r8k, &
        &   1.37e+03_r8k, 1.31e+03_r8k, 1.23e+03_r8k, 1.14e+03_r8k, 1.06e+03_r8k, &
        &   9.83e+02_r8k, 9.20e+02_r8k, 8.68e+02_r8k, 8.27e+02_r8k, 7.96e+02_r8k, &
        &   7.74e+02_r8k, 7.60e+02_r8k, 7.51e+02_r8k, 7.46e+02_r8k, 7.44e+02_r8k, &
        &   7.44e+02_r8k, 5.68e+02_r8k, 5.28e+02_r8k, 1.39e+03_r8k, 1.36e+03_r8k, &
        &   1.30e+03_r8k, 1.22e+03_r8k, 1.14e+03_r8k, 1.06e+03_r8k, 9.82e+02_r8k, &
        &   9.20e+02_r8k, 8.69e+02_r8k, 8.28e+02_r8k, 7.97e+02_r8k, 7.75e+02_r8k, &
        &   7.60e+02_r8k, 7.51e+02_r8k, 7.47e+02_r8k, 7.45e+02_r8k, 7.45e+02_r8k, &
        &   5.68e+02_r8k, 5.28e+02_r8k, 1.39e+03_r8k, 1.36e+03_r8k, 1.30e+03_r8k, &
        &   1.22e+03_r8k, 1.14e+03_r8k, 1.06e+03_r8k, 9.83e+02_r8k, 9.20e+02_r8k, &
        &   8.69e+02_r8k, 8.28e+02_r8k, 7.97e+02_r8k, 7.75e+02_r8k, 7.60e+02_r8k, &
        &   7.52e+02_r8k, 7.47e+02_r8k, 7.45e+02_r8k, 7.45e+02_r8k, 5.68e+02_r8k, &
        &   5.28e+02_r8k ]
    real(r8k), dimension(1:3,n_x*n_y*n_z)    :: points
    real(r8k), dimension(n_x*n_y*n_z,1:n_params_to_write) :: vals
    character(len=20), dimension(n_params_to_write), parameter :: dataname = &
        & [ 'temperature_(k)     ' ]

    cnt = 1
    do k = 1, n_z
        do j = 1, n_y
            do i = 1, n_x
                points(1,cnt) = x_vals(i)
                points(2,cnt) = y_vals(j)
                points(3,cnt) = z_vals(k)
                cnt = cnt + 1
            end do
        end do
    end do

    vals(:,1) = temp(:)
    dims = [ n_x, n_y, n_z ]

    call cylinder%init (dims=dims, points=points)

    do i = 1, n_params_to_write
        if (.not. allocated(vals_to_write(i)%attribute))then
            allocate(scalar::vals_to_write(i)%attribute)
        end if
        call vals_to_write(i)%attribute%init (dataname(i), numcomp=1, real1d=vals(:,i))
    end do

    call vtk_legacy_write (cylinder, unit=unit, pointdatasets=vals_to_write)

    write(*,*) 'Finished'

end program cylinder_test
