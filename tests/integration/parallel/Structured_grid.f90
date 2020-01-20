program parallel_structured_grid_test
    use precision,      only : i4k, r8k
    use vtk_datasets,   only : struct_grid
    use vtk_attributes, only : scalar, attribute, attributes
    use vtk,            only : vtk_parallel_write
    implicit none
    !! author: Ian Porter
    !! date: 01/18/2020
    !!
    !! this is a test of a cylindrical geometry using a rectilinear grid
    !!
    integer(i4k), parameter     :: n_params_to_write = 2
    type (struct_grid)          :: cylinder
    type (attributes), dimension(n_params_to_write) :: point_data
    type (scalar)               :: cell_data
    integer(i4k)                :: i, j, k, z, t, cnt = 1
    integer(i4k),     parameter :: n_x = 19, n_y = 1, n_z = 4, n_steps = 2
    character(len=*), parameter :: filename = 'parallel_structured_grid'
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
    integer(i4k), dimension(max(n_x-1,1)*max(n_y-1,1)*max(n_z-1,1)), parameter :: mat_id = &
        &  [ 5, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 3, 3, 3, &
        &    5, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 3, 3, 3, &
        &    5, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 3, 3, 3 ]
    real(r8k), dimension(1:3,n_x*n_y*n_z) :: points
    real(r8k), dimension(n_x*n_y*n_z)     :: pressure
    real(r8k), dimension(n_x*n_y*n_z,1:n_params_to_write) :: vals
    character(len=20), dimension(n_params_to_write), parameter :: dataname = &
        & [ 'temperature (Kelvin)', 'pressure (Pa)       ' ]

    !! Fake simulation of multiple images
    WRITE(0,*) num_images()
    do t = 1, n_steps
        do z = 1, num_images()
            cnt = 1
            do k = 1, n_z
                do j = 1, n_y
                    do i = 1, n_x
                        points(1:3,cnt) = [x_vals(i), y_vals(j), z_vals(k)]
                        pressure(cnt) = real(cnt)
                        cnt = cnt + 1
                    end do
                end do
            end do

            vals(:,1) = temp(:)
            vals(:,2) = pressure(:)
            dims = [ n_x, n_y, n_z ]

            call cylinder%init (dims=dims, points=points)

            do i = 1, n_params_to_write
                if (.not. allocated(point_data(i)%attribute))then
                    allocate(scalar::point_data(i)%attribute)
                end if
                call point_data(i)%attribute%init (dataname(i), numcomp=1, real1d=vals(:,i))
            end do

            !! dummy "material" information
            call cell_data%init ('material id', numcomp=1, int1d=mat_id)

            call vtk_parallel_write (cylinder, image=this_image(), filename=filename, &
                &                    pointdatasets=point_data, celldata=cell_data)

        end do

        if (this_image() == 1) call vtk_parallel_write(num_images())  !! This is the finalizer
    end do

    write(*,*) 'Finished'

end program parallel_structured_grid_test
