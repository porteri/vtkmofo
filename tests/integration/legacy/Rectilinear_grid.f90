program legacy_rectilinear_test
    use precision,      only : i4k, r8k
    use vtk_datasets,   only : rectlnr_grid
    use vtk_attributes, only : scalar, attributes
    use vtk,            only : vtk_legacy_write
    implicit none
    !! author: Ian Porter
    !! date: 12/20/2017
    !!
    !! this is a test of a cube geometry using a rectilinear grid
    !!
    integer(i4k), parameter     :: n_params_to_write = 3
    type (rectlnr_grid)         :: cube
    type (attributes), dimension(n_params_to_write) :: vals_to_write
    integer(i4k)                :: i = 0_i4k
    integer(i4k),     parameter :: n_x = 11, n_y = 6, n_z = 3, unit = 20
    real(r8k)                   :: j = 0.0_r8k
    real(r8k),        parameter :: temp_val = 555.0_r8k
    character(len=*), parameter :: filename = 'legacy_rectlnr_grid'
    character(len=*), parameter :: title    = 'testing of cube geometry'
    integer(i4k), dimension(3)  :: dims = [ n_x, n_y, n_z ]
    real(r8k), dimension(n_x), parameter :: x_coords = &
        & [ 0.1_r8k, 0.2_r8k, 0.3_r8k, 0.4_r8k, 0.5_r8k, 0.6_r8k, 0.7_r8k, 0.8_r8k, 0.9_r8k, 1.0_r8k, 1.1_r8k ]
    real(r8k), dimension(n_y), parameter :: y_coords = &
        & [ 0.2_r8k, 0.4_r8k, 0.6_r8k, 0.8_r8k, 1.0_r8k, 1.2_r8k ]
    real(r8k), dimension(n_z), parameter :: z_coords = &
        & [ 0.5_r8k, 1.0_r8k, 1.5_r8k ]
    real(r8k), dimension(n_x*n_y*n_z,1:n_params_to_write) :: vals
    character(len=20), dimension(n_params_to_write), parameter :: dataname = &
        & [ 'temperature_(k)     ', 'pressure_(pa)       ', 'stress_(pa)         ' ]

    vals(1,:) = temp_val
    do i = 2, size(vals,dim=1)
        if (i <= size(vals) / 2) then
            vals(i,1) = vals(i-1,1) + 2.0_r8k          !! temperature
        else
            vals(i,1) = vals(i-1,1) - 2.0_r8k          !! temperature
        end if
        j = j + 1.0_r8k
        vals(i,2) = vals(i-1,2) + max(50.0_r8k, j)     !! pressure
        vals(i,3) = vals(i-1,3) + sqrt(real(i))        !! stress
    end do
    call cube%init (dims=dims, x_coords=x_coords, y_coords=y_coords, z_coords=z_coords)
    do i = 1, n_params_to_write
        if (.not. allocated(vals_to_write(i)%attribute))then
            allocate(scalar::vals_to_write(i)%attribute)
        end if
        call vals_to_write(i)%attribute%init (dataname(i), numcomp=1, real1d=vals(:,i))
    end do

    call vtk_legacy_write (cube, pointdatasets=vals_to_write, unit=unit, filename=filename, title=title)

    write(*,*) 'Finished'

end program legacy_rectilinear_test
