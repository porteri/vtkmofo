program serial_rectangular_prism_test
    use precision,      only : i4k, r8k
    use vtk_datasets,   only : struct_pts
    use vtk_attributes, only : scalar, attributes
    use vtk,            only : vtk_serial_write
    implicit none
    !! author: Ian Porter
    !! date: 12/28/2017
    !!
    !! this is a test of a rectangular_prism geometry using structured points
    !!
    integer(i4k), parameter     :: n_params_to_write = 3
    type (struct_pts)           :: rectangular_prism
    type (attributes), dimension(n_params_to_write) :: vals_to_write
    integer(i4k)                :: i = 0_i4k
    integer(i4k),     parameter :: n_x = 11, n_y = 6, n_z = 3, unit = 20
    real(r8k)                   :: j = 0.0_r8k
    real(r8k),        parameter :: temp_val = 555.0_r8k
    character(len=*), parameter :: filename = 'serial_struct_pts'
    integer(i4k), dimension(3)  :: dims
    real(r8k), dimension(3), parameter :: origin  = &
        & [ 0.0_r8k, 0.0_r8k, 0.0_r8k ]
    real(r8k), dimension(3), parameter :: spacing = &
        & [ 0.3_r8k, 0.2_r8k, 0.1_r8k ]
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
    dims = [ n_x, n_y, n_z ]
    call rectangular_prism%init (dims=dims, origin=origin, spacing=spacing)
    do i = 1, n_params_to_write
        if (.not. allocated(vals_to_write(i)%attribute))then
            allocate(scalar::vals_to_write(i)%attribute)
        end if
        call vals_to_write(i)%attribute%init (trim(dataname(i)), numcomp=1, real1d=vals(:,i))
    end do

    call vtk_serial_write (rectangular_prism, pointdatasets=vals_to_write, unit=unit, filename=filename)

    write(*,*) 'Finished'

end program serial_rectangular_prism_test
