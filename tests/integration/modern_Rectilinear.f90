PROGRAM modern_Rectilinear_test
    USE Precision,      ONLY : i4k, r8k
    USE vtk_datasets,   ONLY : rectlnr_grid
    USE vtk_attributes, ONLY : scalar, attributes
    USE vtk,            ONLY : vtk_serial_write
    IMPLICIT NONE
    !! author: Ian Porter
    !! date: 05/20/2019
    !!
    !! This is a test of a cube geometry using a rectilinear grid
    !!
    INTEGER(i4k), PARAMETER     :: n_params_to_write = 3
    TYPE (rectlnr_grid)         :: cube
    TYPE (attributes), DIMENSION(n_params_to_write) :: vals_to_write
    INTEGER(i4k)                :: i = 0_i4k
    INTEGER(i4k),     PARAMETER :: n_x = 11, n_y = 6, n_z = 3, unit = 20
    REAL(r8k)                   :: j = 0.0_r8k
    REAL(r8k),        PARAMETER :: temp_val = 555.0_r8k
    CHARACTER(LEN=*), PARAMETER :: filename = 'cube.vtr'
    CHARACTER(LEN=*), PARAMETER :: title    = 'Testing of cube geometry'
    INTEGER(i4k), DIMENSION(3)  :: dims
    REAL(r8k), DIMENSION(n_x), PARAMETER :: x_coords = &
      & [ 0.1_r8k, 0.2_r8k, 0.3_r8k, 0.4_r8k, 0.5_r8k, 0.6_r8k, 0.7_r8k, 0.8_r8k, 0.9_r8k, 1.0_r8k, 1.1_r8k ]
    REAL(r8k), DIMENSION(n_y), PARAMETER :: y_coords = &
      & [ 0.2_r8k, 0.4_r8k, 0.6_r8k, 0.8_r8k, 1.0_r8k, 1.2_r8k ]
    REAL(r8k), DIMENSION(n_z), PARAMETER :: z_coords = &
      & [ 0.5_r8k, 1.0_r8k, 1.5_r8k ]
    REAL(r8k), DIMENSION(n_x*n_y*n_z,1:n_params_to_write) :: vals
    CHARACTER(LEN=20), DIMENSION(n_params_to_write), PARAMETER :: dataname = &
      & [ 'Temperature_(K)     ', 'Pressure_(Pa)       ', 'Stress_(Pa)         ' ]

    vals(1,:) = temp_val
    DO i = 2, SIZE(vals,DIM=1)
        IF (i <= SIZE(vals) / 2) THEN
            vals(i,1) = vals(i-1,1) + 2.0_r8k              !! Temperature
        ELSE
            vals(i,1) = vals(i-1,1) - 2.0_r8k              !! Temperature
        END IF
        j = j + 1.0_r8k
        vals(i,2) = vals(i-1,2) + MAX(50.0_r8k, j)         !! Pressure
        vals(i,3) = vals(i-1,3) + SQRT(REAL(i))            !! Stress
    END DO
    dims = [ n_x, n_y, n_z ]
    CALL cube%init (dims=dims, x_coords=x_coords, y_coords=y_coords, z_coords=z_coords)
    DO i = 1, n_params_to_write
        IF (.NOT. ALLOCATED(vals_to_write(i)%attribute))THEN
            ALLOCATE(scalar::vals_to_write(i)%attribute)
        END IF
        CALL vals_to_write(i)%attribute%init (dataname(i), numcomp=1, real1d=vals(:,i))
    END DO

    CALL vtk_serial_write (cube, pointdatasets=vals_to_write, unit=unit, filename=filename, title=title)
                                                           !! This tests a full 1-time write

    CALL vtk_serial (cube)

    CALL vtk_serial (vals_to_write(1))

    CALL vtk_serial ([vals_to_write(2:3)])

    WRITE(*,*) 'Finished'

END PROGRAM modern_Rectilinear_test
