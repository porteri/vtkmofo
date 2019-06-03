PROGRAM rectangular_prism_test
    USE Precision
    USE vtk_datasets,   ONLY : struct_pts
    USE vtk_attributes, ONLY : scalar, attributes
    USE vtk,            ONLY : vtk_legacy_write
    IMPLICIT NONE
    !! author: Ian Porter
    !! date: 12/28/2017
    !!
    !! This is a test of a rectangular_prism geometry using structured points
    !!
    INTEGER(i4k), PARAMETER     :: n_params_to_write = 3
    TYPE (struct_pts)           :: rectangular_prism
    TYPE (attributes), DIMENSION(n_params_to_write) :: vals_to_write
    INTEGER(i4k)                :: i = 0_i4k
    INTEGER(i4k),     PARAMETER :: n_x = 11, n_y = 6, n_z = 3, unit = 20
    REAL(r8k)                   :: j = 0.0_r8k
    REAL(r8k),        PARAMETER :: temp_val = 555.0_r8k
    CHARACTER(LEN=*), PARAMETER :: filename = 'rectangular_prism.vtk'
    CHARACTER(LEN=*), PARAMETER :: title    = 'Testing of rectangular_prism geometry'
    INTEGER(i4k), DIMENSION(3)  :: dims
    REAL(r8k), DIMENSION(3), PARAMETER :: origin  = &
      & [ 0.0_r8k, 0.0_r8k, 0.0_r8k ]
    REAL(r8k), DIMENSION(3), PARAMETER :: spacing = &
      & [ 0.3_r8k, 0.2_r8k, 0.1_r8k ]
    REAL(r8k), DIMENSION(n_x*n_y*n_z,1:n_params_to_write) :: vals
    CHARACTER(LEN=20), DIMENSION(n_params_to_write), PARAMETER :: dataname = &
      & [ 'Temperature_(K)     ', 'Pressure_(Pa)       ', 'Stress_(Pa)         ' ]

    vals(1,:) = temp_val
    DO i = 2, SIZE(vals,DIM=1)
        IF (i <= SIZE(vals) / 2) THEN
            vals(i,1) = vals(i-1,1) + 2.0_r8k          !! Temperature
        ELSE
            vals(i,1) = vals(i-1,1) - 2.0_r8k          !! Temperature
        END IF
        j = j + 1.0_r8k
        vals(i,2) = vals(i-1,2) + MAX(50.0_r8k, j)     !! Pressure
        vals(i,3) = vals(i-1,3) + SQRT(REAL(i))        !! Stress
    END DO
    dims = (/ n_x, n_y, n_z /)
    CALL rectangular_prism%init (dims=dims, origin=origin, spacing=spacing)
    DO i = 1, n_params_to_write
        IF (.NOT. ALLOCATED(vals_to_write(i)%attribute))THEN
            ALLOCATE(scalar::vals_to_write(i)%attribute)
            vals_to_write(1)%n = SIZE(vals(:,1))
        END IF
        CALL vals_to_write(i)%attribute%init (TRIM(dataname(i)), numcomp=1, real1d=vals(:,i))
    END DO

    CALL vtk_legacy_write (rectangular_prism, pointdatasets=vals_to_write, unit=unit, filename=filename, title=title)

    WRITE(*,*) 'Finished'

END PROGRAM rectangular_prism_test
