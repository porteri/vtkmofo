PROGRAM Cylinder_test
    USE Precision
    USE vtk_datasets,   ONLY : struct_grid
    USE vtk_attributes, ONLY : scalar, attributes
    USE vtk,            ONLY : vtk_legacy_write
    IMPLICIT NONE
    !! author: Ian Porter
    !! date: 12/20/2017
    !!
    !! This is a test of a cylindrical geometry using a rectilinear grid
    !!
    INTEGER(i4k), PARAMETER     :: n_params_to_write = 1
    TYPE (struct_grid)          :: cylinder
    TYPE (attributes), DIMENSION(n_params_to_write) :: vals_to_write
    INTEGER(i4k)                :: i, j, k, cnt = 1
    INTEGER(i4k),     PARAMETER :: n_x = 19, n_y = 1, n_z = 4, unit = 20
    CHARACTER(LEN=*), PARAMETER :: filename = 'cylinder.vtk'
    CHARACTER(LEN=*), PARAMETER :: title    = 'Testing of cylindrical geometry'
    INTEGER(i4k), DIMENSION(3)  :: dims
    REAL(r8k), DIMENSION(n_x), PARAMETER :: x_vals = &
      & [ 0.00E+00_r8k, 8.03E-04_r8k, 1.51E-03_r8k, 2.12E-03_r8k, 2.64E-03_r8k, &
      &   3.08E-03_r8k, 3.45E-03_r8k, 3.75E-03_r8k, 3.99E-03_r8k, 4.18E-03_r8k, &
      &   4.32E-03_r8k, 4.42E-03_r8k, 4.49E-03_r8k, 4.53E-03_r8k, 4.56E-03_r8k, &
      &   4.56E-03_r8k, 4.56E-03_r8k, 4.65E-03_r8k, 5.38E-03_r8k ]
    REAL(r8k), DIMENSION(n_y), PARAMETER :: y_vals = &
      & [ 0.00E+00_r8k ]
    REAL(r8k), DIMENSION(n_z), PARAMETER :: z_vals = &
      & [ 2.50E-03_r8k, 5.00E-03_r8k, 7.50E-03_r8k, 1.00E-03_r8k ]
    REAL(r8k), DIMENSION(n_x*n_y*n_z), PARAMETER :: temp = &
      & [ 1.27E+03_r8k, 1.25E+03_r8k, 1.21E+03_r8k, 1.15E+03_r8k, 1.08E+03_r8k, &
      &   1.02E+03_r8k, 9.63E+02_r8k, 9.11E+02_r8k, 8.67E+02_r8k, 8.32E+02_r8k, &
      &   8.04E+02_r8k, 7.84E+02_r8k, 7.70E+02_r8k, 7.61E+02_r8k, 7.56E+02_r8k, &
      &   7.55E+02_r8k, 7.54E+02_r8k, 5.68E+02_r8k, 5.28E+02_r8k, 1.40E+03_r8k, &
      &   1.37E+03_r8k, 1.31E+03_r8k, 1.23E+03_r8k, 1.14E+03_r8k, 1.06E+03_r8k, &
      &   9.83E+02_r8k, 9.20E+02_r8k, 8.68E+02_r8k, 8.27E+02_r8k, 7.96E+02_r8k, &
      &   7.74E+02_r8k, 7.60E+02_r8k, 7.51E+02_r8k, 7.46E+02_r8k, 7.44E+02_r8k, &
      &   7.44E+02_r8k, 5.68E+02_r8k, 5.28E+02_r8k, 1.39E+03_r8k, 1.36E+03_r8k, &
      &   1.30E+03_r8k, 1.22E+03_r8k, 1.14E+03_r8k, 1.06E+03_r8k, 9.82E+02_r8k, &
      &   9.20E+02_r8k, 8.69E+02_r8k, 8.28E+02_r8k, 7.97E+02_r8k, 7.75E+02_r8k, &
      &   7.60E+02_r8k, 7.51E+02_r8k, 7.47E+02_r8k, 7.45E+02_r8k, 7.45E+02_r8k, &
      &   5.68E+02_r8k, 5.28E+02_r8k, 1.39E+03_r8k, 1.36E+03_r8k, 1.30E+03_r8k, &
      &   1.22E+03_r8k, 1.14E+03_r8k, 1.06E+03_r8k, 9.83E+02_r8k, 9.20E+02_r8k, &
      &   8.69E+02_r8k, 8.28E+02_r8k, 7.97E+02_r8k, 7.75E+02_r8k, 7.60E+02_r8k, &
      &   7.52E+02_r8k, 7.47E+02_r8k, 7.45E+02_r8k, 7.45E+02_r8k, 5.68E+02_r8k, &
      &   5.28E+02_r8k ]
    REAL(r8k), DIMENSION(1:3,n_x*n_y*n_z)    :: points
    REAL(r8k), DIMENSION(n_x*n_y*n_z,1:n_params_to_write) :: vals
    CHARACTER(LEN=20), DIMENSION(n_params_to_write), PARAMETER :: dataname = &
      & [ 'Temperature_(K)     ' ]

    cnt = 1
    DO k = 1, n_z
        DO j = 1, n_y
            DO i = 1, n_x
                points(1,cnt) = x_vals(i)
                points(2,cnt) = y_vals(j)
                points(3,cnt) = z_vals(k)
                cnt = cnt + 1
            END DO
        END DO
    END DO

    vals(:,1) = temp(:)
    dims = [ n_x, n_y, n_z ]

    CALL cylinder%init (dims=dims, points=points)

    DO i = 1, n_params_to_write
        IF (.NOT. ALLOCATED(vals_to_write(i)%attribute))THEN
            ALLOCATE(scalar::vals_to_write(i)%attribute)
            vals_to_write(1)%n = SIZE(vals(:,1))
        END IF
        CALL vals_to_write(i)%attribute%init (dataname(i), numcomp=1, values1d=vals(:,i))
    END DO

    CALL vtk_legacy_write (unit, cylinder, pointdatasets=vals_to_write)

    WRITE(*,*) 'Finished'

END PROGRAM Cylinder_test
