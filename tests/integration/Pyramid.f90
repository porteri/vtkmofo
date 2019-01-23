PROGRAM pyramid_test
    USE Precision
    USE vtk_datasets,   ONLY : polygonal_data
    USE vtk_attributes, ONLY : scalar, attributes
    USE vtk_cells,      ONLY : polygon
    USE vtk,            ONLY : vtk_legacy_write
    IMPLICIT NONE
    !>@brief
    !> This is a test of a pyramid geometry using polygonal data
    !>@author
    !> Ian Porter
    !>@date
    !> 12/31/2017

    INTEGER(i4k), PARAMETER     :: n_params_to_write = 1
    TYPE (polygonal_data)       :: pyramid
    TYPE (attributes), DIMENSION(n_params_to_write) :: point_vals_to_write, cell_vals_to_write
    INTEGER(i4k)                :: i = 0_i4k
    INTEGER(i4k),     PARAMETER :: n_x = 3, n_y = 3, n_z = 3, n_faces = 20, unit = 20
    CHARACTER(LEN=*), PARAMETER :: filename = 'pyramid.vtk'
    CHARACTER(LEN=*), PARAMETER :: title    = 'Testing of pyramid geometry'
    INTEGER(i4k), DIMENSION(3)  :: dims
    REAL(r8k), DIMENSION(n_faces,1:n_params_to_write)     :: cell_vals
    REAL(r8k), DIMENSION(n_x*n_y*n_z,1:n_params_to_write) :: point_vals
    REAL(r8k), DIMENSION(3,n_x*n_y*n_z), PARAMETER :: points = RESHAPE ( &
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
    REAL(r8k), DIMENSION(n_x*n_y*n_z), PARAMETER :: power = &
      & [ 100.0_r8k, 100.0_r8k, 100.0_r8k, 100.0_r8k, 100.0_r8k, 100.0_r8k, 100.0_r8k, 100.0_r8k, 100.0_r8k, &
      &   100.0_r8k, 100.0_r8k, 100.0_r8k, 100.0_r8k, 200.0_r8k, 100.0_r8k, 100.0_r8k, 100.0_r8k, 100.0_r8k, &
      &   100.0_r8k, 100.0_r8k, 100.0_r8k, 100.0_r8k, 100.0_r8k, 100.0_r8k, 100.0_r8k, 100.0_r8k, 100.0_r8k ]
    INTEGER(i4k), DIMENSION(n_faces), PARAMETER :: cellID = &
      & [ 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3 ]
    TYPE(polygon), DIMENSION(n_faces) :: polygon_faces
    CHARACTER(LEN=10), DIMENSION(n_params_to_write), PARAMETER :: cell_dataname = &
      & [ 'cellIDs   ' ]
    CHARACTER(LEN=10), DIMENSION(n_params_to_write), PARAMETER :: point_dataname = &
      & [ 'Power_(W) ' ]

    CALL polygon_faces(1)%setup ( (/ 0, 1, 4, 3 /) )
    CALL polygon_faces(2)%setup ( (/ 1, 2, 5, 4 /) )
    CALL polygon_faces(3)%setup ( (/ 3, 4, 7, 6 /) )
    CALL polygon_faces(4)%setup ( (/ 4, 5, 8, 7 /) )
    CALL polygon_faces(5)%setup ( (/ 9, 10, 13, 12 /) )
    CALL polygon_faces(6)%setup ( (/ 10, 11, 14, 13 /) )
    CALL polygon_faces(7)%setup ( (/ 12, 13, 16, 15 /) )
    CALL polygon_faces(8)%setup ( (/ 13, 14, 17, 16 /) )
    CALL polygon_faces(9)%setup ( (/ 18, 19, 22, 21 /) )
    CALL polygon_faces(10)%setup ( (/ 19, 20, 23, 22 /) )
    CALL polygon_faces(11)%setup ( (/ 21, 22, 25, 24 /) )
    CALL polygon_faces(12)%setup ( (/ 22, 23, 26, 25 /) )
    CALL polygon_faces(13)%setup ( (/ 0, 1, 10, 19, 18, 9 /) )
    CALL polygon_faces(14)%setup ( (/ 1, 2, 11, 20, 19, 10 /) )
    CALL polygon_faces(15)%setup ( (/ 0, 3, 12, 21, 18, 9 /) )
    CALL polygon_faces(16)%setup ( (/ 3, 6, 15, 24, 21, 12 /) )
    CALL polygon_faces(17)%setup ( (/ 6, 7, 16, 25, 24, 15 /) )
    CALL polygon_faces(18)%setup ( (/ 7, 8, 17, 26, 25, 16 /) )
    CALL polygon_faces(19)%setup ( (/ 8, 5, 14, 23, 26, 17 /) )
    CALL polygon_faces(20)%setup ( (/ 5, 2, 11, 20, 23, 14 /) )

    cell_vals(:,1)  = REAL(cellID(:))
    point_vals(:,1) = power(:)

    dims = (/ n_x, n_y, n_z /)
    CALL pyramid%init (points=points, polygons=polygon_faces)
    DO i = 1, n_params_to_write
        ! Cell values
        IF (.NOT. ALLOCATED(cell_vals_to_write(i)%attribute))THEN
            ALLOCATE(scalar::cell_vals_to_write(i)%attribute)
            cell_vals_to_write(1)%n = SIZE(cell_vals(:,1))
        END IF
        CALL cell_vals_to_write(i)%attribute%setup (TRIM(cell_dataname(i)), numcomp=1, values1d=cell_vals(:,i))
        ! Point values
        IF (.NOT. ALLOCATED(point_vals_to_write(i)%attribute))THEN
            ALLOCATE(scalar::point_vals_to_write(i)%attribute)
            point_vals_to_write(1)%n = SIZE(point_vals(:,1))
        END IF
        CALL point_vals_to_write(i)%attribute%setup (TRIM(point_dataname(i)), numcomp=1, values1d=point_vals(:,i))
    END DO

    CALL vtk_legacy_write (unit, pyramid, celldatasets=cell_vals_to_write, pointdatasets=point_vals_to_write, &
      &                    filename=filename, title=title)

    WRITE(*,*) 'Finished'

END PROGRAM pyramid_test
