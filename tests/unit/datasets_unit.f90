MODULE vtk_datasets_unit_tests
    USE Kinds
    USE vtk_datasets, ONLY : ASCII, Binary
    IMPLICIT NONE
    !>@brief
    !> Unit testing for datasets
    !>@author
    !> Ian Porter
    !>@date
    !> 12/17/2017
    PRIVATE
    PUBLIC :: vtk_datasets_unit
! Generic information
    INTEGER(i4k), PARAMETER :: n_types = 5
    INTEGER(i4k), PARAMETER :: vtk_unit = 20
    INTEGER(i4k), PARAMETER :: n_x = 11, n_y = 6, n_z = 3
    INTEGER(i4k), DIMENSION(n_types), PARAMETER :: filetype = &
      & [ ASCII, ASCII, ASCII, ASCII, ASCII ]
    CHARACTER(LEN=*), DIMENSION(n_types), PARAMETER :: filename = &
      & [ 'struct_pts.vtk      ', &
      &   'struct_grid.vtk     ', &
      &   'rectlnr_grid.vtk    ', &
      &   'polygonal_data.vtk  ', &
      &   'unstruct_grid.vtk   ' ]
! Structured points
    INTEGER(i4k), DIMENSION(3), PARAMETER :: dims    = &
      & [ n_x, n_y, n_z ]
    REAL(r8k), DIMENSION(3), PARAMETER    :: origin  = &
      & [ 0.0_r8k, 0.0_r8k, 0.0_r8k ]
    REAL(r8k), DIMENSION(3), PARAMETER    :: spacing = &
      & [ 0.1_r8k, 0.2_r8k, 0.5_r8k ]
! Structured grid
    REAL(r8k), DIMENSION(3,n_x*n_y*n_z), PARAMETER :: points = RESHAPE ( & !! [x, y, z]
      & [ 0.0_r8k, 0.0_r8k, 0.0_r8k, &
      &   0.0_r8k, 0.0_r8k, 0.5_r8k, &
      &   0.0_r8k, 0.0_r8k, 1.0_r8k, &
      &   0.0_r8k, 0.2_r8k, 0.0_r8k, &
      &   0.0_r8k, 0.2_r8k, 0.5_r8k, &
      &   0.0_r8k, 0.2_r8k, 1.0_r8k, &
      &   0.0_r8k, 0.4_r8k, 0.0_r8k, &
      &   0.0_r8k, 0.4_r8k, 0.5_r8k, &
      &   0.0_r8k, 0.4_r8k, 1.0_r8k, &
      &   0.0_r8k, 0.6_r8k, 0.0_r8k, &
      &   0.0_r8k, 0.6_r8k, 0.5_r8k, &
      &   0.0_r8k, 0.6_r8k, 1.0_r8k, &
      &   0.0_r8k, 0.8_r8k, 0.0_r8k, &
      &   0.0_r8k, 0.8_r8k, 0.5_r8k, &
      &   0.0_r8k, 0.8_r8k, 1.0_r8k, &
      &   0.0_r8k, 1.0_r8k, 0.0_r8k, &
      &   0.0_r8k, 1.0_r8k, 0.5_r8k, &
      &   0.0_r8k, 1.0_r8k, 1.0_r8k, &
      &   0.1_r8k, 0.0_r8k, 0.0_r8k, &
      &   0.1_r8k, 0.0_r8k, 0.5_r8k, &
      &   0.1_r8k, 0.0_r8k, 1.0_r8k, &
      &   0.1_r8k, 0.2_r8k, 0.0_r8k, &
      &   0.1_r8k, 0.2_r8k, 0.5_r8k, &
      &   0.1_r8k, 0.2_r8k, 1.0_r8k, &
      &   0.1_r8k, 0.4_r8k, 0.0_r8k, &
      &   0.1_r8k, 0.4_r8k, 0.5_r8k, &
      &   0.1_r8k, 0.4_r8k, 1.0_r8k, &
      &   0.1_r8k, 0.6_r8k, 0.0_r8k, &
      &   0.1_r8k, 0.6_r8k, 0.5_r8k, &
      &   0.1_r8k, 0.6_r8k, 1.0_r8k, &
      &   0.1_r8k, 0.8_r8k, 0.0_r8k, &
      &   0.1_r8k, 0.8_r8k, 0.5_r8k, &
      &   0.1_r8k, 0.8_r8k, 1.0_r8k, &
      &   0.1_r8k, 1.0_r8k, 0.0_r8k, &
      &   0.1_r8k, 1.0_r8k, 0.5_r8k, &
      &   0.1_r8k, 1.0_r8k, 1.0_r8k, &
      &   0.2_r8k, 0.0_r8k, 0.0_r8k, &
      &   0.2_r8k, 0.0_r8k, 0.5_r8k, &
      &   0.2_r8k, 0.0_r8k, 1.0_r8k, &
      &   0.2_r8k, 0.2_r8k, 0.0_r8k, &
      &   0.2_r8k, 0.2_r8k, 0.5_r8k, &
      &   0.2_r8k, 0.2_r8k, 1.0_r8k, &
      &   0.2_r8k, 0.4_r8k, 0.0_r8k, &
      &   0.2_r8k, 0.4_r8k, 0.5_r8k, &
      &   0.2_r8k, 0.4_r8k, 1.0_r8k, &
      &   0.2_r8k, 0.6_r8k, 0.0_r8k, &
      &   0.2_r8k, 0.6_r8k, 0.5_r8k, &
      &   0.2_r8k, 0.6_r8k, 1.0_r8k, &
      &   0.2_r8k, 0.8_r8k, 0.0_r8k, &
      &   0.2_r8k, 0.8_r8k, 0.5_r8k, &
      &   0.2_r8k, 0.8_r8k, 1.0_r8k, &
      &   0.2_r8k, 1.0_r8k, 0.0_r8k, &
      &   0.2_r8k, 1.0_r8k, 0.5_r8k, &
      &   0.2_r8k, 1.0_r8k, 1.0_r8k, &
      &   0.3_r8k, 0.0_r8k, 0.0_r8k, &
      &   0.3_r8k, 0.0_r8k, 0.5_r8k, &
      &   0.3_r8k, 0.0_r8k, 1.0_r8k, &
      &   0.3_r8k, 0.2_r8k, 0.0_r8k, &
      &   0.3_r8k, 0.2_r8k, 0.5_r8k, &
      &   0.3_r8k, 0.2_r8k, 1.0_r8k, &
      &   0.3_r8k, 0.4_r8k, 0.0_r8k, &
      &   0.3_r8k, 0.4_r8k, 0.5_r8k, &
      &   0.3_r8k, 0.4_r8k, 1.0_r8k, &
      &   0.3_r8k, 0.6_r8k, 0.0_r8k, &
      &   0.3_r8k, 0.6_r8k, 0.5_r8k, &
      &   0.3_r8k, 0.6_r8k, 1.0_r8k, &
      &   0.3_r8k, 0.8_r8k, 0.0_r8k, &
      &   0.3_r8k, 0.8_r8k, 0.5_r8k, &
      &   0.3_r8k, 0.8_r8k, 1.0_r8k, &
      &   0.3_r8k, 1.0_r8k, 0.0_r8k, &
      &   0.3_r8k, 1.0_r8k, 0.5_r8k, &
      &   0.3_r8k, 1.0_r8k, 1.0_r8k, &
      &   0.4_r8k, 0.0_r8k, 0.0_r8k, &
      &   0.4_r8k, 0.0_r8k, 0.5_r8k, &
      &   0.4_r8k, 0.0_r8k, 1.0_r8k, &
      &   0.4_r8k, 0.2_r8k, 0.0_r8k, &
      &   0.4_r8k, 0.2_r8k, 0.5_r8k, &
      &   0.4_r8k, 0.2_r8k, 1.0_r8k, &
      &   0.4_r8k, 0.4_r8k, 0.0_r8k, &
      &   0.4_r8k, 0.4_r8k, 0.5_r8k, &
      &   0.4_r8k, 0.4_r8k, 1.0_r8k, &
      &   0.4_r8k, 0.6_r8k, 0.0_r8k, &
      &   0.4_r8k, 0.6_r8k, 0.5_r8k, &
      &   0.4_r8k, 0.6_r8k, 1.0_r8k, &
      &   0.4_r8k, 0.8_r8k, 0.0_r8k, &
      &   0.4_r8k, 0.8_r8k, 0.5_r8k, &
      &   0.4_r8k, 0.8_r8k, 1.0_r8k, &
      &   0.4_r8k, 1.0_r8k, 0.0_r8k, &
      &   0.4_r8k, 1.0_r8k, 0.5_r8k, &
      &   0.4_r8k, 1.0_r8k, 1.0_r8k, &
      &   0.5_r8k, 0.0_r8k, 0.0_r8k, &
      &   0.5_r8k, 0.0_r8k, 0.5_r8k, &
      &   0.5_r8k, 0.0_r8k, 1.0_r8k, &
      &   0.5_r8k, 0.2_r8k, 0.0_r8k, &
      &   0.5_r8k, 0.2_r8k, 0.5_r8k, &
      &   0.5_r8k, 0.2_r8k, 1.0_r8k, &
      &   0.5_r8k, 0.4_r8k, 0.0_r8k, &
      &   0.5_r8k, 0.4_r8k, 0.5_r8k, &
      &   0.5_r8k, 0.4_r8k, 1.0_r8k, &
      &   0.5_r8k, 0.6_r8k, 0.0_r8k, &
      &   0.5_r8k, 0.6_r8k, 0.5_r8k, &
      &   0.5_r8k, 0.6_r8k, 1.0_r8k, &
      &   0.5_r8k, 0.8_r8k, 0.0_r8k, &
      &   0.5_r8k, 0.8_r8k, 0.5_r8k, &
      &   0.5_r8k, 0.8_r8k, 1.0_r8k, &
      &   0.5_r8k, 1.0_r8k, 0.0_r8k, &
      &   0.5_r8k, 1.0_r8k, 0.5_r8k, &
      &   0.5_r8k, 1.0_r8k, 1.0_r8k, &
      &   0.6_r8k, 0.0_r8k, 0.0_r8k, &
      &   0.6_r8k, 0.0_r8k, 0.5_r8k, &
      &   0.6_r8k, 0.0_r8k, 1.0_r8k, &
      &   0.6_r8k, 0.2_r8k, 0.0_r8k, &
      &   0.6_r8k, 0.2_r8k, 0.5_r8k, &
      &   0.6_r8k, 0.2_r8k, 1.0_r8k, &
      &   0.6_r8k, 0.4_r8k, 0.0_r8k, &
      &   0.6_r8k, 0.4_r8k, 0.5_r8k, &
      &   0.6_r8k, 0.4_r8k, 1.0_r8k, &
      &   0.6_r8k, 0.6_r8k, 0.0_r8k, &
      &   0.6_r8k, 0.6_r8k, 0.5_r8k, &
      &   0.6_r8k, 0.6_r8k, 1.0_r8k, &
      &   0.6_r8k, 0.8_r8k, 0.0_r8k, &
      &   0.6_r8k, 0.8_r8k, 0.5_r8k, &
      &   0.6_r8k, 0.8_r8k, 1.0_r8k, &
      &   0.6_r8k, 1.0_r8k, 0.0_r8k, &
      &   0.6_r8k, 1.0_r8k, 0.5_r8k, &
      &   0.6_r8k, 1.0_r8k, 1.0_r8k, &
      &   0.7_r8k, 0.0_r8k, 0.0_r8k, &
      &   0.7_r8k, 0.0_r8k, 0.5_r8k, &
      &   0.7_r8k, 0.0_r8k, 1.0_r8k, &
      &   0.7_r8k, 0.2_r8k, 0.0_r8k, &
      &   0.7_r8k, 0.2_r8k, 0.5_r8k, &
      &   0.7_r8k, 0.2_r8k, 1.0_r8k, &
      &   0.7_r8k, 0.4_r8k, 0.0_r8k, &
      &   0.7_r8k, 0.4_r8k, 0.5_r8k, &
      &   0.7_r8k, 0.4_r8k, 1.0_r8k, &
      &   0.7_r8k, 0.6_r8k, 0.0_r8k, &
      &   0.7_r8k, 0.6_r8k, 0.5_r8k, &
      &   0.7_r8k, 0.6_r8k, 1.0_r8k, &
      &   0.7_r8k, 0.8_r8k, 0.0_r8k, &
      &   0.7_r8k, 0.8_r8k, 0.5_r8k, &
      &   0.7_r8k, 0.8_r8k, 1.0_r8k, &
      &   0.7_r8k, 1.0_r8k, 0.0_r8k, &
      &   0.7_r8k, 1.0_r8k, 0.5_r8k, &
      &   0.7_r8k, 1.0_r8k, 1.0_r8k, &
      &   0.8_r8k, 0.0_r8k, 0.0_r8k, &
      &   0.8_r8k, 0.0_r8k, 0.5_r8k, &
      &   0.8_r8k, 0.0_r8k, 1.0_r8k, &
      &   0.8_r8k, 0.2_r8k, 0.0_r8k, &
      &   0.8_r8k, 0.2_r8k, 0.5_r8k, &
      &   0.8_r8k, 0.2_r8k, 1.0_r8k, &
      &   0.8_r8k, 0.4_r8k, 0.0_r8k, &
      &   0.8_r8k, 0.4_r8k, 0.5_r8k, &
      &   0.8_r8k, 0.4_r8k, 1.0_r8k, &
      &   0.8_r8k, 0.6_r8k, 0.0_r8k, &
      &   0.8_r8k, 0.6_r8k, 0.5_r8k, &
      &   0.8_r8k, 0.6_r8k, 1.0_r8k, &
      &   0.8_r8k, 0.8_r8k, 0.0_r8k, &
      &   0.8_r8k, 0.8_r8k, 0.5_r8k, &
      &   0.8_r8k, 0.8_r8k, 1.0_r8k, &
      &   0.8_r8k, 1.0_r8k, 0.0_r8k, &
      &   0.8_r8k, 1.0_r8k, 0.5_r8k, &
      &   0.8_r8k, 1.0_r8k, 1.0_r8k, &
      &   0.9_r8k, 0.0_r8k, 0.0_r8k, &
      &   0.9_r8k, 0.0_r8k, 0.5_r8k, &
      &   0.9_r8k, 0.0_r8k, 1.0_r8k, &
      &   0.9_r8k, 0.2_r8k, 0.0_r8k, &
      &   0.9_r8k, 0.2_r8k, 0.5_r8k, &
      &   0.9_r8k, 0.2_r8k, 1.0_r8k, &
      &   0.9_r8k, 0.4_r8k, 0.0_r8k, &
      &   0.9_r8k, 0.4_r8k, 0.5_r8k, &
      &   0.9_r8k, 0.4_r8k, 1.0_r8k, &
      &   0.9_r8k, 0.6_r8k, 0.0_r8k, &
      &   0.9_r8k, 0.6_r8k, 0.5_r8k, &
      &   0.9_r8k, 0.6_r8k, 1.0_r8k, &
      &   0.9_r8k, 0.8_r8k, 0.0_r8k, &
      &   0.9_r8k, 0.8_r8k, 0.5_r8k, &
      &   0.9_r8k, 0.8_r8k, 1.0_r8k, &
      &   0.9_r8k, 1.0_r8k, 0.0_r8k, &
      &   0.9_r8k, 1.0_r8k, 0.5_r8k, &
      &   0.9_r8k, 1.0_r8k, 1.0_r8k, &
      &   1.0_r8k, 0.0_r8k, 0.0_r8k, &
      &   1.0_r8k, 0.0_r8k, 0.5_r8k, &
      &   1.0_r8k, 0.0_r8k, 1.0_r8k, &
      &   1.0_r8k, 0.2_r8k, 0.0_r8k, &
      &   1.0_r8k, 0.2_r8k, 0.5_r8k, &
      &   1.0_r8k, 0.2_r8k, 1.0_r8k, &
      &   1.0_r8k, 0.4_r8k, 0.0_r8k, &
      &   1.0_r8k, 0.4_r8k, 0.5_r8k, &
      &   1.0_r8k, 0.4_r8k, 1.0_r8k, &
      &   1.0_r8k, 0.6_r8k, 0.0_r8k, &
      &   1.0_r8k, 0.6_r8k, 0.5_r8k, &
      &   1.0_r8k, 0.6_r8k, 1.0_r8k, &
      &   1.0_r8k, 0.8_r8k, 0.0_r8k, &
      &   1.0_r8k, 0.8_r8k, 0.5_r8k, &
      &   1.0_r8k, 0.8_r8k, 1.0_r8k, &
      &   1.0_r8k, 1.0_r8k, 0.0_r8k, &
      &   1.0_r8k, 1.0_r8k, 0.5_r8k, &
      &   1.0_r8k, 1.0_r8k, 1.0_r8k ], [3,n_x*n_y*n_z] )
! Rectilinear grid
    REAL(r8k), DIMENSION(n_x), PARAMETER :: x_coords = &
      & [ 0.1_r8k, 0.2_r8k, 0.3_r8k, 0.4_r8k, 0.5_r8k, 0.6_r8k, 0.7_r8k, 0.8_r8k, 0.9_r8k, 1.0_r8k, 1.1_r8k ]
    REAL(r8k), DIMENSION(n_y), PARAMETER :: y_coords = &
      & [ 0.2_r8k, 0.4_r8k, 0.6_r8k, 0.8_r8k, 1.0_r8k, 1.2_r8k ]
    REAL(r8k), DIMENSION(n_z), PARAMETER :: z_coords = &
      & [ 0.5_r8k, 1.0_r8k, 1.5_r8k ]
! Polygonal data
    
! Unstructured grid
    
    CONTAINS
        SUBROUTINE vtk_datasets_unit (test_pass)
        USE Kinds
        USE vtk_datasets, ONLY : dataset, struct_pts, struct_grid, rectlnr_grid, polygonal_data, unstruct_grid
        USE VTK,          ONLY : vtk_legacy_init
        IMPLICIT NONE
        !>@brief
        !> Loops over each dataset type, performs a write, then performs a read on a different dataset
        !> and compares the two to make sure they are identical
        CLASS(dataset), ALLOCATABLE :: vtk_dataset_1, vtk_dataset_2
        INTEGER(i4k)                :: i
        LOGICAL, INTENT(OUT)        :: test_pass
        LOGICAL, DIMENSION(n_types) :: individual_tests_pass

        DO i = 1, 3!n_types
            IF (ALLOCATED(vtk_dataset_1)) DEALLOCATE (vtk_dataset_1)
            IF (ALLOCATED(vtk_dataset_2)) DEALLOCATE (vtk_dataset_2)
            SELECT CASE (i)
            CASE (1)
                !! Structured points
                ALLOCATE(struct_pts::vtk_dataset_1, vtk_dataset_2)
            CASE (2)
                !! Structured grid
                ALLOCATE(struct_grid::vtk_dataset_1, vtk_dataset_2)
            CASE (3)
                !! Rectilinear grid
                ALLOCATE(rectlnr_grid::vtk_dataset_1, vtk_dataset_2)
            CASE (4)
                !! Polygonal data
                ALLOCATE(polygonal_data::vtk_dataset_1, vtk_dataset_2)
            CASE (5)
                !! Unstructured grid
                ALLOCATE(unstruct_grid::vtk_dataset_1, vtk_dataset_2)
            END SELECT

            !! Data type is generated from the defined values above
            CALL vtk_dataset_1%setup(dims=dims, origin=origin, spacing=spacing, points=points, x_coords=x_coords, &
              &                      y_coords=y_coords, z_coords=z_coords)
            OPEN (unit=vtk_unit, file=filename(i), form='formatted')
            CALL vtk_dataset_1%write(vtk_unit)
            CLOSE(unit=vtk_unit)

            !! Data type is generated from the read
            OPEN (unit=vtk_unit, file=filename(i), status='old', form='formatted')
            CALL vtk_dataset_2%read(vtk_unit)
            CLOSE(unit=vtk_unit)

            !! Compare the read file and the written/read file to ensure both types are the same
            individual_tests_pass(i) = .NOT. (vtk_dataset_1 .diff. vtk_dataset_2)

            CALL vtk_legacy_init (vtk_dataset_1, vtk_unit, filetype(i), filename(i))
        END DO

        test_pass = ALL(individual_tests_pass(1:3))

        END SUBROUTINE vtk_datasets_unit
END MODULE vtk_datasets_unit_tests

PROGRAM vtk_datasets_test
    USE vtk_datasets_unit_tests, ONLY : vtk_datasets_unit
    USE PassFail,                ONLY : all_tests_pass
    IMPLICIT NONE
    !>@brief
    !> Driver testing subroutine for the attributes information
    !>@author
    !> Ian Porter
    !>@date
    !> 12/14/2017
    LOGICAL :: test_passes = .FALSE.

    CALL vtk_datasets_unit (test_passes)

    IF (test_passes) CALL all_tests_pass()

END PROGRAM vtk_datasets_test
