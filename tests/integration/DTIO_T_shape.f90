MODULE DTIO_vtkmofo
    USE Precision,    ONLY : i4k, r8k
    USE vtk_datasets, ONLY : unstruct_grid
    IMPLICIT NONE
    !! This module contains a TBP to write a formatted file
    !! Inside the TBP is the call to vtkmofo

    PRIVATE
    PUBLIC :: foo

    TYPE, EXTENDS(unstruct_grid) :: foo
        !! foo dt
    CONTAINS
        PROCEDURE, PRIVATE :: write_formatted
        GENERIC, PUBLIC :: WRITE(FORMATTED) => write_formatted
        !PROCEDURE, PRIVATE :: read_formatted
        !GENERIC, PUBLIC :: READ(FORMATTED) => read_formatted
    END TYPE foo

    CONTAINS

        SUBROUTINE write_formatted (me, unit, iotype, v_list, iostat, iomsg)
        USE Precision
        USE vtk_attributes, ONLY : scalar, attributes
        USE vtk_cells,      ONLY : voxel, hexahedron, vtkcell_list
        USE vtk,            ONLY : vtk_legacy_write
        IMPLICIT NONE
        !! Subroutine performs a formatted write for a cell
        CLASS(foo),       INTENT(IN)    :: me
        INTEGER(i4k),     INTENT(IN)    :: unit
        CHARACTER(LEN=*), INTENT(IN)    :: iotype
        INTEGER(i4k),     DIMENSION(:), INTENT(IN) :: v_list
        INTEGER(i4k),     INTENT(OUT)   :: iostat
        CHARACTER(LEN=*), INTENT(INOUT) :: iomsg
        CLASS(foo),       ALLOCATABLE   :: t_shape

        INTEGER(i4k), PARAMETER     :: n_params_to_write = 1
        TYPE (attributes), DIMENSION(n_params_to_write) :: point_vals_to_write, cell_vals_to_write
        INTEGER(i4k)                :: i
        INTEGER(i4k),     PARAMETER :: n_points = 24, n_cells = 5
        CHARACTER(LEN=*), PARAMETER :: title    = 'Testing of T-shape unstructured grid geometry'
        REAL(r8k), DIMENSION(n_cells, 1:n_params_to_write) :: cell_vals
        REAL(r8k), DIMENSION(n_points,1:n_params_to_write) :: point_vals
        REAL(r8k), DIMENSION(3,n_points), PARAMETER        :: points = RESHAPE ( &
          & [ 0.5_r8k, 0.0_r8k, 0.0_r8k, &
          &   1.0_r8k, 0.0_r8k, 0.0_r8k, &
          &   0.5_r8k, 0.5_r8k, 0.0_r8k, &
          &   1.0_r8k, 0.5_r8k, 0.0_r8k, &
          &   0.5_r8k, 0.0_r8k, 0.5_r8k, &
          &   1.0_r8k, 0.0_r8k, 0.5_r8k, &
          &   0.5_r8k, 0.5_r8k, 0.5_r8k, &
          &   1.0_r8k, 0.5_r8k, 0.5_r8k, &
          &   0.0_r8k, 0.0_r8k, 1.0_r8k, &
          &   0.5_r8k, 0.0_r8k, 1.0_r8k, &
          &   1.0_r8k, 0.0_r8k, 1.0_r8k, &
          &   1.5_r8k, 0.0_r8k, 1.0_r8k, &
          &   0.0_r8k, 0.5_r8k, 1.0_r8k, &
          &   0.5_r8k, 0.5_r8k, 1.0_r8k, &
          &   1.0_r8k, 0.5_r8k, 1.0_r8k, &
          &   1.5_r8k, 0.5_r8k, 1.0_r8k, &
          &   0.0_r8k, 0.0_r8k, 1.5_r8k, &
          &   0.5_r8k, 0.0_r8k, 1.5_r8k, &
          &   1.0_r8k, 0.0_r8k, 1.5_r8k, &
          &   1.5_r8k, 0.0_r8k, 1.5_r8k, &
          &   0.0_r8k, 0.5_r8k, 1.5_r8k, &
          &   0.5_r8k, 0.5_r8k, 1.5_r8k, &
          &   1.0_r8k, 0.5_r8k, 1.5_r8k, &
          &   1.5_r8k, 0.5_r8k, 1.5_r8k ], [3,n_points] )
        REAL(r8k), PARAMETER :: temp_default = 100.0_r8k, temp_increment = 10.0_r8k
        REAL(r8k), DIMENSION(n_points), PARAMETER :: temp_norm = &
          & [ 1.0_r8k, 1.0_r8k, 1.0_r8k, 1.0_r8k, 2.0_r8k, 2.0_r8k, 2.0_r8k, 2.0_r8k, 1.0_r8k, &
          &   3.0_r8k, 3.0_r8k, 1.0_r8k, 1.0_r8k, 4.0_r8k, 4.0_r8k, 1.0_r8k, 1.0_r8k, 2.0_r8k, &
          &   2.0_r8k, 1.0_r8k, 1.0_r8k, 3.0_r8k, 3.0_r8k, 1.0_r8k ]
        INTEGER(i4k), DIMENSION(n_cells), PARAMETER :: cellID = &
          & [ 11, 11, 11, 11, 12 ]
        TYPE(voxel),        DIMENSION(n_cells-1) :: voxel_cells     !! Voxel cell type
        TYPE(hexahedron)                         :: hexahedron_cell !! Hexahedron cell type
        TYPE(vtkcell_list), DIMENSION(n_cells)   :: cell_list       !! Full list of all cells
        CHARACTER(LEN=10), DIMENSION(n_params_to_write), PARAMETER :: cell_dataname = &
          & [ 'cellIDs   ' ]
        CHARACTER(LEN=15), DIMENSION(n_params_to_write), PARAMETER :: point_dataname = &
          & [ 'Temperature(K) ' ]

        IF (SIZE(v_list) > 0) THEN
            !! Maybe at some point, do something with v_list. For now this silences the compiler warning for unused variable
        END IF

        CALL voxel_cells(1)%setup ( [ 0, 1, 2, 3, 4, 5, 6, 7 ] )
        CALL voxel_cells(2)%setup ( [ 4, 5, 6, 7, 9, 10, 13, 14 ] )
        CALL voxel_cells(3)%setup ( [ 8, 9, 12, 13, 16, 17, 20, 21 ] )
        CALL voxel_cells(4)%setup ( [ 9, 10, 13, 14, 17, 18, 21, 22 ] )
        CALL hexahedron_cell%setup ( [ 10, 11, 15, 14, 18, 19, 23, 22 ] )

        ALLOCATE(cell_list(1)%cell,source=voxel_cells(1)) !! Workaround for gfortran-8.2
        ALLOCATE(cell_list(2)%cell,source=voxel_cells(2))
        ALLOCATE(cell_list(3)%cell,source=voxel_cells(3))
        ALLOCATE(cell_list(4)%cell,source=voxel_cells(4))
        ALLOCATE(cell_list(5)%cell,source=hexahedron_cell)

        ALLOCATE(t_shape, source=me)
        CALL t_shape%init (points=points, cell_list=cell_list)

        DO i = 1, n_params_to_write
            ! Cell values
            IF (.NOT. ALLOCATED(cell_vals_to_write(i)%attribute))THEN
                ALLOCATE(scalar::cell_vals_to_write(i)%attribute)
                cell_vals_to_write(1)%n = SIZE(cell_vals(:,1))
            END IF
            CALL cell_vals_to_write(i)%attribute%init (TRIM(cell_dataname(i)), numcomp=1, real1d=cell_vals(:,i))
            ! Point values
            IF (.NOT. ALLOCATED(point_vals_to_write(i)%attribute))THEN
                ALLOCATE(scalar::point_vals_to_write(i)%attribute)
                point_vals_to_write(1)%n = SIZE(point_vals(:,1))
            END IF
            CALL point_vals_to_write(i)%attribute%init (TRIM(point_dataname(i)), numcomp=1, real1d=point_vals(:,i))
        END DO

        SELECT CASE (iotype)
        CASE DEFAULT
            !! No specific formatting at this point
            CALL vtk_legacy_write (t_shape, celldatasets=cell_vals_to_write, pointdatasets=point_vals_to_write, &
              &                    unit=unit, title=title, multiple_io=.FALSE.)
        END SELECT

        iostat = 0; iomsg=''

        END SUBROUTINE write_formatted


        !SUBROUTINE read_formatted (me, unit, iotype, v_list, iostat, iomsg)
        !IMPLICIT NONE
        !!! Subroutine performs a formatted read for a cell
        !CLASS(foo),       INTENT(INOUT) :: me
        !INTEGER(i4k),     INTENT(IN)    :: unit
        !CHARACTER(LEN=*), INTENT(IN)    :: iotype
        !INTEGER(i4k),     DIMENSION(:), INTENT(IN) :: v_list
        !INTEGER(i4k),     INTENT(OUT)   :: iostat
        !CHARACTER(LEN=*), INTENT(INOUT) :: iomsg
        !
        !iostat = 0
        !
        !END SUBROUTINE read_formatted

END MODULE DTIO_vtkmofo


PROGRAM DTIO_T_shape_test
    USE Precision,    ONLY : i4k
    USE DTIO_vtkmofo, ONLY : foo
    IMPLICIT NONE
    !! author: Ian Porter
    !! date: 03/24/2019
    !!
    !! This is a test of an unstructured grid (T-shape) geometry
    !!
    INTEGER(i4k) :: unit
    TYPE(foo) :: t_shape
    CHARACTER(LEN=*), PARAMETER :: filename = 'dtio_t_shape.vtk'

    OPEN(newunit=unit, file=filename, status='unknown', form='formatted')
    WRITE(unit,'(DT)') t_shape

    CLOSE(unit)

    !OPEN(newunit=unit, file=filename, status='old', form='formatted')
    !READ(unit,'(DT)') t_shape

    WRITE(*,*) 'Finished'

END PROGRAM DTIO_T_shape_test
