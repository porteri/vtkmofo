SUBMODULE (VTK_piece_element) VTK_piece_element_implementation
    USE Precision, ONLY : i4k, r8k
    USE VTK_formats_types, ONLY : type_float64
    USE XML, ONLY : file_format_text
    IMPLICIT NONE
    !! author: Ian Porter
    !! date: 06/07/2019
    !!
    !! This is the basic file piece elements
    !!
    !! Data storage formats

    CONTAINS

        MODULE PROCEDURE Data_setup
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 06/06/2019
        !!
        !! This writes the header for a Data
        !!
        CHARACTER(LEN=*), PARAMETER   :: PointData_name = 'PointData'
        CHARACTER(LEN=*), PARAMETER   :: CellData_name  = 'CellData'
        CHARACTER(LEN=:), ALLOCATABLE :: string
        CHARACTER(LEN=:), ALLOCATABLE :: my_name
        CHARACTER(LEN=:), ALLOCATABLE :: scalar_string

        IF (ALLOCATED(me%Scalars)) THEN
            ALLOCATE(scalar_string,source=' Scalars="' // me%Scalars // '"')
        ELSE
            ALLOCATE(scalar_string,source='')
        END IF

        ALLOCATE(string, source=scalar_string)

        SELECT TYPE (me)
        CLASS IS (pointdata_dt)
            ALLOCATE(my_name,source=PointData_name)
        CLASS IS (celldata_dt)
            ALLOCATE(my_name,source=CellData_name)
        CLASS DEFAULT
            ERROR STOP 'Error: Undefined type in Data_setup'
        END SELECT

        CALL me%setup(name=my_name, string=string)

        END PROCEDURE Data_setup

        MODULE PROCEDURE Data_initialize
        USE Misc, ONLY : convert_to_string
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 06/07/2019
        !!
        !! This converts the VTK_element_dt header into XML format
        !!

        IF (PRESENT(Scalar)) ALLOCATE(me%Scalars,source=Scalar)

        CALL me%Data_setup()

        END PROCEDURE Data_initialize

        MODULE PROCEDURE Data_add_attribute
        IMPLICIT NONE

        CALL me%add(cell%convert_to_dataarray())

        END PROCEDURE Data_add_attribute

        MODULE PROCEDURE Data_add_attributes
        IMPLICIT NONE
        INTEGER(i4k) :: i

        DO i = 1, SIZE(cell)
            CALL me%add(cell(i)%attribute%convert_to_dataarray())
        END DO

        END PROCEDURE Data_add_attributes

        MODULE PROCEDURE Data_finalize
        IMPLICIT NONE

!! IDK if there's anything to do here

        END PROCEDURE Data_finalize

        MODULE PROCEDURE Data_deallocate
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 06/07/2019
        !!
        !! Explicitly deallocate a Data_dt
        !!

        IF (ALLOCATED(foo%scalars)) DEALLOCATE(foo%scalars)
        IF (ALLOCATED(foo%Vectors)) DEALLOCATE(foo%Vectors)
        IF (ALLOCATED(foo%Normals)) DEALLOCATE(foo%Normals)
        IF (ALLOCATED(foo%Tensors)) DEALLOCATE(foo%Tensors)
        IF (ALLOCATED(foo%TCoords)) DEALLOCATE(foo%TCoords)

        CALL foo%deallocate()

        END PROCEDURE Data_deallocate

        MODULE PROCEDURE Points_initialize
        USE vtk_datasets, ONLY : struct_grid, unstruct_grid
        USE Misc,         ONLY : convert_to_string
        IMPLICIT NONE
        !1 author: Ian Porter
        !! date: 07/09/2019
        !!
        !! Initializes a piece dt with the geometry information
        !!
        INTEGER(i4k) :: i
        CHARACTER(LEN=*), PARAMETER :: Points_name = 'Points'

        CALL me%setup(name=Points_name)

        SELECT TYPE (geometry)
        CLASS IS (struct_grid)
            !! For now, don't allow "pieces" but instead force the piece to be the whole extent
            CALL me%DataArray%initialize(type=type_float64,format=file_format_text,NumberofComponents=3)
            DO i = 1, geometry%n_points
                CALL me%DataArray%add(geometry%get_point(i)) !! New procedure under works to append an array of reals
            END DO
            CALL me%add(me%DataArray)
            CALL me%DataArray%me_deallocate()
        CLASS IS (unstruct_grid)
            !! For now, don't allow "pieces" but instead force the piece to be the whole extent
            CALL me%DataArray%initialize(type=type_float64,format=file_format_text,NumberofComponents=3)
            DO i = 1, geometry%n_points
                CALL me%DataArray%add(geometry%get_point(i)) !! New procedure under works to append an array of reals
            END DO
            CALL me%add(me%DataArray)
            CALL me%DataArray%me_deallocate()
        CLASS DEFAULT
            ERROR STOP 'Error: In Points_initialize, the geometry is not defined.'
        END SELECT

        END PROCEDURE Points_initialize

        MODULE PROCEDURE Points_deallocate
        IMPLICIT NONE
        !! gcc Work-around for deallocating a multi-dimension derived type w/ allocatable character strings

        CALL foo%DataArray%me_deallocate()

        CALL foo%deallocate()

        END PROCEDURE Points_deallocate

        MODULE PROCEDURE Cells_initialize
        USE vtk_datasets, ONLY : unstruct_grid
        IMPLICIT NONE
        !1 author: Ian Porter
        !! date: 07/09/2019
        !!
        !! Initializes a piece dt with the geometry information
        !!
        INTEGER(i4k) :: i, cnt
        CHARACTER(LEN=*), PARAMETER :: my_name = 'Cells'

        CALL me%setup(name=my_name)

        SELECT TYPE (geometry)
        CLASS IS (unstruct_grid)
            !! Set up cell connectivity
            CALL me%connectivity%initialize(name='connectivity',type=type_float64,format=file_format_text)
            DO i = 1, geometry%n_cells
                CALL me%connectivity%add(geometry%get_connectivity(i)) !! New procedure under works to append an array of reals
            END DO
            CALL me%add(me%connectivity)
            CALL me%connectivity%me_deallocate()
            !! Set up cell offsets
            CALL me%offsets%initialize(name='offsets',type=type_float64,format=file_format_text)
            cnt = 0
            DO i = 1, geometry%n_cells
                cnt = cnt + geometry%get_offset(i)
                CALL me%offsets%add([cnt]) !! New procedure under works to append an array of reals
            END DO
            CALL me%add(me%offsets)
            CALL me%offsets%me_deallocate()
            !! Set up cell types
            CALL me%types%initialize(name='types',type=type_float64,format=file_format_text)
            DO i = 1, geometry%n_cells
                CALL me%types%add([geometry%get_type(i)]) !! New procedure under works to append an array of reals
            END DO
            CALL me%add(me%types)
            CALL me%types%me_deallocate()
        CLASS DEFAULT
            ERROR STOP 'Error: In Cells_initialize, the geometry is not yet defined.'
        END SELECT

        END PROCEDURE Cells_initialize

        MODULE PROCEDURE Cells_deallocate
        IMPLICIT NONE
        !! gcc Work-around for deallocating a multi-dimension derived type w/ allocatable character strings

        CALL foo%connectivity%me_deallocate()
        CALL foo%offsets%me_deallocate()
        CALL foo%types%me_deallocate()

        CALL foo%deallocate()

        END PROCEDURE Cells_deallocate

        MODULE PROCEDURE Coordinates_initialize
        USE vtk_datasets, ONLY : dataset, struct_pts, struct_grid, rectlnr_grid, polygonal_data, unstruct_grid
        USE Misc,         ONLY : convert_to_string
        IMPLICIT NONE
        !1 author: Ian Porter
        !! date: 07/09/2019
        !!
        !! Initializes a piece dt with the geometry information
        !!
        REAL(r8k),   DIMENSION(2,3) :: range
        CHARACTER(LEN=*), PARAMETER :: Coordinate_name = 'Coordinates'

        CALL me%setup(name=Coordinate_name)
        !! TODO: Figure out why gfortran requires this
        SELECT TYPE (geometry)
        CLASS IS (dataset)
            range = geometry%get_range()
        END SELECT
        !! end TODO

        SELECT TYPE (geometry)
        CLASS IS (rectlnr_grid)
            !! For now, don't allow "pieces" but instead force the piece to be the whole extent
            CALL me%DataArray_x%initialize(type=type_float64,format=file_format_text,range_min=range(1,1),range_max=range(2,1))
            CALL me%DataArray_x%add(geometry%get_coord(1))
            CALL me%DataArray_y%initialize(type=type_float64,format=file_format_text,range_min=range(1,2),range_max=range(2,2))
            CALL me%DataArray_y%add(geometry%get_coord(2))
            CALL me%DataArray_z%initialize(type=type_float64,format=file_format_text,range_min=range(1,3),range_max=range(2,3))
            CALL me%DataArray_z%add(geometry%get_coord(3))

            CALL me%add(me%DataArray_x)
            CALL me%add(me%DataArray_y)
            CALL me%add(me%DataArray_z)
        CLASS DEFAULT
            ERROR STOP 'Error: In Coordinates_initialize, the geometry is not yet defined.'
        END SELECT

        END PROCEDURE Coordinates_initialize

        MODULE PROCEDURE Coordinates_deallocate
        IMPLICIT NONE
        !! gcc Work-around for deallocating a multi-dimension derived type w/ allocatable character strings

        CALL foo%DataArray_x%me_deallocate()
        CALL foo%DataArray_y%me_deallocate()
        CALL foo%DataArray_z%me_deallocate()

        CALL foo%deallocate()

        END PROCEDURE Coordinates_deallocate

        MODULE PROCEDURE piece_set_grid
        USE vtk_datasets, ONLY : dataset, struct_pts, struct_grid, rectlnr_grid, polygonal_data, unstruct_grid
        IMPLICIT NONE
        !1 author: Ian Porter
        !! date: 07/09/2019
        !!
        !! Initializes a piece dt with the geometry information
        !!
        CHARACTER(LEN=10) :: tmp_string = '          '
        CHARACTER(LEN=10) :: n_points   = '          '
        CHARACTER(LEN=10) :: n_cells    = '          '
        CHARACTER(LEN=:), ALLOCATABLE :: range_string
        INTEGER(i4k) :: i, j
        INTEGER(i4k), DIMENSION(2,3)  :: range

        !! TODO: Figure out why gfortran requires this
        SELECT TYPE (geometry)
        CLASS IS (dataset)
            range = geometry%get_range_cnt()
            !! end TODO
            DO i = 1, 3
                DO j = 1, 2
                    WRITE(tmp_string,'(i10)') range(j,i)
                    IF (.NOT. ALLOCATED(range_string)) THEN
                        ALLOCATE(range_string,source=TRIM(ADJUSTL(tmp_string)))
                    ELSE
                        range_string = range_string // ' ' // TRIM(ADJUSTL(tmp_string))
                    END IF
                END DO
            END DO
        CLASS IS (unstruct_grid)
            WRITE(n_points,'(i10)') geometry%n_points
            WRITE(n_cells,'(i10)') geometry%n_cells
        END SELECT

        SELECT TYPE (geometry)
        CLASS IS (struct_pts)
            CALL me%setup(name="Piece",string="Extent=" // '"' // range_string // '"')
        CLASS IS (struct_grid)
            !! For now, don't allow "pieces" but instead force the piece to be the whole extent
            CALL me%setup(name="Piece",string="Extent=" // '"' // range_string // '"')
            ALLOCATE(me%points)
            CALL me%points%initialize(geometry)
            CALL me%add(me%points)
        CLASS IS (rectlnr_grid)
            !! For now, don't allow "pieces" but instead force the piece to be the whole extent
            CALL me%setup(name="Piece",string="Extent=" // '"' // range_string // '"')
            ALLOCATE(me%coordinates)
            CALL me%coordinates%initialize(geometry)
            CALL me%add(me%coordinates)
        CLASS IS (polygonal_data)
            ERROR STOP 'Error: polygonal_data is not yet implemented in piece_set_grid'
        CLASS IS (unstruct_grid)
            !! For now, don't allow "pieces" but instead force the piece to be the whole extent
            CALL me%setup(name="Piece",string="NumberOfPoints=" // '"' // TRIM(ADJUSTL(n_points)) // '"' // &
                &                             " NumberOfCells=" // '"' // TRIM(ADJUSTL(n_cells)) // '"')
            ALLOCATE(me%points)
            CALL me%points%initialize(geometry)
            CALL me%add(me%points)
            ALLOCATE(me%cells)
            CALL me%cells%initialize(geometry)
            CALL me%add(me%cells)
        CLASS DEFAULT
            ERROR STOP 'Error: Unknown geometry type in piece_set_grid'
        END SELECT

        END PROCEDURE piece_set_grid

        MODULE PROCEDURE piece_add_data
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 07/28/2019
        !!
        !! This is a deferred routine for each grid type to implement its own routine to set grid dependent data / info
        !!

        IF (PRESENT(celldatasets)) THEN
            IF (.NOT. ALLOCATED(me%celldata)) THEN
                ALLOCATE(me%celldata)
                CALL me%celldata%initialize()
            END IF
            CALL me%celldata%add_cell(celldatasets)
        ELSE IF (PRESENT(celldata)) THEN
            IF (.NOT. ALLOCATED(me%celldata)) THEN
                ALLOCATE(me%celldata)
                CALL me%celldata%initialize()
            END IF
            CALL me%celldata%add_cell(celldata)
        END IF
        IF (PRESENT(pointdatasets)) THEN
            IF (.NOT. ALLOCATED(me%pointdata)) THEN
                ALLOCATE(me%pointdata)
                CALL me%pointdata%initialize()
            END IF
            CALL me%pointdata%add_cell(pointdatasets)
        ELSE IF (PRESENT(pointdata)) THEN
            IF (.NOT. ALLOCATED(me%pointdata)) THEN
                ALLOCATE(me%pointdata)
                CALL me%pointdata%initialize()
            END IF
            CALL me%pointdata%add_cell(pointdata)
        END IF

        END PROCEDURE piece_add_data

        MODULE PROCEDURE piece_finalize
        IMPLICIT NONE

        IF (ALLOCATED(me%pointdata)) THEN
            CALL me%pointdata%finalize()
            CALL me%add(me%pointdata)
        END IF
        IF (ALLOCATED(me%celldata)) THEN
          CALL me%celldata%finalize()
          CALL me%add(me%celldata)
        END IF

        END PROCEDURE piece_finalize

        MODULE PROCEDURE piece_deallocate
        IMPLICIT NONE
        !! gcc Work-around for deallocating a multi-dimension derived type w/ allocatable character strings

        IF (ALLOCATED(foo%pointdata))   CALL foo%pointdata%data_deallocate()
        IF (ALLOCATED(foo%celldata))    CALL foo%celldata%data_deallocate()
        IF (ALLOCATED(foo%coordinates)) CALL foo%coordinates%coordinates_deallocate()
        IF (ALLOCATED(foo%points))      CALL foo%points%points_deallocate()
        IF (ALLOCATED(foo%cells))       CALL foo%cells%cells_deallocate()

        CALL foo%me_deallocate()

        END PROCEDURE piece_deallocate

END SUBMODULE VTK_piece_element_implementation
