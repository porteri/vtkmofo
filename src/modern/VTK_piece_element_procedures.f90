SUBMODULE (VTK_piece_element) VTK_piece_element_implementation
    IMPLICIT NONE
    !! author: Ian Porter
    !! date: 06/07/2019
    !!
    !! This is the basic file piece elements
    !!
    !! Data storage formats
    CHARACTER(LEN=*), PARAMETER :: format_ascii  = 'ascii'
    CHARACTER(LEN=*), PARAMETER :: format_binary = 'binary'
    CHARACTER(LEN=*), PARAMETER :: format_append = 'appended'
    !! Data types
    CHARACTER(LEN=*), PARAMETER :: type_int8    = 'Int8'
    CHARACTER(LEN=*), PARAMETER :: type_uint8   = 'UInt8'
    CHARACTER(LEN=*), PARAMETER :: type_int16   = 'Int16'
    CHARACTER(LEN=*), PARAMETER :: type_uint16  = 'UInt16'
    CHARACTER(LEN=*), PARAMETER :: type_int32   = 'Int32'
    CHARACTER(LEN=*), PARAMETER :: type_uint32  = 'UInt32'
    CHARACTER(LEN=*), PARAMETER :: type_int64   = 'Int64'
    CHARACTER(LEN=*), PARAMETER :: type_uint64  = 'UInt64'
    CHARACTER(LEN=*), PARAMETER :: type_float32 = 'Float32'
    CHARACTER(LEN=*), PARAMETER :: type_float64 = 'Float64'

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

        MODULE PROCEDURE Data_add_attribute
        IMPLICIT NONE

        !! Need to get the name of the cell and type of the cell
        !! name = cell%dataname
        !! type = cell%datatype
        !! and append this to the text line for the pointdata
        !!
        !! Then need to get the type, etc
        !! Then need to get the data

        CALL me%add(cell%convert_to_dataarray())

        END PROCEDURE Data_add_attribute

        MODULE PROCEDURE Data_add_attributes
        IMPLICIT NONE
        INTEGER(i4k) :: i

        DO i = 1, SIZE(cell)
            CALL me%add(cell(i)%attribute%convert_to_dataarray())
        END DO

        END PROCEDURE Data_add_attributes

        MODULE PROCEDURE Coordinates_initialize
        USE Precision,    ONLY : i4k, r8k
        USE vtk_datasets, ONLY : dataset, rectlnr_grid
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
            CALL me%DataArray_x%initialize(type=type_float64,format=format_ascii,range_min=range(1,1),range_max=range(2,1))
            CALL me%DataArray_x%add(geometry%get_coord(1_i4k)) !! New procedure under works to append an array of reals
            CALL me%DataArray_y%initialize(type=type_float64,format=format_ascii,range_min=range(1,2),range_max=range(2,2))
            CALL me%DataArray_y%add(geometry%get_coord(2))
            CALL me%DataArray_z%initialize(type=type_float64,format=format_ascii,range_min=range(1,3),range_max=range(2,3))
            CALL me%DataArray_z%add(geometry%get_coord(3))

            CALL me%add(me%DataArray_x)
            CALL me%add(me%DataArray_y)
            CALL me%add(me%DataArray_z)
        CLASS DEFAULT
            ERROR STOP 'Error: In Coordinates_initialize, the geometry is not yet defined.'
        END SELECT

        END PROCEDURE Coordinates_initialize

        MODULE PROCEDURE piece_set_grid
        USE Precision,    ONLY : i4k
        USE vtk_datasets, ONLY : dataset
        IMPLICIT NONE
        !1 author: Ian Porter
        !! date: 07/09/2019
        !!
        !! Initializes a piece dt with the geometry information
        !!
        CHARACTER(LEN=10) :: tmp_string = '          '
        CHARACTER(LEN=:), ALLOCATABLE :: range_string
        INTEGER(i4k) :: i, j
        INTEGER(i4k), DIMENSION(2,3)  :: range
        TYPE(Coordinates_dt) :: Coordinates

        !! TODO: Figure out why gfortran requires this
        SELECT TYPE (geometry)
        CLASS IS (dataset)
            range = geometry%get_range_cnt()
        END SELECT
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

        !! For now, don't allow "pieces" but instead force the piece to be the whole extent
        CALL me%setup(name="Piece",string="Extent=" // '"' // range_string // '"')

        CALL coordinates%initialize(geometry)

        CALL me%add(coordinates)

        CALL coordinates%deallocate()

        END PROCEDURE piece_set_grid

        MODULE PROCEDURE piece_add_data
        USE VTK_piece_element, ONLY : CellData_dt, PointData_dt
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 05/07/2019
        !!
        !! This is a deferred routine for each grid type to implement its own routine to set grid dependent data / info
        !!
        TYPE(CellData_dt)  :: CellData_xml
        TYPE(PointData_dt) :: PointData_xml

        IF (PRESENT(celldatasets)) THEN
            CALL CellData_xml%initialize()
            CALL CellData_xml%add_cell(celldatasets)
write(0,*) 'in procedure add_data, before call piece%add(CellData_xml)'
            CALL me%add(CellData_xml)
        ELSE IF (PRESENT(celldata)) THEN
            CALL CellData_xml%initialize()
            CALL CellData_xml%add_cell(celldata)
write(0,*) 'in procedure add_data, before call piece%add(CellData_xml)'
            CALL me%add(CellData_xml)
        END IF
        IF (PRESENT(pointdatasets)) THEN
            CALL PointData_xml%initialize()
            CALL PointData_xml%add_cell(pointdatasets)
write(0,*) 'in procedure add_data, before call me%piece%add(PointData_xml)'
!            IF (.NOT. ALLOCATED(me%piece)) ALLOCATE(me%piece)
            CALL me%add(PointData_xml)
        ELSE IF (PRESENT(pointdata)) THEN
            CALL PointData_xml%initialize()
            CALL PointData_xml%add_cell(pointdata)
write(0,*) 'in procedure add_data, before call piece%add(PointData_xml)'
!            IF (.NOT. ALLOCATED(me%piece)) ALLOCATE(me%piece)
            CALL me%add(PointData_xml)
        END IF

        CALL PointData_xml%deallocate()
        CALL CellData_xml%deallocate()

        END PROCEDURE piece_add_data

        MODULE PROCEDURE piece_deallocate
        IMPLICIT NONE
        !! gcc Work-around for deallocating a multi-dimension derived type w/ allocatable character strings

        IF (ALLOCATED(foo%pointdata)) CALL foo%pointdata%data_deallocate()
        IF (ALLOCATED(foo%celldata)) CALL foo%celldata%data_deallocate()

        CALL foo%me_deallocate()

        END PROCEDURE piece_deallocate

END SUBMODULE VTK_piece_element_implementation
