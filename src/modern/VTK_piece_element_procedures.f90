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

        MODULE PROCEDURE piece_initialize
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
write(0,*) 'before call coordinates%initialize(geometry)'
        CALL coordinates%initialize(geometry)

        CALL me%add(coordinates)

        CALL coordinates%deallocate()
write(0,*) 'before end procedure piece_initialize'
        END PROCEDURE piece_initialize

        MODULE PROCEDURE deallocate_piece_dt
        IMPLICIT NONE
        !1 author: Ian Porter
        !! date: 07/23/2019
        !!
        !! GCC bug work around to explicitly deallocate the strings
        !!
        IF (ALLOCATED(me%type))           DEALLOCATE(me%type)
        IF (ALLOCATED(me%version))        DEALLOCATE(me%version)
        IF (ALLOCATED(me%byte_order))     DEALLOCATE(me%byte_order)
        IF (ALLOCATED(me%compression))    DEALLOCATE(me%compression)
        IF (ALLOCATED(me%file_extension)) DEALLOCATE(me%file_extension)

        CALL me%deallocate() !! Deallocates the XML pieces

        END PROCEDURE deallocate_piece_dt

        MODULE PROCEDURE PointData_setup
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 06/06/2019
        !!
        !! This writes the header for a PointData
        !!
        CHARACTER(LEN=*), PARAMETER   :: PointData_name = 'PointData'
        CHARACTER(LEN=:), ALLOCATABLE :: string
        CHARACTER(LEN=:), ALLOCATABLE :: scalar_string

!        type=”Float32” Name=”vectors” NumberOfComponents=”3”
!                       format=”appended” offset=”0”/
        IF (ALLOCATED(me%Scalars)) THEN
            ALLOCATE(scalar_string,source=' Scalars="' // me%Scalars // '"')
        ELSE
            ALLOCATE(scalar_string,source='')
        END IF

        ALLOCATE(string, source=scalar_string)

        CALL me%setup(name=PointData_name, string=string)

        END PROCEDURE PointData_setup

        MODULE PROCEDURE PointData_initialize
        USE Misc, ONLY : convert_to_string
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 06/07/2019
        !!
        !! This converts the VTK_element_dt header into XML format
        !!

        IF (PRESENT(Scalar))             ALLOCATE(me%Scalars,source=Scalar)

        CALL me%PointData_setup()

        END PROCEDURE PointData_initialize

        MODULE PROCEDURE PointData_add_attribute
        IMPLICIT NONE
        !TYPE(DataArray) :: data

        !! Need to get the name of the cell and type of the cell
        !! name = cell%dataname
        !! type = cell%datatype
        !! and append this to the text line for the pointdata
        !!
        !! Then need to get the type, etc
        !! Then need to get the data
write(0,*) 'in PointData_add_attribute. Before call me%add'
        CALL me%add(cell%convert_to_dataarray())
        ERROR STOP 'Error: PointData_add_attribute is not yet implemented.'

        END PROCEDURE PointData_add_attribute

        MODULE PROCEDURE PointData_add_attributes
        IMPLICIT NONE
        INTEGER(i4k) :: i
write(0,*) 'in PointData_add_attributes. Before call me%add'
        DO i = 1, SIZE(cell)
            CALL me%add(cell(i)%attribute%convert_to_dataarray())
        END DO

!        ERROR STOP 'Error: PointData_add_attributes is not yet implemented.'

        END PROCEDURE PointData_add_attributes

        MODULE PROCEDURE CellData_setup
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 06/06/2019
        !!
        !! This writes the header for a CellData
        !!
        CHARACTER(LEN=*), PARAMETER   :: CellData_name = 'CellData'
        CHARACTER(LEN=:), ALLOCATABLE :: string
        CHARACTER(LEN=:), ALLOCATABLE :: scalar_string

!        type=”Float32” Name=”vectors” NumberOfComponents=”3”
!                       format=”appended” offset=”0”/
        IF (ALLOCATED(me%Scalars)) THEN
            ALLOCATE(scalar_string,source=' Scalars="' // me%Scalars // '"')
        ELSE
            ALLOCATE(scalar_string,source='')
        END IF

        ALLOCATE(string, source=scalar_string)

        CALL me%setup(name=CellData_name, string=string)

        END PROCEDURE CellData_setup

        MODULE PROCEDURE CellData_initialize
        USE Misc, ONLY : convert_to_string
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 06/07/2019
        !!
        !! This converts the VTK_element_dt header into XML format
        !!

        IF (PRESENT(Scalar))             ALLOCATE(me%Scalars,source=Scalar)

        CALL me%CellData_setup()

        END PROCEDURE CellData_initialize

        MODULE PROCEDURE CellData_add_attribute
        IMPLICIT NONE

        ERROR STOP 'Error: CellData_add_attribute is not yet implemented.'

        END PROCEDURE CellData_add_attribute

        MODULE PROCEDURE CellData_add_attributes
        IMPLICIT NONE

        ERROR STOP 'Error: CellData_add_attributes is not yet implemented.'

        END PROCEDURE CellData_add_attributes

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
            CALL me%DataArray_x%initialize(type=type_float32,format=format_ascii,range_min=range(1,1),range_max=range(2,1))
            CALL me%DataArray_x%add(geometry%get_coord(1_i4k)) !! New procedure under works to append an array of reals
            CALL me%DataArray_y%initialize(type=type_float32,format=format_ascii,range_min=range(1,2),range_max=range(2,2))
            CALL me%DataArray_y%add(geometry%get_coord(2))
            CALL me%DataArray_z%initialize(type=type_float32,format=format_ascii,range_min=range(1,3),range_max=range(2,3))
            CALL me%DataArray_z%add(geometry%get_coord(3))

            CALL me%add(me%DataArray_x)
            CALL me%add(me%DataArray_y)
            CALL me%add(me%DataArray_z)
        CLASS DEFAULT
            ERROR STOP 'Error: In Coordinates_initialize, the geometry is not yet defined.'
        END SELECT

        END PROCEDURE Coordinates_initialize

END SUBMODULE VTK_piece_element_implementation
