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

        CALL coordinates%initialize(geometry)

        CALL me%add(coordinates)

        END PROCEDURE piece_initialize

        MODULE PROCEDURE DataArray_setup
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 06/06/2019
        !!
        !! This writes the header for a DataArray
        !!
        CHARACTER(LEN=*), PARAMETER   :: DataArray_name = 'DataArray'
        CHARACTER(LEN=:), ALLOCATABLE :: string
        CHARACTER(LEN=:), ALLOCATABLE :: type_string
        CHARACTER(LEN=:), ALLOCATABLE :: name_string
        CHARACTER(LEN=:), ALLOCATABLE :: NofC_string
        CHARACTER(LEN=:), ALLOCATABLE :: format_string
        CHARACTER(LEN=:), ALLOCATABLE :: offset_string
        CHARACTER(LEN=:), ALLOCATABLE :: range_min_string
        CHARACTER(LEN=:), ALLOCATABLE :: range_max_string
!        type=”Float32” Name=”vectors” NumberOfComponents=”3”
!                       format=”appended” offset=”0”/
        IF (ALLOCATED(me%type)) THEN
            ALLOCATE(type_string,source=' type="' // me%type // '"')
        ELSE
            ALLOCATE(type_string,source='')
        END IF
        IF (ALLOCATED(me%array_name)) THEN
            ALLOCATE(name_string,source=' Name="' // me%array_name // '"')
        ELSE
            ALLOCATE(name_string,source='')
        END IF
        IF (ALLOCATED(me%NumberofComponents)) THEN
            ALLOCATE(NofC_string,source=' NumberOfComponents="' // me%NumberOfComponents // '"')
        ELSE
            ALLOCATE(NofC_string,source='')
        END IF
        IF (ALLOCATED(me%format)) THEN
            ALLOCATE(format_string,source=' format="' // me%format // '"')
        ELSE
            ALLOCATE(format_string,source='')
        END IF
        IF (ALLOCATED(me%array_offset)) THEN
            ALLOCATE(offset_string,source=' offset="' // me%array_offset // '"')
        ELSE
            ALLOCATE(offset_string,source='')
        END IF
        IF (ALLOCATED(me%range_min)) THEN
            ALLOCATE(range_min_string,source=' RangeMin="' // me%range_min // '"')
        ELSE
            ALLOCATE(range_min_string,source='')
        END IF
        IF (ALLOCATED(me%range_max)) THEN
            ALLOCATE(range_max_string,source=' RangeMax="' // me%range_max // '"')
        ELSE
            ALLOCATE(range_max_string,source='')
        END IF

        ALLOCATE(string, source=type_string // name_string // NofC_string // format_string // &
            &                   offset_string // range_min_string // range_max_string)

        CALL me%setup(name=DataArray_name, string=string)

        END PROCEDURE DataArray_setup

        MODULE PROCEDURE DataArray_initialize
        USE Misc, ONLY : convert_to_string
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 06/07/2019
        !!
        !! This converts the VTK_element_dt header into XML format
        !!

        IF (PRESENT(type))               ALLOCATE(me%type,source=type)
        IF (PRESENT(name))               ALLOCATE(me%array_name,source=name)
        IF (PRESENT(NumberofComponents)) THEN
            ALLOCATE(me%NumberOfComponents,source=convert_to_string(NumberOfComponents))
        END IF
        IF (PRESENT(format))             ALLOCATE(me%format,source=format)
        IF (PRESENT(offset))             ALLOCATE(me%array_offset,source=offset)
        IF (PRESENT(range_min)) THEN
            ALLOCATE(me%range_min,source=convert_to_string(range_min))
        END IF
        IF (PRESENT(range_max)) THEN
            ALLOCATE(me%range_max,source=convert_to_string(range_max))
        END IF

        CALL me%DataArray_setup()

        END PROCEDURE DataArray_initialize

        MODULE PROCEDURE DataArray_add_DataArray
        IMPLICIT NONE
        !! This adds an element inside of an xml element block
        TYPE(xml_element_dt), DIMENSION(:), ALLOCATABLE :: tmp_element_dt
! Currently commented out b/c element is not public
!        IF (.NOT. ALLOCATED(me%element)) THEN
!            ALLOCATE(me%element(1),source=element)
!        ELSE
!            ALLOCATE(tmp_element_dt,source=[ me%element, element ])
!            CALL MOVE_ALLOC(tmp_element_dt, me%element)
!        END IF

        END PROCEDURE DataArray_add_DataArray

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
