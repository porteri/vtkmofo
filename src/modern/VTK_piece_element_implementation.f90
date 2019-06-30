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

        ALLOCATE(string, source=type_string // name_string // NofC_string // format_string // offset_string)

        CALL me%setup(name=DataArray_name, string=string)

        END PROCEDURE

        MODULE PROCEDURE DataArray_initialize
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 06/07/2019
        !!
        !! This converts the VTK_element_dt header into XML format
        !!
        CHARACTER(LEN=:), ALLOCATABLE :: NofC

        IF (PRESENT(type))               ALLOCATE(me%type,source=type)
        IF (PRESENT(name))               ALLOCATE(me%array_name,source=name)
        IF (PRESENT(NumberofComponents)) THEN
            ALLOCATE(NofC,source='          ')
            WRITE(NofC,'(i10)') NumberOfComponents
            ALLOCATE(me%NumberOfComponents,source=TRIM(ADJUSTL(NofC)))
        END IF
        IF (PRESENT(format))             ALLOCATE(me%format,source=format)
        IF (PRESENT(offset))             ALLOCATE(me%array_offset,source=offset)

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

END SUBMODULE VTK_piece_element_implementation
