SUBMODULE (VTK_DataArray_element) VTK_DataArray_element_implementation
    USE VTK_formats_types
    IMPLICIT NONE
    !! author: Ian Porter
    !! date: 06/07/2019
    !!
    !! This is the basic file piece elements
    !!
    !! Data storage formats

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
        CHARACTER(LEN=:), ALLOCATABLE :: range_min_string
        CHARACTER(LEN=:), ALLOCATABLE :: range_max_string

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
        USE Misc, ONLY : convert_to_string, to_lowercase
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 06/07/2019
        !!
        !! This converts the VTK_element_dt header into XML format
        !!

        IF (PRESENT(type)) THEN
            !! May need to convert the legacy data type names to the modern type names
            !bit, unsigned_char, char, unsigned_short, short, unsigned_int, int, unsigned_long, long, float, or double
            !Int8, UInt8, Int16, UInt16, Int32, UInt32, Int64, UInt64, Float32, Float64
            SELECT CASE (to_lowercase(type))
            CASE ('float')
                ALLOCATE(me%type,source=type_float32)
            CASE ('double')
                ALLOCATE(me%type,source=type_float64)
            CASE ('int')
                ALLOCATE(me%type,source=type_int32)
            CASE ('unsigned_int')
                ALLOCATE(me%type,source=type_uint32)
            CASE DEFAULT
                !! Assume all other data types are ok
                ALLOCATE(me%type,source=type)
            END SELECT
        END IF
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
        !TYPE(xml_element_dt), DIMENSION(:), ALLOCATABLE :: tmp_element_dt

        END PROCEDURE DataArray_add_DataArray

        MODULE PROCEDURE DataArray_deallocate
        IMPLICIT NONE
        !! This explicitly deallocates a DataArray

        IF (ALLOCATED(me%type))               DEALLOCATE(me%type)
        IF (ALLOCATED(me%array_name))         DEALLOCATE(me%array_name)
        IF (ALLOCATED(me%NumberOfComponents)) DEALLOCATE(me%NumberofComponents)
        IF (ALLOCATED(me%format))             DEALLOCATE(me%format)
        IF (ALLOCATED(me%array_offset))       DEALLOCATE(me%array_offset)
        IF (ALLOCATED(me%range_min))          DEALLOCATE(me%range_min)
        IF (ALLOCATED(me%range_max))          DEALLOCATE(me%range_max)

        CALL me%deallocate()

        END PROCEDURE DataArray_deallocate

END SUBMODULE VTK_DataArray_element_implementation
