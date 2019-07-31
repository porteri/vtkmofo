MODULE VTK_DataArray_element
    USE XML,            ONLY : xml_element_dt
    USE Precision,      ONLY : i4k, r8k
    IMPLICIT NONE
    !! author: Ian Porter
    !! date: 06/07/2019
    !!
    !! This is the basic file piece elements
    !!

    PRIVATE
    PUBLIC :: DataArray_dt

    TYPE, EXTENDS(xml_element_dt) :: DataArray_dt
        !! DataArray derived type
        PRIVATE
        CHARACTER(LEN=:), ALLOCATABLE :: type
        CHARACTER(LEN=:), ALLOCATABLE :: array_name
        CHARACTER(LEN=:), ALLOCATABLE :: NumberofComponents
        CHARACTER(LEN=:), ALLOCATABLE :: format
        CHARACTER(LEN=:), ALLOCATABLE :: array_offset
        CHARACTER(LEN=:), ALLOCATABLE :: range_min
        CHARACTER(LEN=:), ALLOCATABLE :: range_max
    CONTAINS
        PROCEDURE, NON_OVERRIDABLE :: DataArray_setup
        PROCEDURE, NON_OVERRIDABLE :: DataArray_initialize
        GENERIC, PUBLIC :: initialize => DataArray_initialize
        PROCEDURE :: element_add_element => DataArray_add_DataArray
        PROCEDURE :: DataArray_deallocate
        GENERIC, PUBLIC :: me_deallocate => DataArray_deallocate
    END TYPE DataArray_dt

    INTERFACE

        MODULE SUBROUTINE DataArray_setup (me)
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 06/07/2019
        !!
        !! This writes the header for the DataArray
        !!
        CLASS(DataArray_dt), INTENT(INOUT) :: me                     !! DataArray DT

        END SUBROUTINE DataArray_setup

        MODULE SUBROUTINE DataArray_initialize (me, type, name, NumberOfComponents, format, offset, range_min, range_max)
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 06/07/2019
        !!
        !! This converts the VTK_element_dt header into XML format
        !!
        CLASS(DataArray_dt), INTENT(INOUT) :: me                        !! DataArray DT
        CHARACTER(LEN=*),    INTENT(IN), OPTIONAL :: type               !! type of data of a single component
        CHARACTER(LEN=*),    INTENT(IN), OPTIONAL :: name               !! Name of the array
        INTEGER(i4k),        INTENT(IN), OPTIONAL :: NumberOfComponents !! The # of components per value in the array
        CHARACTER(LEN=*),    INTENT(IN), OPTIONAL :: format             !! The means by whih the data is stored in the file
        CHARACTER(LEN=*),    INTENT(IN), OPTIONAL :: offset             !! If format='appended', this specifies the offset from the
                                                                        !! beginning of the appended data
        REAL(r8k),           INTENT(IN), OPTIONAL :: range_min          !! Min value in array of numbers
        REAL(r8k),           INTENT(IN), OPTIONAL :: range_max          !! Max value in array of numbers

        END SUBROUTINE DataArray_initialize

        MODULE SUBROUTINE DataArray_add_DataArray (me, element)
        IMPLICIT NONE
        !! This adds a DataArray inside of a xml DataArray block
        CLASS(DataArray_dt),   INTENT(INOUT) :: me       !! DataArray DT
        CLASS(xml_element_dt), INTENT(IN)    :: element  !! Inner XML element

        END SUBROUTINE DataArray_add_DataArray

        RECURSIVE MODULE SUBROUTINE DataArray_deallocate (me)
        IMPLICIT NONE
        !! This explicitly deallocates a DataArray
        CLASS(DataArray_dt), INTENT(INOUT) :: me         !! DataArray DT

        END SUBROUTINE DataArray_deallocate

    END INTERFACE

END MODULE VTK_DataArray_element
