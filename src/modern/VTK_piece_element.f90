MODULE VTK_piece_element
    USE XML,       ONLY : xml_file_dt, xml_element_dt
    USE Precision, ONLY : i4k
    IMPLICIT NONE
    !! author: Ian Porter
    !! date: 06/07/2019
    !!
    !! This is the basic file piece elements
    !!

    PRIVATE
    PUBLIC :: DataArray_dt, Coordinates_dt, CellData_dt, PointData_dt

    TYPE, EXTENDS(xml_element_dt) :: Piece_dt
        PRIVATE
        CHARACTER(LEN=:), ALLOCATABLE :: type
        CHARACTER(LEN=:), ALLOCATABLE :: version
        CHARACTER(LEN=:), ALLOCATABLE :: byte_order
        CHARACTER(LEN=:), ALLOCATABLE :: compression
        CHARACTER(LEN=:), ALLOCATABLE :: file_extension
!    CONTAINS
!        PROCEDURE, NON_OVERRIDABLE :: vtk_element_setup
!        PROCEDURE, NON_OVERRIDABLE, PUBLIC :: initialize
!        PROCEDURE(abs_set_grid_data), DEFERRED :: set_grid_data
!        GENERIC, PUBLIC :: set => set_grid_data
!        PROCEDURE, NON_OVERRIDABLE, PUBLIC :: finalize
    END TYPE Piece_dt

    TYPE, EXTENDS(xml_element_dt) :: DataArray_dt
        !! DataArray derived type
        PRIVATE
        CHARACTER(LEN=:), ALLOCATABLE :: type
        CHARACTER(LEN=:), ALLOCATABLE :: array_name
        CHARACTER(LEN=:), ALLOCATABLE :: NumberofComponents
        CHARACTER(LEN=:), ALLOCATABLE :: format
        CHARACTER(LEN=:), ALLOCATABLE :: array_offset
    CONTAINS
        PROCEDURE, NON_OVERRIDABLE :: DataArray_setup
        PROCEDURE, NON_OVERRIDABLE :: DataArray_initialize
        GENERIC, PUBLIC :: initialize => DataArray_initialize
        PROCEDURE :: element_add_element => DataArray_add_DataArray
    END TYPE DataArray_dt

    TYPE, EXTENDS(xml_element_dt) :: PointData_dt
        !! PointData derived type
        PRIVATE
        CHARACTER(LEN=:), ALLOCATABLE :: Scalars
        CHARACTER(LEN=:), ALLOCATABLE :: Vectors
        CHARACTER(LEN=:), ALLOCATABLE :: Normals
        CHARACTER(LEN=:), ALLOCATABLE :: Tensors
        CHARACTER(LEN=:), ALLOCATABLE :: TCoords
    END TYPE PointData_dt

    TYPE, EXTENDS(xml_element_dt) :: CellData_dt
        !! CellData derived type
        PRIVATE
    END TYPE CellData_dt

    TYPE, EXTENDS(xml_element_dt) :: Points_dt
        !! Points derived type
        PRIVATE
        TYPE(DataArray_dt) :: DataArray
    END TYPE Points_dt

    TYPE, EXTENDS(xml_element_dt) :: Coordinates_dt
        !! Coordinates derived type
        PRIVATE
        TYPE(DataArray_dt) :: DataArray_x
        TYPE(DataArray_dt) :: DataArray_y
        TYPE(DataArray_dt) :: DataArray_z
!    CONTAINS
!        PROCEDURE, NON_OVERRIDABLE :: Coordinates_setup
!        PROCEDURE, NON_OVERRIDABLE :: Coordinates_initialize
    END TYPE Coordinates_dt

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

        MODULE SUBROUTINE DataArray_initialize (me, type, name, NumberOfComponents, format, offset)
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 06/07/2019
        !!
        !! This converts the VTK_element_dt header into XML format
        !!
        CLASS(DataArray_dt), INTENT(INOUT) :: me                     !! DataArray DT
        CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: type               !! type of data of a single component
        CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: name               !! Name of the array
        INTEGER(i4k),     INTENT(IN), OPTIONAL :: NumberOfComponents !! The # of components per value in the array
        CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: format             !! The means by whih the data is stored in the file
        CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: offset             !! If format='appended', this specifies the offset from the
                                                                     !! beginning of the appended data

        END SUBROUTINE DataArray_initialize

        MODULE SUBROUTINE DataArray_add_DataArray (me, element)
        IMPLICIT NONE
        !! This adds a DataArray inside of a xml DataArray block
        CLASS(DataArray_dt),   INTENT(INOUT) :: me       !! XML element derived type
        CLASS(xml_element_dt), INTENT(IN)    :: element  !! Inner XML element

        END SUBROUTINE DataArray_add_DataArray

    END INTERFACE

END MODULE VTK_piece_element
