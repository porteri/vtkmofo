MODULE VTK_piece_element
    USE XML,            ONLY : xml_file_dt, xml_element_dt
    USE Precision,      ONLY : i4k, r8k
    USE vtk_datasets,   ONLY : dataset
    USE vtk_attributes, ONLY : attribute, attributes
    USE VTK_DataArray_element, ONLY : DataArray_dt
    IMPLICIT NONE
    !! author: Ian Porter
    !! date: 06/07/2019
    !!
    !! This is the basic file piece elements
    !!

    PRIVATE
    PUBLIC :: Coordinates_dt, CellData_dt, PointData_dt, Piece_dt

    TYPE, EXTENDS(xml_element_dt) :: Piece_dt
        PRIVATE
        CHARACTER(LEN=:), ALLOCATABLE :: type
        CHARACTER(LEN=:), ALLOCATABLE :: version
        CHARACTER(LEN=:), ALLOCATABLE :: byte_order
        CHARACTER(LEN=:), ALLOCATABLE :: compression
        CHARACTER(LEN=:), ALLOCATABLE :: file_extension
    CONTAINS
!        PROCEDURE, NON_OVERRIDABLE :: vtk_element_setup
        PROCEDURE, NON_OVERRIDABLE, PUBLIC :: initialize => piece_initialize
!        PROCEDURE(abs_set_grid), DEFERRED :: set_grid
!        GENERIC, PUBLIC :: set => set_grid
!        PROCEDURE, NON_OVERRIDABLE, PUBLIC :: finalize
        PROCEDURE, PUBLIC :: deallocate_piece_dt
!        GENERIC, PUBLIC :: deallocate => deallocate_piece_dt
    END TYPE Piece_dt

    TYPE, EXTENDS(xml_element_dt) :: PointData_dt
        !! PointData derived type
        PRIVATE
        CHARACTER(LEN=:), ALLOCATABLE :: Scalars
        CHARACTER(LEN=:), ALLOCATABLE :: Vectors
        CHARACTER(LEN=:), ALLOCATABLE :: Normals
        CHARACTER(LEN=:), ALLOCATABLE :: Tensors
        CHARACTER(LEN=:), ALLOCATABLE :: TCoords
    CONTAINS
        PROCEDURE, NON_OVERRIDABLE :: PointData_setup
        PROCEDURE, NON_OVERRIDABLE :: PointData_initialize
        GENERIC, PUBLIC :: initialize => PointData_initialize
        PROCEDURE, NON_OVERRIDABLE :: PointData_add_attribute
        PROCEDURE, NON_OVERRIDABLE :: PointData_add_attributes
        GENERIC, PUBLIC :: add_cell => PointData_add_attribute
        GENERIC, PUBLIC :: add_cell => PointData_add_attributes
    END TYPE PointData_dt

    TYPE, EXTENDS(xml_element_dt) :: CellData_dt
        !! CellData derived type
        PRIVATE
        CHARACTER(LEN=:), ALLOCATABLE :: Scalars
        CHARACTER(LEN=:), ALLOCATABLE :: Vectors
        CHARACTER(LEN=:), ALLOCATABLE :: Normals
        CHARACTER(LEN=:), ALLOCATABLE :: Tensors
        CHARACTER(LEN=:), ALLOCATABLE :: TCoords
    CONTAINS
        PROCEDURE, NON_OVERRIDABLE :: CellData_setup
        PROCEDURE, NON_OVERRIDABLE :: CellData_initialize
        GENERIC, PUBLIC :: initialize => CellData_initialize
        PROCEDURE, NON_OVERRIDABLE :: CellData_add_attribute
        PROCEDURE, NON_OVERRIDABLE :: CellData_add_attributes
        GENERIC, PUBLIC :: add_cell => CellData_add_attribute
        GENERIC, PUBLIC :: add_cell => CellData_add_attributes
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
    CONTAINS
        PROCEDURE, NON_OVERRIDABLE :: Coordinates_initialize
        GENERIC, PUBLIC :: initialize => Coordinates_initialize
    END TYPE Coordinates_dt

    INTERFACE

        MODULE SUBROUTINE piece_initialize (me, geometry)
        IMPLICIT NONE
        !1 author: Ian Porter
        !! date: 07/09/2019
        !!
        !! Initializes a piece dt with the geometry information
        !!
        CLASS(Piece_dt), INTENT(INOUT) :: me
        CLASS(dataset),  INTENT(IN)    :: geometry

        END SUBROUTINE piece_initialize

        MODULE SUBROUTINE deallocate_piece_dt (me)
        IMPLICIT NONE
        !1 author: Ian Porter
        !! date: 07/23/2019
        !!
        !! GCC bug work around to explicitly deallocate the strings
        !!
        CLASS(Piece_dt), INTENT(INOUT) :: me

        END SUBROUTINE deallocate_piece_dt

        MODULE SUBROUTINE PointData_setup (me)
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 06/07/2019
        !!
        !! This writes the header for the PointData
        !!
        CLASS(PointData_dt), INTENT(INOUT) :: me                     !! PointData DT

        END SUBROUTINE PointData_setup

        MODULE SUBROUTINE PointData_initialize (me, scalar)
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 06/07/2019
        !!
        !! This converts the VTK_element_dt header into XML format
        !!
        CLASS(PointData_dt), INTENT(INOUT) :: me                     !! PointData DT
        CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: Scalar             !! Name of scalar component

        END SUBROUTINE PointData_initialize

        RECURSIVE MODULE SUBROUTINE PointData_add_attribute (me, cell)
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 06/07/2019
        !!
        !! This converts the VTK_element_dt header into XML format
        !!
        CLASS(PointData_dt), INTENT(INOUT) :: me               !! PointData DT
        CLASS(attribute),    INTENT(IN)    :: cell             !! Name of scalar component

        END SUBROUTINE PointData_add_attribute

        RECURSIVE MODULE SUBROUTINE PointData_add_attributes (me, cell)
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 06/07/2019
        !!
        !! This converts the VTK_element_dt header into XML format
        !!
        CLASS(PointData_dt),            INTENT(INOUT) :: me               !! PointData DT
        TYPE(attributes), DIMENSION(:), INTENT(IN)    :: cell   !! Name of scalar component

        END SUBROUTINE PointData_add_attributes

        MODULE SUBROUTINE CellData_setup (me)
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 06/07/2019
        !!
        !! This writes the header for the CellData
        !!
        CLASS(CellData_dt), INTENT(INOUT) :: me                     !! CellData DT

        END SUBROUTINE CellData_setup

        MODULE SUBROUTINE CellData_initialize (me, scalar)
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 06/07/2019
        !!
        !! This converts the VTK_element_dt header into XML format
        !!
        CLASS(CellData_dt), INTENT(INOUT) :: me                     !! CellData DT
        CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: Scalar             !! Name of scalar component

        END SUBROUTINE CellData_initialize

        MODULE SUBROUTINE CellData_add_attribute (me, cell)
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 06/07/2019
        !!
        !! This converts the VTK_element_dt header into XML format
        !!
        CLASS(CellData_dt), INTENT(INOUT) :: me               !! PointData DT
        CLASS(attribute),   INTENT(IN)    :: cell             !! Name of scalar component

        END SUBROUTINE CellData_add_attribute

        MODULE SUBROUTINE CellData_add_attributes (me, cell)
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 06/07/2019
        !!
        !! This converts the VTK_element_dt header into XML format
        !!
        CLASS(CellData_dt),             INTENT(INOUT) :: me     !! PointData DT
        TYPE(attributes), DIMENSION(:), INTENT(IN)    :: cell   !! Name of scalar component

        END SUBROUTINE CellData_add_attributes

        MODULE SUBROUTINE Coordinates_initialize (me, geometry)
        IMPLICIT NONE
        !1 author: Ian Porter
        !! date: 07/09/2019
        !!
        !! Initializes a piece dt with the geometry information
        !!
        CLASS(Coordinates_dt), INTENT(INOUT) :: me
        CLASS(dataset),        INTENT(IN)    :: geometry

        END SUBROUTINE Coordinates_initialize

    END INTERFACE

END MODULE VTK_piece_element
