MODULE VTK_piece_element
    USE Precision,      ONLY : i4k, r8k
    USE XML,            ONLY : xml_element_dt
    USE vtk_datasets,   ONLY : dataset
    USE vtk_attributes, ONLY : attribute, attributes
    USE VTK_DataArray_element, ONLY : DataArray_dt
    USE VTK_element,    ONLY : VTK_element_dt
    IMPLICIT NONE
    !! author: Ian Porter
    !! date: 06/07/2019
    !!
    !! This is the basic file piece elements
    !!

    PRIVATE
    PUBLIC :: Coordinates_dt, CellData_dt, PointData_dt, Piece_dt

    TYPE, EXTENDS(xml_element_dt) :: Data_dt
        !! PointData derived type
        PRIVATE
        CHARACTER(LEN=:), ALLOCATABLE :: Scalars
        CHARACTER(LEN=:), ALLOCATABLE :: Vectors
        CHARACTER(LEN=:), ALLOCATABLE :: Normals
        CHARACTER(LEN=:), ALLOCATABLE :: Tensors
        CHARACTER(LEN=:), ALLOCATABLE :: TCoords
    CONTAINS
        PROCEDURE, NON_OVERRIDABLE :: Data_setup
        PROCEDURE, NON_OVERRIDABLE :: Data_initialize
        GENERIC, PUBLIC :: initialize => Data_initialize
        PROCEDURE, NON_OVERRIDABLE :: Data_add_attribute
        PROCEDURE, NON_OVERRIDABLE :: Data_add_attributes
        GENERIC, PUBLIC :: add_cell => Data_add_attribute
        GENERIC, PUBLIC :: add_cell => Data_add_attributes
        PROCEDURE :: Data_deallocate
    END TYPE Data_dt

    TYPE, EXTENDS(Data_dt) :: PointData_dt
        !! PointData derived type
    END TYPE PointData_dt

    TYPE, EXTENDS(Data_dt) :: CellData_dt
        !! CellData derived type
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

    TYPE, EXTENDS(VTK_element_dt) :: Piece_dt
        TYPE(Coordinates_dt), ALLOCATABLE :: coordinates
        TYPE(PointData_dt),   ALLOCATABLE :: pointdata
        TYPE(CellData_dt),    ALLOCATABLE :: celldata
    CONTAINS
!        PROCEDURE, NON_OVERRIDABLE, PUBLIC :: initialize => piece_initialize
        PROCEDURE, PRIVATE :: piece_set_grid
        GENERIC, PUBLIC :: set => piece_set_grid
        PROCEDURE, NON_OVERRIDABLE, PUBLIC :: piece_add_data
        GENERIC, PUBLIC :: add_data => piece_add_data
        PROCEDURE, PUBLIC :: piece_deallocate
    END TYPE Piece_dt

    INTERFACE

        MODULE SUBROUTINE Data_setup (me)
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 06/07/2019
        !!
        !! This writes the header for a Data_dt
        !!
        CLASS(Data_dt), INTENT(INOUT) :: me                     !! PointData DT

        END SUBROUTINE Data_setup

        MODULE SUBROUTINE Data_initialize (me, scalar)
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 06/07/2019
        !!
        !! This initializes the data within a Data_dt
        !!
        CLASS(Data_dt),   INTENT(INOUT)        :: me            !! PointData DT
        CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: Scalar        !! Name of scalar component

        END SUBROUTINE Data_initialize

        RECURSIVE MODULE SUBROUTINE Data_deallocate (foo)
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 06/07/2019
        !!
        !! Explicitly deallocate a Data_dt
        !!
        CLASS(Data_dt), INTENT(INOUT) :: foo                    !! PointData DT

        END SUBROUTINE Data_deallocate

        RECURSIVE MODULE SUBROUTINE Data_add_attribute (me, cell)
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 06/07/2019
        !!
        !! This adds data inside of a Data_dt
        !!
        CLASS(Data_dt),   INTENT(INOUT) :: me               !! Data DT
        CLASS(attribute), INTENT(IN)    :: cell             !! Name of scalar component

        END SUBROUTINE Data_add_attribute

        RECURSIVE MODULE SUBROUTINE Data_add_attributes (me, cell)
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 06/07/2019
        !!
        !! This adds a set of data inside of a Data_dt the VTK_element_dt header into XML format
        !!
        CLASS(Data_dt),                 INTENT(INOUT) :: me     !! Data DT
        TYPE(attributes), DIMENSION(:), INTENT(IN)    :: cell   !! Name of scalar component

        END SUBROUTINE Data_add_attributes

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

        MODULE SUBROUTINE piece_set_grid (me, geometry)
        IMPLICIT NONE
        !1 author: Ian Porter
        !! date: 07/09/2019
        !!
        !! Initializes a piece dt with the geometry information
        !!
        CLASS(Piece_dt), INTENT(INOUT) :: me
        CLASS(dataset),  INTENT(IN)    :: geometry

        END SUBROUTINE piece_set_grid

        MODULE SUBROUTINE piece_add_data (me, celldata, pointdata, celldatasets, pointdatasets)
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 05/07/2019
        !!
        !! This is a deferred routine for each grid type to implement its own routine to set grid dependent data / info
        !!
        CLASS(Piece_dt),                INTENT(INOUT)        :: me
        CLASS(attribute),               INTENT(IN), OPTIONAL :: celldata   !!
        CLASS(attribute),               INTENT(IN), OPTIONAL :: pointdata  !!
        TYPE(attributes), DIMENSION(:), INTENT(IN), OPTIONAL :: celldatasets  !!
        TYPE(attributes), DIMENSION(:), INTENT(IN), OPTIONAL :: pointdatasets !!

        END SUBROUTINE piece_add_data

        RECURSIVE MODULE SUBROUTINE piece_deallocate (foo)
        IMPLICIT NONE
        !! Explicitly deallocate a piece dt
        CLASS(Piece_dt), INTENT(INOUT) :: foo

        END SUBROUTINE piece_deallocate

    END INTERFACE

END MODULE VTK_piece_element
