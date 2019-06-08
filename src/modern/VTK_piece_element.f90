MODULE VTK_piece_element
    USE XML, ONLY : xml_file_dt, xml_element_dt
    IMPLICIT NONE
    !! author: Ian Porter
    !! date: 06/07/2019
    !!
    !! This is the basic file piece elements
    !!

    PRIVATE

    TYPE, EXTENDS(xml_element_dt) :: Piece_dt
        PRIVATE
        CHARACTER(LEN=:), ALLOCATABLE :: type
        CHARACTER(LEN=:), ALLOCATABLE :: version
        CHARACTER(LEN=:), ALLOCATABLE :: byte_order
        CHARACTER(LEN=:), ALLOCATABLE :: compression
        CHARACTER(LEN=:), ALLOCATABLE :: file_extension
    CONTAINS
        PROCEDURE, NON_OVERRIDABLE :: vtk_element_setup
        PROCEDURE, NON_OVERRIDABLE, PUBLIC :: initialize
        PROCEDURE(abs_set_grid_data), DEFERRED :: set_grid_data
        GENERIC, PUBLIC :: set => set_grid_data
        PROCEDURE, NON_OVERRIDABLE, PUBLIC :: finalize
    END TYPE Piece_dt

    TYPE, EXTENDS(xml_element_dt) :: DataArray_dt
        PRIVATE
        CHARACTER(LEN=:), ALLOCATABLE :: type
        CHARACTER(LEN=:), ALLOCATABLE :: name
        CHARACTER(LEN=:), ALLOCATABLE :: NumberofComponents
        CHARACTER(LEN=:), ALLOCATABLE :: format
        CHARACTER(LEN=:), ALLOCATABLE :: offset
    END TYPE DataArray_dt

    TYPE, EXTENDS(xml_element_dt) :: PointData_dt
        CHARACTER(LEN=:), ALLOCATABLE :: Scalars
        CHARACTER(LEN=:), ALLOCATABLE :: Vectors
        CHARACTER(LEN=:), ALLOCATABLE :: Normals
        CHARACTER(LEN=:), ALLOCATABLE :: Tensors
        CHARACTER(LEN=:), ALLOCATABLE :: TCoords
    END TYPE PointData_dt

    TYPE, EXTENDS(xml_element_dt) :: CellData_dt
    END TYPE CellData_dt

    TYPE, EXTENDS(xml_element_dt) :: Points_dt
        TYPE(DataArray_dt) :: DataArray
    END TYPE Points_dt

    TYPE, EXTENDS(xml_element_dt) :: Coordinates_dt
        TYPE(DataArray_dt) :: DataArray_x
        TYPE(DataArray_dt) :: DataArray_y
        TYPE(DataArray_dt) :: DataArray_z
    END TYPE Coordinates_dt

    INTERFACE

        MODULE SUBROUTINE vtk_element_setup (me)
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 06/07/2019
        !!
        !! This converts the VTK_element_dt header into XML format
        !!
        CLASS(VTK_element_dt), INTENT(INOUT) :: me

        END SUBROUTINE vtk_element_setup

    END INTERFACE

END MODULE VTK_piece_element
