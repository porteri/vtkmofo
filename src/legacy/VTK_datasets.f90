MODULE VTK_datasets
    USE Precision, ONLY : i4k, r8k
    USE vtk_cells, ONLY : vtkcell, vtkcell_list
    IMPLICIT NONE
    !! author: Ian Porter
    !! date: 12/1/2017
    !!
    !! This module contains the dataset formats for vtk format
    !!
    !! The following dataset formats are available:
    !! 1) Structured points
    !! 2) Structured grid
    !! 3) Rectilinear grid
    !! 4) Polygonal data
    !! 5) Unstructured grid
    !!
    PRIVATE
    PUBLIC :: dataset, struct_pts, struct_grid, rectlnr_grid, polygonal_data, unstruct_grid

    TYPE :: coordinates
        CHARACTER(LEN=:),        ALLOCATABLE :: datatype
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: coord
    END TYPE coordinates

    TYPE, ABSTRACT :: dataset
        !! Abstract DT of dataset information
        PRIVATE
        CHARACTER(LEN=:), ALLOCATABLE :: name
        CHARACTER(LEN=:), ALLOCATABLE :: datatype
        INTEGER(i4k), DIMENSION(3)    :: dimensions = [ 0, 0, 0 ]
        LOGICAL, PUBLIC               :: firstcall = .TRUE.
    CONTAINS
        PROCEDURE(abs_read),  DEFERRED, PUBLIC :: read
        PROCEDURE(abs_write), DEFERRED, PUBLIC :: write
        PROCEDURE, NON_OVERRIDABLE, PUBLIC :: init
        PROCEDURE, PRIVATE :: check_for_diffs
        GENERIC, PUBLIC :: OPERATOR(.diff.) => check_for_diffs
        PROCEDURE, NON_OVERRIDABLE, PUBLIC :: get_range_cnt
        PROCEDURE, PUBLIC :: get_range
        PROCEDURE, PUBLIC :: get_coord
    END TYPE dataset

    TYPE, EXTENDS(dataset) :: struct_pts
        !! Structured points
        PRIVATE
        REAL(r8k), DIMENSION(3) :: origin  = [ 0.0_r8k, 0.0_r8k, 0.0_r8k ]
        REAL(r8k), DIMENSION(3) :: spacing = [ 0.0_r8k, 0.0_r8k, 0.0_r8k ]
    CONTAINS
        PROCEDURE :: read  => struct_pts_read
        PROCEDURE :: write => struct_pts_write
        PROCEDURE, PRIVATE :: setup => struct_pts_setup
        PROCEDURE :: check_for_diffs => struct_pts_check_for_diffs
    END TYPE struct_pts

    TYPE, EXTENDS(dataset) :: struct_grid
        !! Structured grid
        PRIVATE
        INTEGER(i4k), PUBLIC                   :: n_points = 0
        REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: points
    CONTAINS
        PROCEDURE :: read  => struct_grid_read
        PROCEDURE :: write => struct_grid_write
        PROCEDURE, PRIVATE :: setup => struct_grid_setup
        PROCEDURE :: check_for_diffs => struct_grid_check_for_diffs
        PROCEDURE :: get_point => struct_grid_get_point
    END TYPE struct_grid

    TYPE, EXTENDS(dataset) :: rectlnr_grid
        !! Rectilinear grid
        PRIVATE
        TYPE (coordinates) :: x
        TYPE (coordinates) :: y
        TYPE (coordinates) :: z
    CONTAINS
        PROCEDURE :: read  => rectlnr_grid_read
        PROCEDURE :: write => rectlnr_grid_write
        PROCEDURE, PRIVATE :: setup => rectlnr_grid_setup
        PROCEDURE :: check_for_diffs => rectlnr_grid_check_for_diffs
        PROCEDURE :: get_range => rectlnr_grid_get_range
        PROCEDURE :: get_coord => rectlnr_grid_get_coord
    END TYPE rectlnr_grid

    TYPE, EXTENDS(dataset) :: polygonal_data
        !! Polygonal data
        PRIVATE
        INTEGER(i4k)                                :: n_points = 0
        REAL(r8k),      DIMENSION(:,:), ALLOCATABLE :: points
        CLASS(vtkcell), DIMENSION(:),   ALLOCATABLE :: vertices
        CLASS(vtkcell), DIMENSION(:),   ALLOCATABLE :: lines
        CLASS(vtkcell), DIMENSION(:),   ALLOCATABLE :: polygons
        CLASS(vtkcell), DIMENSION(:),   ALLOCATABLE :: triangles
    CONTAINS
        PROCEDURE :: read  => polygonal_data_read
        PROCEDURE :: write => polygonal_data_write
        PROCEDURE, PRIVATE :: setup => polygonal_data_setup
    END TYPE polygonal_data

    TYPE, EXTENDS(dataset) :: unstruct_grid
        !! Unstructured grid
        PRIVATE
        INTEGER(i4k), PUBLIC :: n_points = 0
        INTEGER(i4k), PUBLIC :: n_cells  = 0
        INTEGER(i4k) :: n_cell_types = 0
        INTEGER(i4k) :: size         = 0
        REAL(r8k),          DIMENSION(:,:), ALLOCATABLE :: points
        TYPE(vtkcell_list), DIMENSION(:),   ALLOCATABLE :: cell_list
    CONTAINS
        PROCEDURE :: read  => unstruct_grid_read
        PROCEDURE :: write => unstruct_grid_write
        PROCEDURE :: unstruct_grid_setup
        PROCEDURE :: unstruct_grid_setup_multiclass
        GENERIC, PRIVATE :: setup => unstruct_grid_setup, unstruct_grid_setup_multiclass
        PROCEDURE :: get_point => unstruct_grid_get_point
        PROCEDURE :: get_connectivity => unstruct_grid_get_connectivity
        PROCEDURE :: get_offset => unstruct_grid_get_offset
        PROCEDURE :: get_type => unstruct_grid_get_type
    END TYPE unstruct_grid

    INTERFACE
! ****************
! Abstract dataset
! ****************
        MODULE SUBROUTINE abs_read (me, unit)
        IMPLICIT NONE
        !! Reads the dataset information from the .vtk file
        CLASS(dataset), INTENT(OUT) :: me
        INTEGER(i4k),   INTENT(IN)  :: unit

        END SUBROUTINE abs_read

        MODULE SUBROUTINE abs_write (me, unit)
        IMPLICIT NONE
        !! Writes the dataset information to the .vtk file
        CLASS(dataset), INTENT(IN) :: me
        INTEGER(i4k),   INTENT(IN) :: unit

        END SUBROUTINE abs_write

        MODULE SUBROUTINE init (me, datatype, dims, origin, spacing, points, cells, cell_list, &
          &                     x_coords, y_coords, z_coords, vertices, lines, polygons, triangles)
        IMPLICIT NONE
        !! Initializes the dataset
        CLASS (dataset),                     INTENT(OUT)          :: me
        CLASS(vtkcell),      DIMENSION(:),   INTENT(IN), OPTIONAL :: vertices
        CLASS(vtkcell),      DIMENSION(:),   INTENT(IN), OPTIONAL :: lines
        CLASS(vtkcell),      DIMENSION(:),   INTENT(IN), OPTIONAL :: polygons
        CLASS(vtkcell),      DIMENSION(:),   INTENT(IN), OPTIONAL :: triangles
        CLASS(vtkcell),      DIMENSION(:),   INTENT(IN), OPTIONAL :: cells      !! DT of same cell types
        TYPE(vtkcell_list),  DIMENSION(:),   INTENT(IN), OPTIONAL :: cell_list  !! DT of different cell types
        CHARACTER(LEN=*),                    INTENT(IN), OPTIONAL :: datatype   !! Type of data (floating, integer, etc.)
        INTEGER(i4k),        DIMENSION(3),   INTENT(IN), OPTIONAL :: dims
        REAL(r8k),           DIMENSION(3),   INTENT(IN), OPTIONAL :: origin
        REAL(r8k),           DIMENSION(3),   INTENT(IN), OPTIONAL :: spacing
        REAL(r8k),           DIMENSION(:),   INTENT(IN), OPTIONAL :: x_coords
        REAL(r8k),           DIMENSION(:),   INTENT(IN), OPTIONAL :: y_coords
        REAL(r8k),           DIMENSION(:),   INTENT(IN), OPTIONAL :: z_coords
        REAL(r8k),           DIMENSION(:,:), INTENT(IN), OPTIONAL :: points

        END SUBROUTINE init

        MODULE FUNCTION check_for_diffs (me, you) RESULT (diffs)
        IMPLICIT NONE
        !! Function checks for differences in a dataset
        CLASS(dataset), INTENT(IN) :: me, you
        LOGICAL :: diffs

        END FUNCTION check_for_diffs

        MODULE FUNCTION get_range_cnt (me) RESULT (range)
        IMPLICIT NONE
        !! Function returns the number of variables in x,y,z coordinates
        CLASS(dataset), INTENT(IN)   :: me
        INTEGER(i4k), DIMENSION(2,3) :: range

        END FUNCTION get_range_cnt

        MODULE FUNCTION get_range (me) RESULT (range)
        IMPLICIT NONE
        !! Function returns the min / max range of values in x,y,z coordinates
        CLASS(dataset), INTENT(IN) :: me
        REAL(r8k),  DIMENSION(2,3) :: range

        END FUNCTION get_range

        MODULE FUNCTION get_coord (me, dim) RESULT (coord)
        IMPLICIT NONE
        !! Function returns the min / max range of values in x,y,z coordinates
        CLASS(dataset), INTENT(IN) :: me
        INTEGER(i4k),   INTENT(IN) :: dim
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: coord

        END FUNCTION get_coord
! *****************
! Structured Points
! *****************
        MODULE SUBROUTINE struct_pts_read (me, unit)
        IMPLICIT NONE
        !! Reads the structured points dataset information from the .vtk file
        CLASS(struct_pts), INTENT(OUT) :: me
        INTEGER(i4k),      INTENT(IN)  :: unit

        END SUBROUTINE struct_pts_read

        MODULE SUBROUTINE struct_pts_write (me, unit)
        IMPLICIT NONE
        !! Writes the structured points dataset information to the .vtk file
        CLASS(struct_pts), INTENT(IN) :: me
        INTEGER(i4k),      INTENT(IN) :: unit

        END SUBROUTINE struct_pts_write

        MODULE SUBROUTINE struct_pts_setup (me, dims, origin, spacing)
        IMPLICIT NONE
        !! Sets up the structured points dataset with information
        CLASS (struct_pts),         INTENT(OUT) :: me
        INTEGER(i4k), DIMENSION(3), INTENT(IN)  :: dims
        REAL(r8k),    DIMENSION(3), INTENT(IN)  :: origin, spacing

        END SUBROUTINE struct_pts_setup

        MODULE FUNCTION struct_pts_check_for_diffs (me, you) RESULT (diffs)
        IMPLICIT NONE
        !! Function checks for differences in a structured points dataset
        CLASS(struct_pts), INTENT(IN) :: me
        CLASS(dataset),    INTENT(IN) :: you
        LOGICAL                       :: diffs

        END FUNCTION struct_pts_check_for_diffs

        MODULE FUNCTION struct_pts_get_range (me) RESULT (range)
        IMPLICIT NONE
        !! Function returns the min / max range of values in x,y,z coordinates
        CLASS(struct_pts), INTENT(IN) :: me
        INTEGER(i4k), DIMENSION(2,3)  :: range

        END FUNCTION struct_pts_get_range
! ***************
! Structured Grid
! ***************
        MODULE SUBROUTINE struct_grid_read (me, unit)
        IMPLICIT NONE
        !! Reads the structured grid dataset information from the .vtk file
        CLASS(struct_grid), INTENT(OUT) :: me
        INTEGER(i4k),       INTENT(IN)  :: unit

        END SUBROUTINE struct_grid_read

        MODULE SUBROUTINE struct_grid_write (me, unit)
        IMPLICIT NONE
        !! Writes the structured grid dataset information to the .vtk file
        CLASS(struct_grid), INTENT(IN) :: me
        INTEGER(i4k),       INTENT(IN) :: unit

        END SUBROUTINE struct_grid_write

        MODULE SUBROUTINE struct_grid_setup (me, dims, points)
        IMPLICIT NONE
        !! Sets up the structured grid dataset with information
        CLASS (struct_grid),          INTENT(OUT) :: me
        INTEGER(i4k), DIMENSION(3),   INTENT(IN)  :: dims
        REAL(r8k),    DIMENSION(:,:), INTENT(IN)  :: points

        END SUBROUTINE struct_grid_setup

        MODULE FUNCTION struct_grid_check_for_diffs (me, you) RESULT (diffs)
        IMPLICIT NONE
        !! Function checks for differences in a structured grid dataset
        CLASS(struct_grid), INTENT(IN) :: me
        CLASS(dataset),     INTENT(IN) :: you
        LOGICAL                        :: diffs

        END FUNCTION struct_grid_check_for_diffs

        MODULE FUNCTION struct_grid_get_point (me, i) RESULT (coord)
        IMPLICIT NONE
        !! Function returns the (x,y,z) dimension coordinates for point (i)
        CLASS(struct_grid), INTENT(IN) :: me
        INTEGER(i4k),       INTENT(IN) :: i
        REAL(r8k), DIMENSION(3)        :: coord

        END FUNCTION struct_grid_get_point

        MODULE FUNCTION struct_grid_get_range (me) RESULT (range)
        IMPLICIT NONE
        !! Function returns the min / max range of values in x,y,z coordinates
        CLASS(struct_grid), INTENT(IN) :: me
        INTEGER(i4k), DIMENSION(2,3)   :: range

        END FUNCTION struct_grid_get_range
! ****************
! Rectilinear Grid
! ****************
        MODULE SUBROUTINE rectlnr_grid_read (me, unit)
        IMPLICIT NONE
        !! Reads the rectilinear grid dataset information from the .vtk file
        CLASS(rectlnr_grid), INTENT(OUT) :: me
        INTEGER(i4k),        INTENT(IN)  :: unit

        END SUBROUTINE rectlnr_grid_read

        MODULE SUBROUTINE rectlnr_grid_write (me, unit)
        IMPLICIT NONE
        !! Writes the rectilinear grid dataset information to the .vtk file
        CLASS(rectlnr_grid), INTENT(IN) :: me
        INTEGER(i4k),        INTENT(IN) :: unit

        END SUBROUTINE rectlnr_grid_write

        MODULE SUBROUTINE rectlnr_grid_setup (me, dims, x_coords, y_coords, z_coords, datatype)
        IMPLICIT NONE
        !! Sets up the rectilinear grid dataset with information
        CLASS (rectlnr_grid),       INTENT(OUT) :: me         !! Rectilinear grid DT
        INTEGER(i4k), DIMENSION(3), INTENT(IN)  :: dims       !! # of dimensions in (x,y,z) direction
        REAL(r8k),    DIMENSION(:), INTENT(IN)  :: x_coords   !! X coordinates
        REAL(r8k),    DIMENSION(:), INTENT(IN)  :: y_coords   !! Y coordinates
        REAL(r8k),    DIMENSION(:), INTENT(IN)  :: z_coords   !! Z coordinates
        CHARACTER(LEN=*),           INTENT(IN)  :: datatype   !! Type of data (floating, integer, etc.)

        END SUBROUTINE rectlnr_grid_setup

        MODULE FUNCTION rectlnr_grid_check_for_diffs (me, you) RESULT (diffs)
        IMPLICIT NONE
        !! Function checks for differences in a rectilinear grid dataset
        CLASS(rectlnr_grid), INTENT(IN) :: me
        CLASS(dataset),      INTENT(IN) :: you
        LOGICAL                         :: diffs

        END FUNCTION rectlnr_grid_check_for_diffs

        MODULE FUNCTION rectlnr_grid_get_range (me) RESULT (range)
        IMPLICIT NONE
        !! Function returns the min / max range of values in x,y,z coordinates
        CLASS(rectlnr_grid), INTENT(IN) :: me
        REAL(r8k), DIMENSION(2,3)       :: range

        END FUNCTION rectlnr_grid_get_range

        MODULE FUNCTION rectlnr_grid_get_coord (me, dim) RESULT (coord)
        IMPLICIT NONE
        !! Function returns the min / max range of values in x,y,z coordinates
        CLASS(rectlnr_grid), INTENT(IN) :: me
        INTEGER(i4k),        INTENT(IN) :: dim
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: coord

        END FUNCTION rectlnr_grid_get_coord
! **************
! Polygonal Data
! **************
        MODULE SUBROUTINE polygonal_data_read (me, unit)
        IMPLICIT NONE
        !! Reads the polygonal data dataset information from the .vtk file
        CLASS(polygonal_data), INTENT(OUT) :: me
        INTEGER(i4k),          INTENT(IN)  :: unit

        END SUBROUTINE polygonal_data_read

        MODULE SUBROUTINE polygonal_data_write (me, unit)
        IMPLICIT NONE
        !! Writes the polygonal data dataset information to the .vtk file
        CLASS(polygonal_data), INTENT(IN) :: me
        INTEGER(i4k),          INTENT(IN) :: unit

        END SUBROUTINE polygonal_data_write

        MODULE SUBROUTINE polygonal_data_setup (me, points, vertices, lines, polygons, triangles)
        IMPLICIT NONE
        !! Sets up the polygonal data dataset with information
        CLASS (polygonal_data),       INTENT(OUT)          :: me
        REAL(r8k),    DIMENSION(:,:), INTENT(IN)           :: points
        CLASS(vtkcell), DIMENSION(:), INTENT(IN), OPTIONAL :: vertices
        CLASS(vtkcell), DIMENSION(:), INTENT(IN), OPTIONAL :: lines
        CLASS(vtkcell), DIMENSION(:), INTENT(IN), OPTIONAL :: polygons
        CLASS(vtkcell), DIMENSION(:), INTENT(IN), OPTIONAL :: triangles

        END SUBROUTINE polygonal_data_setup

        MODULE FUNCTION polygonal_data_get_range (me) RESULT (range)
        IMPLICIT NONE
        !! Function returns the polygonal data min / max range of values in x,y,z coordinates
        CLASS(polygonal_data), INTENT(IN) :: me
        INTEGER(i4k), DIMENSION(2,3)      :: range

        END FUNCTION polygonal_data_get_range
! *****************
! Unstructured Grid
! *****************
        MODULE SUBROUTINE unstruct_grid_read (me, unit)
        IMPLICIT NONE
        !! Reads the unstructured grid dataset information from the .vtk file
        CLASS(unstruct_grid), INTENT(OUT) :: me
        INTEGER(i4k),         INTENT(IN)  :: unit

        END SUBROUTINE unstruct_grid_read

        MODULE SUBROUTINE unstruct_grid_write (me, unit)
        IMPLICIT NONE
        !! Writes the unstructured grid dataset information from the .vtk file
        CLASS(unstruct_grid), INTENT(IN) :: me
        INTEGER(i4k),         INTENT(IN) :: unit

        END SUBROUTINE unstruct_grid_write

        MODULE SUBROUTINE unstruct_grid_setup (me, points, cells)
        IMPLICIT NONE
        !! Sets up the unstructured grid dataset with information for a single class of cells
        CLASS(unstruct_grid),           INTENT(OUT) :: me      !! DT
        REAL(r8k),      DIMENSION(:,:), INTENT(IN)  :: points  !!
        CLASS(vtkcell), DIMENSION(:),   INTENT(IN)  :: cells   !! DT of same cell types

        END SUBROUTINE unstruct_grid_setup

        MODULE SUBROUTINE unstruct_grid_setup_multiclass (me, points, cell_list)
        IMPLICIT NONE
        !! Sets up the unstructured grid dataset with information for a list of different classes of cells
        CLASS(unstruct_grid),               INTENT(OUT) :: me         !! DT
        REAL(r8k),          DIMENSION(:,:), INTENT(IN)  :: points     !!
        TYPE(vtkcell_list), DIMENSION(:),   INTENT(IN)  :: cell_list  !! DT of different cell types

        END SUBROUTINE unstruct_grid_setup_multiclass

        MODULE FUNCTION unstruct_grid_get_range (me) RESULT (range)
        IMPLICIT NONE
        !! Function returns the unstructured grid min / max range of values in x,y,z coordinates
        CLASS(unstruct_grid), INTENT(IN) :: me
        INTEGER(i4k), DIMENSION(2,3)     :: range

        END FUNCTION unstruct_grid_get_range

        MODULE FUNCTION unstruct_grid_get_point (me, i) RESULT (coord)
        IMPLICIT NONE
        !! Function returns the (x,y,z) dimension coordinates for point (i)
        CLASS(unstruct_grid), INTENT(IN) :: me
        INTEGER(i4k),         INTENT(IN) :: i
        REAL(r8k), DIMENSION(3)          :: coord

        END FUNCTION unstruct_grid_get_point

        MODULE FUNCTION unstruct_grid_get_connectivity (me, i) RESULT (connectivity)
        IMPLICIT NONE
        !! Function returns the connectivity for cell (i)
        CLASS(unstruct_grid), INTENT(IN) :: me
        INTEGER(i4k),         INTENT(IN) :: i
        INTEGER(i4k), DIMENSION(:), ALLOCATABLE :: connectivity

        END FUNCTION unstruct_grid_get_connectivity

        MODULE FUNCTION unstruct_grid_get_offset (me, i) RESULT (offset)
        IMPLICIT NONE
        !! Function returns the offset for cell (i)
        !! The offset is just the # of points
        CLASS(unstruct_grid), INTENT(IN) :: me
        INTEGER(i4k),         INTENT(IN) :: i
        INTEGER(i4k) :: offset

        END FUNCTION unstruct_grid_get_offset

        MODULE FUNCTION unstruct_grid_get_type (me, i) RESULT (type)
        IMPLICIT NONE
        !! Function returns the type for cell (i)
        CLASS(unstruct_grid), INTENT(IN) :: me
        INTEGER(i4k),         INTENT(IN) :: i
        INTEGER(i4k) :: type

        END FUNCTION unstruct_grid_get_type

    END INTERFACE

END MODULE VTK_datasets
