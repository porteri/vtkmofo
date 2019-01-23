MODULE vtk_datasets
    USE Precision
    USE vtk_cells, ONLY : vtkcell, vtkcell_list
    IMPLICIT NONE
    !>@brief
    !> This module contains the dataset formats for vtk format
    !>@author
    !> Ian Porter
    !>@date
    !> 12/1/2017
    !
    ! The following dataset formats are available:
    ! 1) Structured points
    ! 2) Structured grid
    ! 3) Rectilinear grid
    ! 4) Polygonal data
    ! 5) Unstructured grid
    !

    PRIVATE
    PUBLIC :: dataset, struct_pts, struct_grid, rectlnr_grid, polygonal_data, unstruct_grid

    TYPE :: coordinates
        CHARACTER(LEN=:),        ALLOCATABLE :: datatype
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: coord
    END TYPE coordinates

    TYPE, ABSTRACT :: dataset
        PRIVATE
        CHARACTER(LEN=:), ALLOCATABLE :: name
        CHARACTER(LEN=:), ALLOCATABLE :: datatype
        INTEGER(i4k), DIMENSION(3)    :: dimensions
        LOGICAL, PUBLIC               :: firstcall = .TRUE.
    CONTAINS
        PROCEDURE(abs_read),  DEFERRED, PUBLIC :: read
        PROCEDURE(abs_write), DEFERRED, PUBLIC :: write
        PROCEDURE, NON_OVERRIDABLE, PUBLIC :: init
        PROCEDURE, PRIVATE :: check_for_diffs
        GENERIC, PUBLIC :: OPERATOR(.diff.) => check_for_diffs
    END TYPE dataset

    TYPE, EXTENDS(dataset) :: struct_pts
        !! Structured points
        PRIVATE
        REAL(r8k), DIMENSION(3) :: origin
        REAL(r8k), DIMENSION(3) :: spacing
    CONTAINS
        PROCEDURE :: read  => struct_pts_read
        PROCEDURE :: write => struct_pts_write
        PROCEDURE :: setup => struct_pts_setup
        PROCEDURE :: check_for_diffs => check_for_diffs_struct_pts
    END TYPE struct_pts

    TYPE, EXTENDS(dataset) :: struct_grid
        !! Structured grid
        PRIVATE
        INTEGER(i4k)                           :: n_points
        REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: points
    CONTAINS
        PROCEDURE :: read  => struct_grid_read
        PROCEDURE :: write => struct_grid_write
        PROCEDURE :: setup => struct_grid_setup
        PROCEDURE :: check_for_diffs => check_for_diffs_struct_grid
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
        PROCEDURE :: setup => rectlnr_grid_setup
        PROCEDURE :: check_for_diffs => check_for_diffs_rectlnr_grid
    END TYPE rectlnr_grid

    TYPE, EXTENDS(dataset) :: polygonal_data
        !! Polygonal data
        PRIVATE
        INTEGER(i4k)                                :: n_points
        REAL(r8k),      DIMENSION(:,:), ALLOCATABLE :: points
        CLASS(vtkcell), DIMENSION(:),   ALLOCATABLE :: vertices
        CLASS(vtkcell), DIMENSION(:),   ALLOCATABLE :: lines
        CLASS(vtkcell), DIMENSION(:),   ALLOCATABLE :: polygons
        CLASS(vtkcell), DIMENSION(:),   ALLOCATABLE :: triangles
    CONTAINS
        PROCEDURE :: read  => polygonal_data_read
        PROCEDURE :: write => polygonal_data_write
        PROCEDURE :: setup => polygonal_data_setup
    END TYPE polygonal_data

    TYPE, EXTENDS(dataset) :: unstruct_grid
        !! Unstructured grid
        PRIVATE
        INTEGER(i4k) :: n_points
        INTEGER(i4k) :: n_cells
        INTEGER(i4k) :: n_cell_types
        INTEGER(i4k) :: size
        REAL(r8k),          DIMENSION(:,:), ALLOCATABLE :: points
        TYPE(vtkcell_list), DIMENSION(:),   ALLOCATABLE :: cell_list
    CONTAINS
        PROCEDURE :: read  => unstruct_grid_read
        PROCEDURE :: write => unstruct_grid_write
        PROCEDURE :: unstruct_grid_setup
        PROCEDURE :: unstruct_grid_setup_multiclass
        GENERIC   :: setup => unstruct_grid_setup, unstruct_grid_setup_multiclass
    END TYPE unstruct_grid

    INTERFACE
! ****************
! Abstract dataset
! ****************
        MODULE SUBROUTINE abs_read (me, unit)
        !>@brief
        !> Reads the dataset information from the .vtk file
        CLASS(dataset), INTENT(OUT) :: me
        INTEGER(i4k),   INTENT(IN)  :: unit

        END SUBROUTINE abs_read

        MODULE SUBROUTINE abs_write (me, unit)
        !>@brief
        !> Writes the dataset information to the .vtk file
        CLASS(dataset), INTENT(IN) :: me
        INTEGER(i4k),   INTENT(IN) :: unit

        END SUBROUTINE abs_write

        MODULE SUBROUTINE init (me, datatype, dims, origin, spacing, points, cells, cell_list, &
          &                     x_coords, y_coords, z_coords, vertices, lines, polygons, triangles)
        !>@brief
        !> initializes the dataset
        CLASS (dataset),                     INTENT(OUT)          :: me
        CLASS(vtkcell),      DIMENSION(:),   INTENT(IN), OPTIONAL :: vertices
        CLASS(vtkcell),      DIMENSION(:),   INTENT(IN), OPTIONAL :: lines
        CLASS(vtkcell),      DIMENSION(:),   INTENT(IN), OPTIONAL :: polygons
        CLASS(vtkcell),      DIMENSION(:),   INTENT(IN), OPTIONAL :: triangles
        CLASS(vtkcell),      DIMENSION(:),   INTENT(IN), OPTIONAL :: cells      !! DT of same cell types
        TYPE(vtkcell_list),  DIMENSION(:),   INTENT(IN), OPTIONAL :: cell_list  !! DT of different cell types
        CHARACTER(LEN=*),                    INTENT(IN), OPTIONAL :: datatype
        INTEGER(i4k),        DIMENSION(3),   INTENT(IN), OPTIONAL :: dims
        REAL(r8k),           DIMENSION(3),   INTENT(IN), OPTIONAL :: origin
        REAL(r8k),           DIMENSION(3),   INTENT(IN), OPTIONAL :: spacing
        REAL(r8k),           DIMENSION(:),   INTENT(IN), OPTIONAL :: x_coords
        REAL(r8k),           DIMENSION(:),   INTENT(IN), OPTIONAL :: y_coords
        REAL(r8k),           DIMENSION(:),   INTENT(IN), OPTIONAL :: z_coords
        REAL(r8k),           DIMENSION(:,:), INTENT(IN), OPTIONAL :: points

        END SUBROUTINE init

        MODULE FUNCTION check_for_diffs (me, you) RESULT (diffs)
        !>@brief
        !> Function checks for differences in a dataset
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 12/18/2017
        CLASS(dataset), INTENT(IN) :: me, you
        LOGICAL :: diffs

        END FUNCTION check_for_diffs
! *****************
! Structured Points
! *****************
        MODULE SUBROUTINE struct_pts_read (me, unit)
        !>@brief
        !> Reads the structured points dataset information from the .vtk file
        CLASS(struct_pts), INTENT(OUT) :: me
        INTEGER(i4k),      INTENT(IN)  :: unit

        END SUBROUTINE struct_pts_read

        MODULE SUBROUTINE struct_pts_write (me, unit)
        !>@brief
        !> Writes the structured points dataset information to the .vtk file
        CLASS(struct_pts), INTENT(IN) :: me
        INTEGER(i4k),      INTENT(IN) :: unit

        END SUBROUTINE struct_pts_write

        MODULE SUBROUTINE struct_pts_setup (me, dims, origin, spacing)
        !>@brief
        !> Sets up the structured points dataset with information
        CLASS (struct_pts),         INTENT(OUT) :: me
        INTEGER(i4k), DIMENSION(3), INTENT(IN)  :: dims
        REAL(r8k),    DIMENSION(3), INTENT(IN)  :: origin, spacing

        END SUBROUTINE struct_pts_setup

        MODULE FUNCTION check_for_diffs_struct_pts (me, you) RESULT (diffs)
        !>@brief
        !> Function checks for differences in a structured points dataset
        CLASS(struct_pts), INTENT(IN) :: me
        CLASS(dataset),    INTENT(IN) :: you
        LOGICAL                       :: diffs

        END FUNCTION check_for_diffs_struct_pts
! ***************
! Structured Grid
! ***************
        MODULE SUBROUTINE struct_grid_read (me, unit)
        !>@brief
        !> Reads the structured grid dataset information from the .vtk file
        CLASS(struct_grid), INTENT(OUT) :: me
        INTEGER(i4k),       INTENT(IN)  :: unit

        END SUBROUTINE struct_grid_read

        MODULE SUBROUTINE struct_grid_write (me, unit)
        !>@brief
        !> Writes the structured grid dataset information to the .vtk file
        CLASS(struct_grid), INTENT(IN) :: me
        INTEGER(i4k),       INTENT(IN) :: unit

        END SUBROUTINE struct_grid_write

        MODULE SUBROUTINE struct_grid_setup (me, dims, points)
        !>@brief
        !> Sets up the structured grid dataset with information
        CLASS (struct_grid),          INTENT(OUT) :: me
        INTEGER(i4k), DIMENSION(3),   INTENT(IN)  :: dims
        REAL(r8k),    DIMENSION(:,:), INTENT(IN)  :: points

        END SUBROUTINE struct_grid_setup

        MODULE FUNCTION check_for_diffs_struct_grid (me, you) RESULT (diffs)
        !>@brief
        !> Function checks for differences in a structured grid dataset
        CLASS(struct_grid), INTENT(IN) :: me
        CLASS(dataset),     INTENT(IN) :: you
        LOGICAL                        :: diffs

        END FUNCTION check_for_diffs_struct_grid
! ****************
! Rectilinear Grid
! ****************
        MODULE SUBROUTINE rectlnr_grid_read (me, unit)
        !>@brief
        !> Reads the rectilinear grid dataset information from the .vtk file
        CLASS(rectlnr_grid), INTENT(OUT) :: me
        INTEGER(i4k),        INTENT(IN)  :: unit

        END SUBROUTINE rectlnr_grid_read

        MODULE SUBROUTINE rectlnr_grid_write (me, unit)
        !>@brief
        !> Writes the rectilinear grid dataset information to the .vtk file
        CLASS(rectlnr_grid), INTENT(IN) :: me
        INTEGER(i4k),        INTENT(IN) :: unit

        END SUBROUTINE rectlnr_grid_write

        MODULE SUBROUTINE rectlnr_grid_setup (me, dims, x_coords, y_coords, z_coords)
        !>@brief
        !> Sets up the rectilinear grid dataset with information
        CLASS (rectlnr_grid),       INTENT(OUT) :: me
        INTEGER(i4k), DIMENSION(3), INTENT(IN)  :: dims
        REAL(r8k),    DIMENSION(:), INTENT(IN)  :: x_coords
        REAL(r8k),    DIMENSION(:), INTENT(IN)  :: y_coords
        REAL(r8k),    DIMENSION(:), INTENT(IN)  :: z_coords

        END SUBROUTINE rectlnr_grid_setup

        MODULE FUNCTION check_for_diffs_rectlnr_grid (me, you) RESULT (diffs)
        !>@brief
        !> Function checks for differences in a rectilinear grid dataset
        CLASS(rectlnr_grid), INTENT(IN) :: me
        CLASS(dataset),      INTENT(IN) :: you
        LOGICAL                         :: diffs

        END FUNCTION check_for_diffs_rectlnr_grid
! **************
! Polygonal Data
! **************
        MODULE SUBROUTINE polygonal_data_read (me, unit)
        !>@brief
        !> Reads the polygonal data dataset information from the .vtk file
        CLASS(polygonal_data), INTENT(OUT) :: me
        INTEGER(i4k),          INTENT(IN)  :: unit

        END SUBROUTINE polygonal_data_read

        MODULE SUBROUTINE polygonal_data_write (me, unit)
        !>@brief
        !> Writes the polygonal data dataset information to the .vtk file
        CLASS(polygonal_data), INTENT(IN) :: me
        INTEGER(i4k),          INTENT(IN) :: unit

        END SUBROUTINE polygonal_data_write

        MODULE SUBROUTINE polygonal_data_setup (me, points, vertices, lines, polygons, triangles)
        !>@brief
        !> Sets up the polygonal data dataset with information
        CLASS (polygonal_data),       INTENT(OUT)          :: me
        REAL(r8k),    DIMENSION(:,:), INTENT(IN)           :: points
        CLASS(vtkcell), DIMENSION(:), INTENT(IN), OPTIONAL :: vertices
        CLASS(vtkcell), DIMENSION(:), INTENT(IN), OPTIONAL :: lines
        CLASS(vtkcell), DIMENSION(:), INTENT(IN), OPTIONAL :: polygons
        CLASS(vtkcell), DIMENSION(:), INTENT(IN), OPTIONAL :: triangles

        END SUBROUTINE polygonal_data_setup
! *****************
! Unstructured Grid
! *****************
        MODULE SUBROUTINE unstruct_grid_read (me, unit)
        !>@brief
        !> Reads the unstructured grid dataset information from the .vtk file
        CLASS(unstruct_grid), INTENT(OUT) :: me
        INTEGER(i4k),         INTENT(IN)  :: unit

        END SUBROUTINE unstruct_grid_read

        MODULE SUBROUTINE unstruct_grid_write (me, unit)
        !>@brief
        !> Writes the unstructured grid dataset information from the .vtk file
        CLASS(unstruct_grid), INTENT(IN) :: me
        INTEGER(i4k),         INTENT(IN) :: unit

        END SUBROUTINE unstruct_grid_write

        MODULE SUBROUTINE unstruct_grid_setup (me, points, cells)
        !! Sets up the unstructured grid dataset with information for a single class of cells
        !!
        CLASS(unstruct_grid),           INTENT(OUT) :: me      !! DT
        REAL(r8k),      DIMENSION(:,:), INTENT(IN)  :: points  !! 
        CLASS(vtkcell), DIMENSION(:),   INTENT(IN)  :: cells   !! DT of same cell types

        END SUBROUTINE unstruct_grid_setup

        MODULE SUBROUTINE unstruct_grid_setup_multiclass (me, points, cell_list)
        !! Sets up the unstructured grid dataset with information for a list of different classes of cells
        !!
        CLASS(unstruct_grid),               INTENT(OUT) :: me         !! DT
        REAL(r8k),          DIMENSION(:,:), INTENT(IN)  :: points     !! 
        TYPE(vtkcell_list), DIMENSION(:),   INTENT(IN)  :: cell_list  !! DT of different cell types

        END SUBROUTINE unstruct_grid_setup_multiclass

    END INTERFACE

END MODULE vtk_datasets
