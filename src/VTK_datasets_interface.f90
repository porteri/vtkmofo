MODULE vtk_datasets
    USE Precision
    USE vtk_cells, ONLY : vtkcell
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
        CHARACTER(LEN=:), ALLOCATABLE :: datatype
        REAL(r8k),        DIMENSION(:), ALLOCATABLE :: coord
    END TYPE coordinates

    TYPE, ABSTRACT :: dataset
        PRIVATE
        CHARACTER(LEN=:), ALLOCATABLE  :: name
        INTEGER(i4k),     DIMENSION(3) :: dimensions
        LOGICAL, PUBLIC                :: firstcall = .TRUE.
    CONTAINS
        PROCEDURE(abs_read),  DEFERRED, PUBLIC :: read
        PROCEDURE(abs_write), DEFERRED, PUBLIC :: write
        PROCEDURE(abs_setup), DEFERRED, PUBLIC :: setup
        PROCEDURE, PRIVATE :: check_for_diffs
        GENERIC, PUBLIC :: OPERATOR(.diff.) => check_for_diffs
    END TYPE dataset

    TYPE, EXTENDS(dataset) :: struct_pts
        !! Structured points
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
        INTEGER(i4k)                           :: n_points
        CHARACTER(LEN=:),          ALLOCATABLE :: datatype
        REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: points
    CONTAINS
        PROCEDURE :: read  => struct_grid_read
        PROCEDURE :: write => struct_grid_write
        PROCEDURE :: setup => struct_grid_setup
        PROCEDURE :: check_for_diffs => check_for_diffs_struct_grid
    END TYPE struct_grid

    TYPE, EXTENDS(dataset) :: rectlnr_grid
        !! Rectilinear grid
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
        INTEGER(i4k)                                :: n_points
        CHARACTER(LEN=:),               ALLOCATABLE :: datatype
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
        INTEGER(i4k) :: n_points
        INTEGER(i4k) :: n_cells
        INTEGER(i4k) :: n_cell_types
        INTEGER(i4k) :: size
        CHARACTER(LEN=:),               ALLOCATABLE :: datatype
        REAL(r8k),      DIMENSION(:,:), ALLOCATABLE :: points
        CLASS(vtkcell), DIMENSION(:),   ALLOCATABLE :: cell
    CONTAINS
        PROCEDURE :: read  => unstruct_grid_read
        PROCEDURE :: write => unstruct_grid_write
        PROCEDURE :: setup => unstruct_grid_setup
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

        MODULE SUBROUTINE abs_setup (me, datatype, dims, origin, spacing, points, cells, &
          &                   x_coords, y_coords, z_coords, vertices, lines, polygons, triangles)
        !>@brief
        !> Sets up the dataset with information
        CLASS (dataset),              INTENT(OUT)          :: me
        CLASS(vtkcell), DIMENSION(:), INTENT(IN), OPTIONAL :: vertices, lines, polygons, triangles, cells
        CHARACTER(LEN=*),             INTENT(IN), OPTIONAL :: datatype
        INTEGER(i4k), DIMENSION(3),   INTENT(IN), OPTIONAL :: dims
        REAL(r8k),    DIMENSION(3),   INTENT(IN), OPTIONAL :: origin, spacing
        REAL(r8k),    DIMENSION(:),   INTENT(IN), OPTIONAL :: x_coords, y_coords, z_coords
        REAL(r8k),    DIMENSION(:,:), INTENT(IN), OPTIONAL :: points

        END SUBROUTINE abs_setup

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

        MODULE SUBROUTINE struct_pts_setup (me, datatype, dims, origin, spacing, points, cells, &
          &                          x_coords, y_coords, z_coords, vertices, lines, polygons, triangles)
        !>@brief
        !> Sets up the structured points dataset with information
        CLASS (struct_pts),           INTENT(OUT)          :: me
        CLASS(vtkcell), DIMENSION(:), INTENT(IN), OPTIONAL :: vertices, lines, polygons, triangles, cells
        CHARACTER(LEN=*),             INTENT(IN), OPTIONAL :: datatype
        INTEGER(i4k), DIMENSION(3),   INTENT(IN), OPTIONAL :: dims
        REAL(r8k),    DIMENSION(3),   INTENT(IN), OPTIONAL :: origin, spacing
        REAL(r8k),    DIMENSION(:),   INTENT(IN), OPTIONAL :: x_coords, y_coords, z_coords
        REAL(r8k),    DIMENSION(:,:), INTENT(IN), OPTIONAL :: points

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

        MODULE SUBROUTINE struct_grid_setup (me, datatype, dims, origin, spacing, points, cells, &
          &                           x_coords, y_coords, z_coords, vertices, lines, polygons, triangles)
        !>@brief
        !> Sets up the structured grid dataset with information
        CLASS (struct_grid),          INTENT(OUT)          :: me
        CLASS(vtkcell), DIMENSION(:), INTENT(IN), OPTIONAL :: vertices, lines, polygons, triangles, cells
        CHARACTER(LEN=*),             INTENT(IN), OPTIONAL :: datatype
        INTEGER(i4k), DIMENSION(3),   INTENT(IN), OPTIONAL :: dims
        REAL(r8k),    DIMENSION(3),   INTENT(IN), OPTIONAL :: origin, spacing
        REAL(r8k),    DIMENSION(:),   INTENT(IN), OPTIONAL :: x_coords, y_coords, z_coords
        REAL(r8k),    DIMENSION(:,:), INTENT(IN), OPTIONAL :: points

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

        MODULE SUBROUTINE rectlnr_grid_setup (me, datatype, dims, origin, spacing, points, cells, &
          &                            x_coords, y_coords, z_coords, vertices, lines, polygons, triangles)
        !>@brief
        !> Sets up the rectilinear grid dataset with information
        CLASS (rectlnr_grid),         INTENT(OUT)          :: me
        CLASS(vtkcell), DIMENSION(:), INTENT(IN), OPTIONAL :: vertices, lines, polygons, triangles, cells
        CHARACTER(LEN=*),             INTENT(IN), OPTIONAL :: datatype
        INTEGER(i4k), DIMENSION(3),   INTENT(IN), OPTIONAL :: dims
        REAL(r8k),    DIMENSION(3),   INTENT(IN), OPTIONAL :: origin, spacing
        REAL(r8k),    DIMENSION(:),   INTENT(IN), OPTIONAL :: x_coords, y_coords, z_coords
        REAL(r8k),    DIMENSION(:,:), INTENT(IN), OPTIONAL :: points

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

        MODULE SUBROUTINE polygonal_data_setup (me, datatype, dims, origin, spacing, points, cells, &
          &                              x_coords, y_coords, z_coords, vertices, lines, polygons, triangles)
        !>@brief
        !> Sets up the polygonal data dataset with information
        CLASS (polygonal_data),       INTENT(OUT)          :: me
        CLASS(vtkcell), DIMENSION(:), INTENT(IN), OPTIONAL :: vertices, lines, polygons, triangles, cells
        CHARACTER(LEN=*),             INTENT(IN), OPTIONAL :: datatype
        INTEGER(i4k), DIMENSION(3),   INTENT(IN), OPTIONAL :: dims
        REAL(r8k),    DIMENSION(3),   INTENT(IN), OPTIONAL :: origin, spacing
        REAL(r8k),    DIMENSION(:),   INTENT(IN), OPTIONAL :: x_coords, y_coords, z_coords
        REAL(r8k),    DIMENSION(:,:), INTENT(IN), OPTIONAL :: points

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

        MODULE SUBROUTINE unstruct_grid_setup (me, datatype, dims, origin, spacing, points, cells, &
          &                             x_coords, y_coords, z_coords, vertices, lines, polygons, triangles)
        !>@brief
        !> Sets up the unstructured grid dataset with information
        CLASS (unstruct_grid),        INTENT(OUT)          :: me
        CLASS(vtkcell), DIMENSION(:), INTENT(IN), OPTIONAL :: vertices, lines, polygons, triangles, cells
        CHARACTER(LEN=*),             INTENT(IN), OPTIONAL :: datatype
        INTEGER(i4k), DIMENSION(3),   INTENT(IN), OPTIONAL :: dims
        REAL(r8k),    DIMENSION(3),   INTENT(IN), OPTIONAL :: origin, spacing
        REAL(r8k),    DIMENSION(:),   INTENT(IN), OPTIONAL :: x_coords, y_coords, z_coords
        REAL(r8k),    DIMENSION(:,:), INTENT(IN), OPTIONAL :: points

        END SUBROUTINE unstruct_grid_setup

    END INTERFACE

END MODULE vtk_datasets
