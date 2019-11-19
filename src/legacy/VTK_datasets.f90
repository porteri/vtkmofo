module vtk_datasets
    use precision, only : i4k, r8k
    use vtk_cells, only : vtkcell, vtkcell_list
    implicit none
    !! author: Ian Porter
    !! date: 12/1/2017
    !!
    !! this module contains the dataset formats for vtk format
    !!
    !! the following dataset formats are available:
    !! 1) structured points
    !! 2) structured grid
    !! 3) rectilinear grid
    !! 4) polygonal data
    !! 5) unstructured grid
    !!
    private
    public :: dataset, struct_pts, struct_grid, rectlnr_grid, polygonal_data, unstruct_grid

    type :: coordinates
        !! Coordinates DT
        character(len=:),        allocatable :: datatype
        real(r8k), dimension(:), allocatable :: coord
    end type coordinates

    type, abstract :: dataset
        !! abstract dt of dataset information
        private
        character(len=:), allocatable :: name
        character(len=:), allocatable :: datatype
        integer(i4k), dimension(3)    :: dimensions = [ 0, 0, 0 ]
        logical, public               :: firstcall = .true.
    contains
        procedure(abs_read),  deferred, public :: read
        procedure(abs_write), deferred, public :: write
        procedure, non_overridable, public :: init
        procedure, private :: check_for_diffs
        generic, public :: operator(.diff.) => check_for_diffs
        procedure, non_overridable, public :: get_range_cnt
        procedure, public :: get_range
        procedure, public :: get_coord
    end type dataset

    type, extends(dataset) :: struct_pts
        !! structured points
        private
        real(r8k), dimension(3) :: origin  = [ 0.0_r8k, 0.0_r8k, 0.0_r8k ]
        real(r8k), dimension(3) :: spacing = [ 0.0_r8k, 0.0_r8k, 0.0_r8k ]
    contains
        procedure :: read  => struct_pts_read
        procedure :: write => struct_pts_write
        procedure :: struct_pts_get_origin
        procedure :: struct_pts_get_spacing
        generic, public :: get_origin => struct_pts_get_origin
        generic, public :: get_spacing => struct_pts_get_spacing
        procedure :: setup => struct_pts_setup
        procedure :: check_for_diffs => struct_pts_check_for_diffs
    end type struct_pts

    type, extends(dataset) :: struct_grid
        !! structured grid
        private
        integer(i4k), public                   :: n_points = 0
        real(r8k), dimension(:,:), allocatable :: points
    contains
        procedure :: read  => struct_grid_read
        procedure :: write => struct_grid_write
        procedure, private :: setup => struct_grid_setup
        procedure :: check_for_diffs => struct_grid_check_for_diffs
        procedure :: get_point => struct_grid_get_point
    end type struct_grid

    type, extends(dataset) :: rectlnr_grid
        !! rectilinear grid
        private
        type (coordinates) :: x
        type (coordinates) :: y
        type (coordinates) :: z
    contains
        procedure :: read  => rectlnr_grid_read
        procedure :: write => rectlnr_grid_write
        procedure, private :: setup => rectlnr_grid_setup
        procedure :: check_for_diffs => rectlnr_grid_check_for_diffs
        procedure :: get_range => rectlnr_grid_get_range
        procedure :: get_coord => rectlnr_grid_get_coord
    end type rectlnr_grid

    type, extends(dataset) :: polygonal_data
        !! polygonal data
        private
        integer(i4k)                                :: n_points = 0
        real(r8k),      dimension(:,:), allocatable :: points
        class(vtkcell), dimension(:),   allocatable :: vertices
        class(vtkcell), dimension(:),   allocatable :: lines
        class(vtkcell), dimension(:),   allocatable :: polygons
        class(vtkcell), dimension(:),   allocatable :: triangles
    contains
        procedure :: read  => polygonal_data_read
        procedure :: write => polygonal_data_write
        procedure, private :: setup => polygonal_data_setup
    end type polygonal_data

    type, extends(dataset) :: unstruct_grid
        !! unstructured grid
        private
        integer(i4k), public :: n_points = 0
        integer(i4k), public :: n_cells  = 0
        integer(i4k) :: n_cell_types = 0
        integer(i4k) :: size         = 0
        real(r8k),          dimension(:,:), allocatable :: points
        type(vtkcell_list), dimension(:),   allocatable :: cell_list
    contains
        procedure :: read  => unstruct_grid_read
        procedure :: write => unstruct_grid_write
        procedure :: unstruct_grid_setup
        procedure :: unstruct_grid_setup_multiclass
        generic, private :: setup => unstruct_grid_setup, unstruct_grid_setup_multiclass
        procedure :: get_point => unstruct_grid_get_point
        procedure :: get_connectivity => unstruct_grid_get_connectivity
        procedure :: get_offset => unstruct_grid_get_offset
        procedure :: get_type => unstruct_grid_get_type
    end type unstruct_grid

    interface
        ! ****************
        ! abstract dataset
        ! ****************
        module subroutine abs_read (me, unit)
            implicit none
            !! reads the dataset information from the .vtk file
            class(dataset), intent(out) :: me
            integer(i4k),   intent(in)  :: unit

        end subroutine abs_read

        module subroutine abs_write (me, unit)
            implicit none
            !! writes the dataset information to the .vtk file
            class(dataset), intent(in) :: me
            integer(i4k),   intent(in) :: unit

        end subroutine abs_write

        module subroutine init (me, datatype, dims, origin, spacing, points, cells, cell_list, &
            &                     x_coords, y_coords, z_coords, vertices, lines, polygons, triangles)
            implicit none
            !! initializes the dataset
            class (dataset),                     intent(out)          :: me
            class(vtkcell),      dimension(:),   intent(in), optional :: vertices
            class(vtkcell),      dimension(:),   intent(in), optional :: lines
            class(vtkcell),      dimension(:),   intent(in), optional :: polygons
            class(vtkcell),      dimension(:),   intent(in), optional :: triangles
                class(vtkcell),      dimension(:),   intent(in), optional :: cells      !! dt of same cell types
            type(vtkcell_list),  dimension(:),   intent(in), optional :: cell_list  !! dt of different cell types
            character(len=*),                    intent(in), optional :: datatype   !! type of data (floating, integer, etc.)
            integer(i4k),        dimension(3),   intent(in), optional :: dims
            real(r8k),           dimension(3),   intent(in), optional :: origin
            real(r8k),           dimension(3),   intent(in), optional :: spacing
            real(r8k),           dimension(:),   intent(in), optional :: x_coords
            real(r8k),           dimension(:),   intent(in), optional :: y_coords
            real(r8k),           dimension(:),   intent(in), optional :: z_coords
            real(r8k),           dimension(:,:), intent(in), optional :: points

        end subroutine init

        module function check_for_diffs (me, you) result (diffs)
            implicit none
            !! function checks for differences in a dataset
            class(dataset), intent(in) :: me, you
            logical :: diffs

        end function check_for_diffs

        module function get_range_cnt (me) result (range)
            implicit none
            !! function returns the number of variables in x,y,z coordinates
            class(dataset), intent(in)   :: me
            integer(i4k), dimension(2,3) :: range

        end function get_range_cnt

        module function get_range (me) result (range)
            implicit none
            !! function returns the min / max range of values in x,y,z coordinates
            class(dataset), intent(in) :: me
            real(r8k),  dimension(2,3) :: range

        end function get_range

        module function get_coord (me, dim) result (coord)
            implicit none
            !! function returns the min / max range of values in x,y,z coordinates
            class(dataset), intent(in) :: me
            integer(i4k),   intent(in) :: dim
            real(r8k), dimension(:), allocatable :: coord

        end function get_coord
        ! *****************
        ! structured points
        ! *****************
        module subroutine struct_pts_read (me, unit)
            implicit none
            !! reads the structured points dataset information from the .vtk file
            class(struct_pts), intent(out) :: me
            integer(i4k),      intent(in)  :: unit

        end subroutine struct_pts_read

        module subroutine struct_pts_write (me, unit)
            implicit none
            !! writes the structured points dataset information to the .vtk file
            class(struct_pts), intent(in) :: me
            integer(i4k),      intent(in) :: unit

        end subroutine struct_pts_write

        pure module function struct_pts_get_origin (me) result (origin)
            implicit none
            !! gets the private dt data for origin
            class (struct_pts), intent(in) :: me
            real(r8k), dimension(3)        :: origin

        end function struct_pts_get_origin

        pure module function struct_pts_get_spacing (me) result (spacing)
            implicit none
            !! gets the private dt data for spacing
            class (struct_pts), intent(in) :: me
            real(r8k), dimension(3)        :: spacing

        end function struct_pts_get_spacing

        module subroutine struct_pts_setup (me, dims, origin, spacing)
            implicit none
            !! sets up the structured points dataset with information
            class (struct_pts),         intent(out) :: me
            integer(i4k), dimension(3), intent(in)  :: dims
            real(r8k),    dimension(3), intent(in)  :: origin, spacing

        end subroutine struct_pts_setup

        module function struct_pts_check_for_diffs (me, you) result (diffs)
            implicit none
            !! function checks for differences in a structured points dataset
            class(struct_pts), intent(in) :: me
            class(dataset),    intent(in) :: you
            logical                       :: diffs

        end function struct_pts_check_for_diffs

        module function struct_pts_get_range (me) result (range)
            implicit none
            !! function returns the min / max range of values in x,y,z coordinates
            class(struct_pts), intent(in) :: me
            integer(i4k), dimension(2,3)  :: range

        end function struct_pts_get_range
        ! ***************
        ! structured grid
        ! ***************
        module subroutine struct_grid_read (me, unit)
            implicit none
            !! reads the structured grid dataset information from the .vtk file
            class(struct_grid), intent(out) :: me
            integer(i4k),       intent(in)  :: unit

        end subroutine struct_grid_read

        module subroutine struct_grid_write (me, unit)
            implicit none
            !! writes the structured grid dataset information to the .vtk file
            class(struct_grid), intent(in) :: me
            integer(i4k),       intent(in) :: unit

        end subroutine struct_grid_write

        module subroutine struct_grid_setup (me, dims, points)
            implicit none
            !! sets up the structured grid dataset with information
            class (struct_grid),          intent(out) :: me
            integer(i4k), dimension(3),   intent(in)  :: dims
            real(r8k),    dimension(:,:), intent(in)  :: points

        end subroutine struct_grid_setup

        module function struct_grid_check_for_diffs (me, you) result (diffs)
            implicit none
            !! function checks for differences in a structured grid dataset
            class(struct_grid), intent(in) :: me
            class(dataset),     intent(in) :: you
            logical                        :: diffs

        end function struct_grid_check_for_diffs

        module function struct_grid_get_point (me, i) result (coord)
            implicit none
            !! function returns the (x,y,z) dimension coordinates for point (i)
            class(struct_grid), intent(in) :: me
            integer(i4k),       intent(in) :: i
            real(r8k), dimension(3)        :: coord

        end function struct_grid_get_point

        module function struct_grid_get_range (me) result (range)
            implicit none
            !! function returns the min / max range of values in x,y,z coordinates
            class(struct_grid), intent(in) :: me
            integer(i4k), dimension(2,3)   :: range

        end function struct_grid_get_range
        ! ****************
        ! rectilinear grid
        ! ****************
        module subroutine rectlnr_grid_read (me, unit)
            implicit none
            !! reads the rectilinear grid dataset information from the .vtk file
            class(rectlnr_grid), intent(out) :: me
            integer(i4k),        intent(in)  :: unit

        end subroutine rectlnr_grid_read

        module subroutine rectlnr_grid_write (me, unit)
            implicit none
            !! writes the rectilinear grid dataset information to the .vtk file
            class(rectlnr_grid), intent(in) :: me
            integer(i4k),        intent(in) :: unit

        end subroutine rectlnr_grid_write

        module subroutine rectlnr_grid_setup (me, dims, x_coords, y_coords, z_coords, datatype)
            implicit none
            !! sets up the rectilinear grid dataset with information
            class (rectlnr_grid),       intent(out) :: me         !! rectilinear grid dt
            integer(i4k), dimension(3), intent(in)  :: dims       !! # of dimensions in (x,y,z) direction
            real(r8k),    dimension(:), intent(in)  :: x_coords   !! x coordinates
            real(r8k),    dimension(:), intent(in)  :: y_coords   !! y coordinates
            real(r8k),    dimension(:), intent(in)  :: z_coords   !! z coordinates
            character(len=*),           intent(in)  :: datatype   !! type of data (floating, integer, etc.)

        end subroutine rectlnr_grid_setup

        module function rectlnr_grid_check_for_diffs (me, you) result (diffs)
            implicit none
            !! function checks for differences in a rectilinear grid dataset
            class(rectlnr_grid), intent(in) :: me
            class(dataset),      intent(in) :: you
            logical                         :: diffs

        end function rectlnr_grid_check_for_diffs

        module function rectlnr_grid_get_range (me) result (range)
            implicit none
            !! function returns the min / max range of values in x,y,z coordinates
            class(rectlnr_grid), intent(in) :: me
            real(r8k), dimension(2,3)       :: range

        end function rectlnr_grid_get_range

        module function rectlnr_grid_get_coord (me, dim) result (coord)
            implicit none
            !! function returns the min / max range of values in x,y,z coordinates
            class(rectlnr_grid), intent(in) :: me
            integer(i4k),        intent(in) :: dim
            real(r8k), dimension(:), allocatable :: coord

        end function rectlnr_grid_get_coord
        ! **************
        ! polygonal data
        ! **************
        module subroutine polygonal_data_read (me, unit)
            implicit none
            !! reads the polygonal data dataset information from the .vtk file
            class(polygonal_data), intent(out) :: me
            integer(i4k),          intent(in)  :: unit

        end subroutine polygonal_data_read

        module subroutine polygonal_data_write (me, unit)
            implicit none
            !! writes the polygonal data dataset information to the .vtk file
            class(polygonal_data), intent(in) :: me
            integer(i4k),          intent(in) :: unit

        end subroutine polygonal_data_write

        module subroutine polygonal_data_setup (me, points, vertices, lines, polygons, triangles)
            implicit none
            !! sets up the polygonal data dataset with information
            class (polygonal_data),       intent(out)          :: me
            real(r8k),    dimension(:,:), intent(in)           :: points
            class(vtkcell), dimension(:), intent(in), optional :: vertices
            class(vtkcell), dimension(:), intent(in), optional :: lines
            class(vtkcell), dimension(:), intent(in), optional :: polygons
            class(vtkcell), dimension(:), intent(in), optional :: triangles

        end subroutine polygonal_data_setup

        module function polygonal_data_get_range (me) result (range)
            implicit none
            !! function returns the polygonal data min / max range of values in x,y,z coordinates
            class(polygonal_data), intent(in) :: me
            integer(i4k), dimension(2,3)      :: range

        end function polygonal_data_get_range
        ! *****************
        ! unstructured grid
        ! *****************
        module subroutine unstruct_grid_read (me, unit)
            implicit none
            !! reads the unstructured grid dataset information from the .vtk file
            class(unstruct_grid), intent(out) :: me
            integer(i4k),         intent(in)  :: unit

        end subroutine unstruct_grid_read

        module subroutine unstruct_grid_write (me, unit)
            implicit none
            !! writes the unstructured grid dataset information from the .vtk file
            class(unstruct_grid), intent(in) :: me
            integer(i4k),         intent(in) :: unit

        end subroutine unstruct_grid_write

        module subroutine unstruct_grid_setup (me, points, cells)
            implicit none
            !! sets up the unstructured grid dataset with information for a single class of cells
            class(unstruct_grid),           intent(out) :: me      !! dt
            real(r8k),      dimension(:,:), intent(in)  :: points  !!
            class(vtkcell), dimension(:),   intent(in)  :: cells   !! dt of same cell types

        end subroutine unstruct_grid_setup

        module subroutine unstruct_grid_setup_multiclass (me, points, cell_list)
            implicit none
            !! sets up the unstructured grid dataset with information for a list of different classes of cells
            class(unstruct_grid),               intent(out) :: me         !! dt
            real(r8k),          dimension(:,:), intent(in)  :: points     !!
            type(vtkcell_list), dimension(:),   intent(in)  :: cell_list  !! dt of different cell types

        end subroutine unstruct_grid_setup_multiclass

        module function unstruct_grid_get_range (me) result (range)
            implicit none
            !! function returns the unstructured grid min / max range of values in x,y,z coordinates
            class(unstruct_grid), intent(in) :: me
            integer(i4k), dimension(2,3)     :: range

        end function unstruct_grid_get_range

        module function unstruct_grid_get_point (me, i) result (coord)
            implicit none
            !! function returns the (x,y,z) dimension coordinates for point (i)
            class(unstruct_grid), intent(in) :: me
            integer(i4k),         intent(in) :: i
            real(r8k), dimension(3)          :: coord

        end function unstruct_grid_get_point

        module function unstruct_grid_get_connectivity (me, i) result (connectivity)
            implicit none
            !! function returns the connectivity for cell (i)
            class(unstruct_grid), intent(in) :: me
            integer(i4k),         intent(in) :: i
            integer(i4k), dimension(:), allocatable :: connectivity

        end function unstruct_grid_get_connectivity

        module function unstruct_grid_get_offset (me, i) result (offset)
            implicit none
            !! function returns the offset for cell (i)
            !! the offset is just the # of points
            class(unstruct_grid), intent(in) :: me
            integer(i4k),         intent(in) :: i
            integer(i4k) :: offset

        end function unstruct_grid_get_offset

        module function unstruct_grid_get_type (me, i) result (type)
            implicit none
            !! function returns the type for cell (i)
            class(unstruct_grid), intent(in) :: me
            integer(i4k),         intent(in) :: i
            integer(i4k) :: type

        end function unstruct_grid_get_type

    end interface

end module vtk_datasets
