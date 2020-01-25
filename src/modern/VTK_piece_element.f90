module vtk_piece_element
    use xml,            only : xml_element_dt
    use vtk_datasets,   only : dataset
    use vtk_attributes, only : attribute, attributes
    use vtk_dataarray_element, only : dataarray_dt
    use vtk_element,    only : vtk_element_dt
    implicit none
    !! author: Ian Porter
    !! date: 06/07/2019
    !!
    !! this is the basic file piece elements
    !!

    private
    public :: coordinates_dt, celldata_dt, pointdata_dt, piece_dt

    type, extends(xml_element_dt) :: data_dt
        !! pointdata derived type
        private
        character(len=:), allocatable :: scalars
        character(len=:), allocatable :: vectors
        character(len=:), allocatable :: normals
        character(len=:), allocatable :: tensors
        character(len=:), allocatable :: tcoords
        type(dataarray_dt), allocatable :: connectivity
        type(dataarray_dt), allocatable :: offsets
        type(dataarray_dt), allocatable :: types
        type(dataarray_dt), allocatable :: dataarray_x
        type(dataarray_dt), allocatable :: dataarray_y
        type(dataarray_dt), allocatable :: dataarray_z
        type(dataarray_dt), dimension(:), allocatable, public :: dataarray
    contains
        procedure, non_overridable :: data_setup
        procedure, non_overridable :: data_initialize
        generic, public :: initialize => data_initialize
        procedure, non_overridable :: data_add_attribute
        procedure, non_overridable :: data_add_attributes
        generic, public :: add_cell => data_add_attribute
        generic, public :: add_cell => data_add_attributes
        procedure :: data_deallocate
        procedure, private :: data_finalize
        generic, public :: finalize => data_finalize
    end type data_dt

    type, extends(data_dt) :: pointdata_dt
        !! pointdata derived type
    end type pointdata_dt

    type, extends(data_dt) :: celldata_dt
        !! celldata derived type
    end type celldata_dt

    type, extends(data_dt) :: points_dt
        !! points derived type
    contains
        procedure, non_overridable :: points_initialize
        generic, public :: initialize => points_initialize
    end type points_dt

    type, extends(data_dt) :: cells_dt
        !! cells derived type
    contains
        procedure, non_overridable :: cells_initialize
        generic, public :: initialize => cells_initialize
    end type cells_dt

    type, extends(data_dt) :: coordinates_dt
        !! coordinates derived type
    contains
        procedure, non_overridable :: coordinates_initialize
        generic, public :: initialize => coordinates_initialize
    end type coordinates_dt

    type, extends(vtk_element_dt) :: piece_dt
        !! piece derived type
        type(points_dt),      allocatable :: points
        type(coordinates_dt), allocatable :: coordinates
        type(cells_dt),       allocatable :: cells
        type(pointdata_dt),   allocatable :: pointdata
        type(celldata_dt),    allocatable :: celldata
        character(len=:),     allocatable :: source
    contains
        ! procedure, non_overridable, public :: initialize => piece_initialize
        procedure, private :: piece_set_grid
        generic, public :: set => piece_set_grid
        procedure, non_overridable, public :: piece_add_data
        generic, public :: add_data => piece_add_data
        procedure, public :: piece_deallocate
        procedure :: finalize => piece_finalize
    end type piece_dt

    interface

        module subroutine data_setup (me)
            implicit none
            !! author: Ian Porter
            !! date: 06/07/2019
            !!
            !! this writes the header for a data_dt
            !!
            class(data_dt), intent(inout) :: me                     !! data dt

        end subroutine data_setup

        module subroutine data_initialize (me, scalar)
            implicit none
            !! author: Ian Porter
            !! date: 06/07/2019
            !!
            !! this initializes the data within a data_dt
            !!
            class(data_dt),   intent(inout)        :: me            !! data dt
            character(len=*), intent(in), optional :: scalar        !! name of scalar component

        end subroutine data_initialize

        recursive module subroutine data_add_attribute (me, cell)
            implicit none
            !! author: Ian Porter
            !! date: 06/07/2019
            !!
            !! this adds data inside of a data_dt
            !!
            class(data_dt),   intent(inout) :: me               !! data dt
            class(attribute), intent(in)    :: cell             !! name of scalar component

        end subroutine data_add_attribute

        recursive module subroutine data_add_attributes (me, cell)
            implicit none
            !! author: Ian Porter
            !! date: 06/07/2019
            !!
            !! this adds a set of data inside of a data_dt the vtk_element_dt header into xml format
            !!
            class(data_dt),                 intent(inout) :: me     !! data dt
            type(attributes), dimension(:), intent(in)    :: cell   !! name of scalar component

        end subroutine data_add_attributes

        module subroutine data_finalize (me)
            implicit none
            !! author: Ian Porter
            !! date: 06/07/2019
            !!
            !! finalize routine to write the proper data information
            !!
            class(data_dt), intent(inout) :: me

        end subroutine data_finalize

        recursive module subroutine data_deallocate (foo)
            implicit none
            !! author: Ian Porter
            !! date: 06/07/2019
            !!
            !! explicitly deallocate a data_dt
            !!
            class(data_dt), intent(inout) :: foo                    !! data dt

        end subroutine data_deallocate

        module subroutine points_initialize (me, geometry)
            implicit none
            !! author: Ian Porter
            !! date: 07/29/2019
            !!
            !! initializes a points dt with the geometry information
            !!
            class(points_dt), intent(inout) :: me
            class(dataset),   intent(in)    :: geometry

        end subroutine points_initialize

        module subroutine cells_initialize (me, geometry)
            implicit none
            !! author: Ian Porter
            !! date: 07/29/2019
            !!
            !! initializes a cells dt with the geometry information
            !!
            class(cells_dt), intent(inout) :: me
            class(dataset),  intent(in)    :: geometry

        end subroutine cells_initialize

        module subroutine coordinates_initialize (me, geometry)
            implicit none
            !! author: Ian Porter
            !! date: 07/09/2019
            !!
            !! initializes a piece dt with the geometry information
            !!
            class(coordinates_dt), intent(inout) :: me
            class(dataset),        intent(in)    :: geometry

        end subroutine coordinates_initialize

        module subroutine piece_set_grid (me, geometry)
            implicit none
            !! author: Ian Porter
            !! date: 07/09/2019
            !!
            !! initializes a piece dt with the geometry information
            !!
            class(piece_dt), intent(inout) :: me
            class(dataset),  intent(in)    :: geometry

        end subroutine piece_set_grid

        module subroutine piece_add_data (me, celldata, pointdata, celldatasets, pointdatasets)
            implicit none
            !! author: Ian Porter
            !! date: 06/07/2019
            !!
            !! this is a deferred routine for each grid type to implement its own routine to set grid dependent data / info
            !!
            class(piece_dt),                intent(inout)        :: me
            class(attribute),               intent(in), optional :: celldata   !!
            class(attribute),               intent(in), optional :: pointdata  !!
            type(attributes), dimension(:), intent(in), optional :: celldatasets  !!
            type(attributes), dimension(:), intent(in), optional :: pointdatasets !!

        end subroutine piece_add_data

        module subroutine piece_finalize (me)
            implicit none
            !! author: Ian Porter
            !! date: 06/07/2019
            !!
            !! finalize routine to write the proper piece information
            !!
            class(piece_dt), intent(inout) :: me

        end subroutine piece_finalize

        recursive module subroutine piece_deallocate (foo)
            implicit none
            !! author: Ian Porter
            !! date: 06/07/2019
            !!
            !! explicitly deallocate a piece dt
            !!
            class(piece_dt), intent(inout) :: foo

        end subroutine piece_deallocate

    end interface

end module vtk_piece_element
