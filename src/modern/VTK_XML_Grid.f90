module vtk_xml_grid
    use xml,               only : xml_file_dt, xml_element_dt
    use vtk_datasets,      only : dataset
    use vtk_piece_element, only : piece_dt
    use vtk_element,       only : vtk_element_dt
    implicit none
    !! author: Ian Porter
    !! date: 07/28/2019
    !!
    !! this contains the dts for the different serial geometry types
    !!

    private
    public :: vtk_dataset_dt
    public :: vtk_xml_rectilineargrid_dt
    public :: vtk_xml_structuredgrid_dt
    public :: vtk_xml_unstructuredgrid_dt
    public :: vtk_xml_imagedata_dt

    type, extends(vtk_element_dt), abstract :: vtk_dataset_dt
        !! vtk dataset derived type
        character(len=:), allocatable :: wholeextent  !! string for the whole extent of the range
        character(len=:), allocatable :: grid_type    !! name of the grid type
        character(len=:), allocatable :: extra_string !! additional data needed to be written
        type(piece_dt),   allocatable :: piece        !! piece dt (currently only supporting one piece)
    contains
        procedure(abs_set_grid), deferred :: set_grid
        procedure :: parallel_fix !! Performs the steps needed to copy info to a parallel file
        procedure :: vtk_dataset_deallocate
        procedure :: finalize
    end type vtk_dataset_dt

    type, extends(vtk_dataset_dt) :: vtk_xml_imagedata_dt
        !! serial file imagedata grid
        private
    contains
        procedure :: set_grid => imagedata_set_grid
    end type vtk_xml_imagedata_dt

    type, extends(vtk_dataset_dt) :: vtk_xml_rectilineargrid_dt
        !! serial file rectilinear grid
        private
    contains
        procedure :: set_grid => rectilineargrid_set_grid
    end type vtk_xml_rectilineargrid_dt

    type, extends(vtk_dataset_dt) :: vtk_xml_structuredgrid_dt
        !! serial file structured grid
        private
    contains
        procedure :: set_grid => structuredgrid_set_grid
    end type vtk_xml_structuredgrid_dt

    type, extends(vtk_dataset_dt) :: vtk_xml_unstructuredgrid_dt
        !! serial file unstructured grid
        private
    contains
        procedure :: set_grid => unstructuredgrid_set_grid
    end type vtk_xml_unstructuredgrid_dt

    interface

        module subroutine abs_set_grid (me, geometry)
            implicit none
            !! author: Ian Porter
            !! date: 07/09/2019
            !!
            !! initializes a piece dt with the geometry information
            !!
            class(vtk_dataset_dt), intent(inout) :: me
            class(dataset),        intent(in)    :: geometry

        end subroutine abs_set_grid

        module subroutine parallel_fix (me)
            implicit none
            !! author: Ian Porter
            !! date: 01/11/2020
            !!
            !! Performs an update to all of the filenames
            !!
            class(vtk_dataset_dt), intent(inout) :: me

        end subroutine parallel_fix

        module subroutine finalize (me)
            implicit none
            !! author: Ian Porter
            !! date: 07/28/2019
            !!
            !! writes data inside of itself
            !!
            class(vtk_dataset_dt), intent(inout) :: me

        end subroutine finalize

        recursive module subroutine vtk_dataset_deallocate (foo)
            implicit none
            !! author: Ian Porter
            !! date: 07/28/2019
            !!
            !! gcc work-around for deallocating a multi-dimension derived type w/ allocatable character strings
            !!
            class(vtk_dataset_dt), intent(inout) :: foo

        end subroutine vtk_dataset_deallocate

        module subroutine imagedata_set_grid (me, geometry)
            implicit none
            !! author: Ian Porter
            !! date: 08/08/2019
            !!
            !! this writes the grid information for an image data grid
            !!
            class(vtk_xml_imagedata_dt), intent(inout) :: me         !! serial geometry dt
            class(dataset),              intent(in)    :: geometry   !! dt of geometry information

        end subroutine imagedata_set_grid

        module subroutine rectilineargrid_set_grid (me, geometry)
            implicit none
            !! author: Ian Porter
            !! date: 07/28/2019
            !!
            !! this writes the grid information for a rectilinear grid
            !!
            class(vtk_xml_rectilineargrid_dt), intent(inout) :: me         !! serial geometry dt
            class(dataset),                    intent(in)    :: geometry   !! dt of geometry information

        end subroutine rectilineargrid_set_grid

        module subroutine structuredgrid_set_grid (me, geometry)
            implicit none
            !! author: Ian Porter
            !! date: 07/28/2019
            !!
            !! this writes the grid information for a structured grid
            !!
            class(vtk_xml_structuredgrid_dt), intent(inout) :: me         !! serial geometry dt
            class(dataset),                   intent(in)    :: geometry   !! dt of geometry information

        end subroutine structuredgrid_set_grid

        module subroutine unstructuredgrid_set_grid (me, geometry)
            implicit none
            !! author: Ian Porter
            !! date: 07/29/2019
            !!
            !! this writes the grid information for an unstructured grid
            !!
            class(vtk_xml_unstructuredgrid_dt), intent(inout) :: me         !! serial geometry dt
            class(dataset),                     intent(in)    :: geometry   !! dt of geometry information

        end subroutine unstructuredgrid_set_grid

    end interface

end module vtk_xml_grid
