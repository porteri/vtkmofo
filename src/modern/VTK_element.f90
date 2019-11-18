module vtk_element
    use xml,            only : xml_file_dt, xml_element_dt
    use vtk_attributes, only : attribute, attributes
    use vtk_datasets,   only : dataset
    implicit none
    !! author: Ian Porter
    !! date: 05/06/2019
    !!
    !! this is the basic component for a vtk element
    !!

    private
    public :: vtk_element_dt

    character(len=*), parameter :: def_version = "0.1"

    type, extends(xml_element_dt) :: vtk_element_dt
        !!
        private
        character(len=:), allocatable :: type
        character(len=:), allocatable :: version
        character(len=:), allocatable :: compression
        character(len=:), allocatable, public :: file_extension
        character(len=:), allocatable, public :: filename
        type(vtk_element_dt), allocatable, public :: vtk_element !! currently handle only one piece
    contains
        procedure, non_overridable :: vtk_element_setup
        procedure, non_overridable, public :: initialize
        procedure, public :: finalize
        procedure :: me_deallocate => gcc_bug_workaround_deallocate_vtk_element_single
        ! procedure :: gcc_bug_workaround_deallocate_single => gcc_bug_workaround_deallocate_vtk_element_single
    end type vtk_element_dt

    interface

        module subroutine vtk_element_setup (me)
            implicit none
            !! author: Ian Porter
            !! date: 05/07/2019
            !!
            !! this converts the vtk_element_dt header into xml format
            !!
            class(vtk_element_dt), intent(inout) :: me

        end subroutine vtk_element_setup

        module subroutine initialize (me, type, compression, file_extension)
            implicit none
            !! author: Ian Porter
            !! date: 05/07/2019
            !!
            !! this is an external interface to allow the user or other routines to set the internal variables
            !!
            class(vtk_element_dt), intent(inout)        :: me              !!
            character(len=*),      intent(in), optional :: type            !! grid type
            character(len=*),      intent(in), optional :: compression     !!
            character(len=*),      intent(in), optional :: file_extension  !!

        end subroutine initialize

        recursive module subroutine finalize (me)
            implicit none
            !! author: Ian Porter
            !! date: 06/07/2019
            !!
            !! this writes the end of the file
            !!
            class(vtk_element_dt), intent(inout) :: me              !!

        end subroutine finalize

        recursive module subroutine gcc_bug_workaround_deallocate_vtk_element_single (foo)
            implicit none
            !! gcc work-around for deallocating a multi-dimension derived type w/ allocatable character strings
            class(vtk_element_dt), intent(inout) :: foo

        end subroutine gcc_bug_workaround_deallocate_vtk_element_single

    end interface

end module vtk_element
