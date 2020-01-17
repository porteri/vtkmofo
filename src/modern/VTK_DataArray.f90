module vtk_dataarray_element
    use xml,       only : xml_element_dt
    use precision, only : i4k, r8k
    implicit none
    !! author: Ian Porter
    !! date: 06/07/2019
    !!
    !! this is the basic file piece elements
    !!

    private
    public :: dataarray_dt

    type, extends(xml_element_dt) :: dataarray_dt
        !! dataarray derived type
        private
        character(len=:), allocatable :: type
        character(len=:), allocatable :: array_name
        character(len=:), allocatable :: numberofcomponents
        character(len=:), allocatable :: format
        character(len=:), allocatable :: array_offset
        character(len=:), allocatable :: range_min
        character(len=:), allocatable :: range_max
    contains
        procedure, non_overridable :: dataarray_setup
        procedure, non_overridable :: dataarray_initialize
        generic, public :: initialize => dataarray_initialize
        procedure :: element_add_element => dataarray_add_dataarray
        procedure :: dataarray_deallocate
    end type dataarray_dt

    interface

        module subroutine dataarray_setup (me)
            implicit none
            !! author: Ian Porter
            !! date: 06/07/2019
            !!
            !! this writes the header for the dataarray
            !!
            class(dataarray_dt), intent(inout) :: me                     !! dataarray dt

        end subroutine dataarray_setup

        module subroutine dataarray_initialize (me, type, name, numberofcomponents, format, offset, range_min, range_max)
            implicit none
            !! author: Ian Porter
            !! date: 06/07/2019
            !!
            !! this converts the vtk_element_dt header into xml format
            !!
            class(dataarray_dt), intent(inout) :: me                        !! dataarray dt
            character(len=*),    intent(in), optional :: type               !! type of data of a single component
            character(len=*),    intent(in), optional :: name               !! name of the array
            integer(i4k),        intent(in), optional :: numberofcomponents !! the # of components per value in the array
            character(len=*),    intent(in), optional :: format             !! the means by whih the data is stored in the file
            character(len=*),    intent(in), optional :: offset             !! if format='appended', this specifies the offset from
                                                                            !! the beginning of the appended data
            real(r8k),           intent(in), optional :: range_min          !! min value in array of numbers
            real(r8k),           intent(in), optional :: range_max          !! max value in array of numbers

        end subroutine dataarray_initialize

        module subroutine dataarray_add_dataarray (me, element)
            implicit none
            !! this adds a dataarray inside of a xml dataarray block
            class(dataarray_dt),   intent(inout) :: me       !! dataarray dt
            class(xml_element_dt), intent(in)    :: element  !! inner xml element

        end subroutine dataarray_add_dataarray

        recursive module subroutine dataarray_deallocate (me)
            implicit none
            !! this explicitly deallocates a dataarray
            class(dataarray_dt), intent(inout) :: me         !! dataarray dt

        end subroutine dataarray_deallocate

    end interface

end module vtk_dataarray_element
