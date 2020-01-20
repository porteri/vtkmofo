submodule (vtk_dataarray_element) vtk_dataarray_element_procedures
    use vtk_formats_types, only : type_float32, type_float64, type_int32, type_uint32
    implicit none
    !! author: Ian Porter
    !! date: 06/07/2019
    !!
    !! this is the basic file piece elements
    !!
    !! data storage formats

contains

    module procedure dataarray_setup
        use vtk_vars, only : parallel_container_file
        implicit none
        !! author: Ian Porter
        !! date: 06/06/2019
        !!
        !! this writes the header for a dataarray
        !!
        character(len=*), parameter   :: dataarray_name = 'DataArray'
        character(len=:), allocatable :: string
        character(len=:), allocatable :: type_string
        character(len=:), allocatable :: name_string
        character(len=:), allocatable :: nofc_string
        character(len=:), allocatable :: format_string
        character(len=:), allocatable :: offset_string
        character(len=:), allocatable :: range_min_string
        character(len=:), allocatable :: range_max_string

        if (allocated(me%array_name)) then
            allocate(name_string,source=' Name="' // me%array_name // '"')
        else
            allocate(name_string,source='')
        end if
        if (allocated(me%type)) then
            allocate(type_string,source=' type="' // me%type // '"')
        else
            allocate(type_string,source='')
        end if
        if (allocated(me%numberofcomponents)) then
            allocate(nofc_string,source=' NumberOfComponents="' // me%numberofcomponents // '"')
        else
            allocate(nofc_string,source='')
        end if
        if (allocated(me%format)) then
            allocate(format_string,source=' format="' // me%format // '"')
        else
            allocate(format_string,source='')
        end if
        if (allocated(me%array_offset)) then
            allocate(offset_string,source=' offset="' // me%array_offset // '"')
        else
            allocate(offset_string,source='')
        end if
        if (allocated(me%range_min)) then
            allocate(range_min_string,source=' rangemin="' // me%range_min // '"')
        else
            allocate(range_min_string,source='')
        end if
        if (allocated(me%range_max)) then
            allocate(range_max_string,source=' rangemax="' // me%range_max // '"')
        else
            allocate(range_max_string,source='')
        end if

        allocate(string, source=name_string // type_string // nofc_string // format_string // &
            &                   offset_string // range_min_string // range_max_string)

        if (parallel_container_file) then
            call me%setup(name='P' // dataarray_name, string=string)
        else
            call me%setup(name=dataarray_name, string=string)
        end if

    end procedure dataarray_setup

    module procedure dataarray_initialize
        use misc, only : convert_to_string, to_lowercase
        implicit none
        !! author: Ian Porter
        !! date: 06/07/2019
        !!
        !! this converts the vtk_element_dt header into xml format
        !!

        if (present(type)) then
            !! may need to convert the legacy data type names to the modern type names
            !bit, unsigned_char, char, unsigned_short, short, unsigned_int, int, unsigned_long, long, float, or double
            !int8, uint8, int16, uint16, int32, uint32, int64, uint64, float32, float64
            select case (to_lowercase(type))
            case ('float')
                allocate(me%type,source=type_float32)
            case ('double')
                allocate(me%type,source=type_float64)
            case ('int')
                allocate(me%type,source=type_int32)
            case ('unsigned_int')
                allocate(me%type,source=type_uint32)
            case default
                !! assume all other data types are ok
                allocate(me%type,source=type)
            end select
        end if
        if (present(name))               allocate(me%array_name,source=name)
        if (present(numberofcomponents)) then
            allocate(me%numberofcomponents,source=convert_to_string(numberofcomponents))
        end if
        if (present(format))             allocate(me%format,source=format)
        if (present(offset))             allocate(me%array_offset,source=offset)
        if (present(range_min)) then
            allocate(me%range_min,source=convert_to_string(range_min))
        end if
        if (present(range_max)) then
            allocate(me%range_max,source=convert_to_string(range_max))
        end if

        call me%dataarray_setup()

    end procedure dataarray_initialize

    module procedure dataarray_allocate
        implicit none
        !! this is an explicit allocation due to gcc bug
        integer :: i

        if (allocated(me)) then
            do i = lbound(me,dim=1), ubound(me,dim=1)
                call me(i)%dataarray_deallocate()
            end do
            deallocate(me)
        end if
        if (present(oldfoo)) then
            if (present(addfoo)) then
                allocate(me(size(oldfoo)+1))
            else
                allocate(me(size(oldfoo)))
            end if
            do i = 1, size(oldfoo)
!                if (allocated(oldfoo%element)) then
!                end if
            end do
        end if

    end procedure dataarray_allocate

    module procedure dataarray_deallocate
        implicit none
        !! this explicitly deallocates a dataarray

        if (allocated(me%type))               deallocate(me%type)
        if (allocated(me%array_name))         deallocate(me%array_name)
        if (allocated(me%numberofcomponents)) deallocate(me%numberofcomponents)
        if (allocated(me%format))             deallocate(me%format)
        if (allocated(me%array_offset))       deallocate(me%array_offset)
        if (allocated(me%range_min))          deallocate(me%range_min)
        if (allocated(me%range_max))          deallocate(me%range_max)

        call me%deallocate()

    end procedure dataarray_deallocate

end submodule vtk_dataarray_element_procedures
