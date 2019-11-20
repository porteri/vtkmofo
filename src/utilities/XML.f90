module xml
    use precision,       only : i4k, i8k, r4k, r8k
    use file_utility,    only : file_data_structure
    use iso_fortran_env, only : output_unit
    implicit none
    !! author: Ian Porter
    !! date: 04/02/2019
    !!
    !! this is a simple xml format writer
    !!

    private
    public :: xml_element_dt, xml_file_dt, gcc_bug_workaround_allocate, gcc_bug_workaround_deallocate
    public :: file_format, binary, ascii, convert_format_to_string, convert_string_to_format
    public :: format_ascii, format_binary, format_append, file_format_text

    enum, bind(c)
        enumerator :: ascii, binary, append
    end enum

    integer(i4k) :: file_format = ascii

    character(len=*), parameter :: format_ascii  = 'ascii'
    character(len=*), parameter :: format_binary = 'binary'
    character(len=*), parameter :: format_append = 'appended'
    character(len=:), allocatable :: file_format_text

    type string_dt
        !! String derived type to allow handling of various sized strings
        character(len=:), allocatable :: text
    contains
        procedure, private :: gcc_bug_deallocate_string_dt
        generic, public    :: deallocate => gcc_bug_deallocate_string_dt
    end type string_dt

    type real32_dt
        real(r4k), dimension(:), allocatable :: val
    end type real32_dt

    type real64_dt
        real(r8k), dimension(:), allocatable :: val
    end type real64_dt

    type xml_element_dt
        !! xml derived type
        private
        character(len=:), allocatable :: name         !! name of the xml block
        integer(i4k) :: unit = output_unit            !! file unit #
        character(len=:), allocatable :: offset       !! offset for data within xml block
        character(len=:), allocatable :: additional_data !! additional data to write in header
        type(string_dt),      dimension(:), allocatable :: string  !! string data set(s) within element
        type(real32_dt),      dimension(:), allocatable :: real32  !! string of real64
        type(real64_dt),      dimension(:), allocatable :: real64  !! string of real64
        type(xml_element_dt), dimension(:), allocatable :: element !! element data set(s) within element
    contains
        procedure, public  :: setup => element_setup   !! set up element block
        procedure, private :: begin => element_begin   !! write open of element block
        procedure, private :: element_add_string       !! write raw data inside of element block
        procedure, private :: element_add_element      !! write another element inside element block
        procedure, private :: element_add_real32       !! write real32 into a string inside of element block
        procedure, private :: element_add_real64       !! write real64 into a string inside of element block
        procedure, private :: element_add_int32        !! write ints32 into a string inside of element block
        procedure, private :: element_add_int64        !! write ints64 into a string inside of element block
        procedure, private :: element_add_logical      !! write logical into a string inside of element block
        generic, public    :: add   => element_add_string
        generic, public    :: add   => element_add_element
        generic, public    :: add   => element_add_real64
        generic, public    :: add   => element_add_real32
        generic, public    :: add   => element_add_int64
        generic, public    :: add   => element_add_int32
        generic, public    :: add   => element_add_logical
        procedure, private :: end   => element_end     !! write closure of element block
        procedure, public  :: write => element_write   !! writes the element block
        procedure, public  :: replace => replace_in_string !! replaces an identifier in the string
        procedure, private :: gcc_bug_workaround_deallocate_single
        generic, public    :: deallocate => gcc_bug_workaround_deallocate_single
    end type xml_element_dt

    type, extends(file_data_structure) :: xml_file_dt
        !! full xml file dt
        private
        type(xml_element_dt), dimension(:), allocatable, public :: element
    contains
        procedure :: setup_file_information => xml_file_setup
        procedure, private :: begin => xml_begin
        procedure, public  :: add   => xml_add
        procedure, private :: end   => xml_end
        procedure, public  :: write => xml_write
        procedure, private :: gcc_bug_workaround_deallocate_xml_file_dt
        generic,   public  :: deallocate => gcc_bug_workaround_deallocate_xml_file_dt
    end type xml_file_dt

    interface gcc_bug_workaround_deallocate
        procedure :: gcc_bug_workaround_deallocate_array
        procedure :: gcc_bug_workaround_deallocate_single
    end interface gcc_bug_workaround_deallocate

    interface

        module subroutine element_setup (me, name, string, offset)
            implicit none
            !! this sets up the information needed to define the xml element block
            class(xml_element_dt), intent(inout) :: me     !! xml element derived type
            character(len=*),      intent(in)    :: name   !! name of the xml block
            character(len=*),      intent(in), optional :: string !! string of additional data to write
            integer(i4k),          intent(in), optional :: offset !! # of leading spaces inside xml block
        end subroutine element_setup

        module subroutine element_begin (me, unit)
            implicit none
            !! this begins an xml element block
            class(xml_element_dt), intent(in) :: me      !! xml element derived type
            integer(i4k),          intent(in) :: unit    !! file unit # to write to
        end subroutine element_begin

        recursive module subroutine element_add_real32 (me, var)
            implicit none
            !! this adds real double precision data inside of an xml element block
            class(xml_element_dt),   intent(inout) :: me    !! xml element derived type
            real(r4k), dimension(:), intent(in)    :: var   !! data to write
        end subroutine element_add_real32

        recursive module subroutine element_add_real64 (me, var)
            implicit none
            !! this adds real double precision data inside of an xml element block
            class(xml_element_dt),   intent(inout) :: me    !! xml element derived type
            real(r8k), dimension(:), intent(in)    :: var   !! data to write
        end subroutine element_add_real64

        recursive module subroutine element_add_int32 (me, var)
            implicit none
            !! this adds real double precision data inside of an xml element block
            class(xml_element_dt),      intent(inout) :: me    !! xml element derived type
            integer(i4k), dimension(:), intent(in)    :: var   !! data to write
        end subroutine element_add_int32

        recursive module subroutine element_add_int64 (me, var)
            implicit none
            !! this adds real double precision data inside of an xml element block
            class(xml_element_dt),      intent(inout) :: me    !! xml element derived type
            integer(i8k), dimension(:), intent(in)    :: var   !! data to write
        end subroutine element_add_int64

        recursive module subroutine element_add_logical (me, var)
            implicit none
            !! this adds real double precision data inside of an xml element block
            class(xml_element_dt), intent(inout) :: me    !! xml element derived type
            logical, dimension(:), intent(in)    :: var   !! data to write
        end subroutine element_add_logical

        recursive module subroutine element_add_string (me, string, quotes)
            implicit none
            !! this adds data inside of an xml element block
            class(xml_element_dt), intent(inout) :: me      !! xml element derived type
            character(len=*),      intent(in)    :: string  !! string of data to write
            logical, optional,     intent(in)    :: quotes  !! flag to turn quotation marks around string on/off
        end subroutine element_add_string

        recursive module subroutine element_add_element (me, element)
            implicit none
            !! this adds an element inside of an xml element block
            class(xml_element_dt), intent(inout) :: me       !! xml element derived type
            class(xml_element_dt), intent(in)    :: element  !! inner xml element
        end subroutine element_add_element

        module subroutine element_end (me, unit)
            implicit none
            !! this ends an xml element block
            class(xml_element_dt), intent(in) :: me      !! xml element derived type
            integer(i4k),          intent(in) :: unit    !! file unit # to write to
        end subroutine element_end

        recursive module subroutine element_write (me, unit)
            implicit none
            !! this writes an xml element block
            class(xml_element_dt), intent(in) :: me      !! xml element derived type
            integer(i4k),          intent(in) :: unit    !! file unit # to write to
        end subroutine element_write

        module subroutine replace_in_string (me, tag, value)
            implicit none
            !! replaces the existing value associated with tag with value
            class(xml_element_dt), intent(inout) :: me
            character(len=*),      intent(in)    :: tag
            character(len=*),      intent(in)    :: value
        end subroutine replace_in_string

        module subroutine xml_file_setup (me, filename, open_status, close_status, form, access, unit, encoding)
            implicit none
            !! author: Ian Porter
            !! date: 05/02/2019
            !!
            !! establishes the file information
            !!
            class(xml_file_dt), intent(inout) :: me                  !! xml file dt
            character(len=*),   intent(in)    :: filename            !! file name
            character(len=*),   intent(in), optional :: open_status  !! file open status
            character(len=*),   intent(in), optional :: close_status !! file close status
            character(len=*),   intent(in), optional :: form         !! file format(formatted or unformatted)
            character(len=*),   intent(in), optional :: access       !! file access type
            integer(i4k),       intent(in), optional :: unit         !! requested file unit #
            character(len=*),   intent(in), optional :: encoding     !! file encoding type
        end subroutine xml_file_setup

        recursive module subroutine xml_begin (me)
            implicit none
            !! author: Ian Porter
            !! date: 05/03/2019
            !!
            !! begins the writing of the xml file
            !!
            class(xml_file_dt), intent(inout) :: me                  !! xml file dt
        end subroutine xml_begin

        recursive module subroutine xml_add (me, element)
            implicit none
            !! author: Ian Porter
            !! date: 05/03/2019
            !!
            !! this adds data inside of the file
            !!
            class(xml_file_dt),    intent(inout) :: me               !! xml file dt
            class(xml_element_dt), intent(in)    :: element
        end subroutine xml_add

        recursive module subroutine xml_end (me)
            implicit none
            !! author: Ian Porter
            !! date: 05/03/2019
            !!
            !! ends the writing of the xml file
            !!
            class(xml_file_dt), intent(in) :: me                     !! xml file dt
        end subroutine xml_end

        recursive module subroutine xml_write (me)
            implicit none
            !! author: Ian Porter
            !! date: 05/03/2019
            !!
            !! writes the xml file
            !!
            class(xml_file_dt), intent(inout) :: me                  !! xml file dt
        end subroutine xml_write

        recursive module subroutine gcc_bug_workaround_allocate (me, addfoo, oldfoo)
            implicit none
            !! gcc work-around for allocating a multi-dimension derived type w/ allocatable character strings
            !! when trying to increase the size of the foo array by 1
            type(xml_element_dt), dimension(:), intent(inout), allocatable :: me      !! dt to be resized to [oldfoo, addfoo]
            type(xml_element_dt), dimension(:), intent(in), optional       :: oldfoo  !! old array of dts
            type(xml_element_dt),               intent(in), optional       :: addfoo  !! new dt to add to array
        end subroutine gcc_bug_workaround_allocate

        recursive module subroutine gcc_bug_workaround_deallocate_array (me)
            implicit none
            !! gcc work-around for deallocating a multi-dimension derived type w/ allocatable character strings
            type(xml_element_dt), dimension(:), intent(inout), allocatable :: me
        end subroutine gcc_bug_workaround_deallocate_array

        recursive module subroutine gcc_bug_workaround_deallocate_single (me)
            implicit none
            !! gcc work-around for deallocating a multi-dimension derived type w/ allocatable character strings
            class(xml_element_dt), intent(inout) :: me
        end subroutine gcc_bug_workaround_deallocate_single

        recursive module subroutine gcc_bug_deallocate_string_dt (me)
            implicit none
            !! gcc work-around
            class(string_dt), intent(inout) :: me
        end subroutine gcc_bug_deallocate_string_dt

        recursive module subroutine gcc_bug_workaround_deallocate_xml_file_dt (me)
            implicit none
            !! gcc work-around
            class(xml_file_dt), intent(inout) :: me
        end subroutine gcc_bug_workaround_deallocate_xml_file_dt

        module function convert_format_to_string (format) result(string)
            implicit none
            !! converts the format integer to string
            integer(i4k), intent(in) :: format
            character(len=:), allocatable :: string
        end function convert_format_to_string

        module function convert_string_to_format(string) result(format)
            implicit none
            !! converts the format integer to string
            character(len=*), intent(in) :: string
            integer(i4k) :: format
        end function convert_string_to_format

    end interface

end module xml
