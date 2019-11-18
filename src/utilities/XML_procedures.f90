submodule (xml) xml_procedures
    use precision, only : i4k
    implicit none
    !! author: Ian Porter
    !! date: 04/02/2019
    !!
    !! this implements the simple xml format writer
    !!
    integer, parameter :: def_offset = 4          !! default # of leading spaces
    character(len=:), allocatable :: prior_offset !! number of leading spaces for prior xml block
    character(len=*), parameter :: version = '<?xml version="1.0" encoding="utf-8"?>'
    type(xml_element_dt), dimension(:), allocatable, save :: gcc_bug_tmp_element_dt

contains

    module procedure element_setup
        implicit none
        !! this sets up the information needed to define the xml element block
        integer(i4k) :: i, my_offset

        call me%deallocate()

        allocate(me%name,source=name)
        if (present(string)) then
            if (len_trim(string) == 0) then
                allocate(me%additional_data,source='')
            else
                if (string(1:1) == " ") then
                    !! don't add an extra space between the name and the string
                    allocate(me%additional_data, source=string)
                else
                    allocate(me%additional_data, source=" " // string)
                end if
            end if
        end if

        my_offset = def_offset
        if (present(offset)) then
            if (offset >= 0) my_offset = offset
        end if

        do i = 0, my_offset
            if (i == 0) then
                allocate(me%offset,source='')
            else
                me%offset = me%offset // ' '
            end if
        end do

    end procedure element_setup

    module procedure element_begin
        implicit none
        !! this begins an xml element block
        character(len=:), allocatable :: tmp_offset

        select case (file_format)
        case (ascii)
            write(unit,'(a)',advance='no') prior_offset // '<' // me%name // me%additional_data // '>' // new_line('a')
        case (binary)
            write(unit) '<' // me%name // me%additional_data // '>' // new_line('a')
        end select

        allocate(tmp_offset,source=prior_offset // me%offset)   !! set the new offset length
        call move_alloc(tmp_offset,prior_offset)

    end procedure element_begin

    module procedure element_add_real32
        use misc, only : convert_to_string
        implicit none
        !! this adds data inside of an xml element block
        integer(i4k) :: i
        type(real32_dt) :: real32
        type(string_dt), dimension(:), allocatable :: tmp_string_dt
        type(real32_dt), dimension(:), allocatable :: tmp_real32_dt
        character(len=:), allocatable :: string

        select case (file_format)
        case (binary)
            if (.not. allocated(me%real32)) then
                allocate(me%real32(0))
            end if
            allocate(real32%val, source=var)
            allocate(tmp_real32_dt, source = [me%real32, real32])
            call move_alloc(tmp_real32_dt, me%real32)

        case (ascii)
            if (.not. allocated(me%string)) then
                allocate(me%string(0))
            end if

            allocate(tmp_string_dt(1:size(me%string)+1))
            tmp_string_dt(1:size(me%string)) = me%string
            call move_alloc(tmp_string_dt, me%string)

            do i = 1, size(var)
                if (i == 1) then
                    allocate(string, source=convert_to_string(var(i)))
                else
                    string = string // " " // convert_to_string(var(i))
                end if
            end do

            associate (my_entry => ubound(me%string,dim=1))
                allocate(me%string(my_entry)%text,source= string // new_line('a'))
            end associate
        end select

    end procedure element_add_real32

    module procedure element_add_real64
        use misc, only : convert_to_string
        implicit none
        !! this adds data inside of an xml element block
        type(real64_dt) :: real64
        type(string_dt), dimension(:), allocatable :: tmp_string_dt
        type(real64_dt), dimension(:), allocatable :: tmp_real64_dt

        select case (file_format)
        case (binary)
            if (.not. allocated(me%real64)) then
                allocate(me%real64(0))
            end if
            allocate(real64%val, source=var)
            allocate(tmp_real64_dt, source = [me%real64, real64])
            call move_alloc(tmp_real64_dt, me%real64)
        case (ascii)
            if (.not. allocated(me%string)) then
                allocate(me%string(0))
            end if

            allocate(tmp_string_dt(1:size(me%string)+1))
            tmp_string_dt(1:size(me%string)) = me%string
            call move_alloc(tmp_string_dt, me%string)

            associate (my_entry => ubound(me%string,dim=1))
                allocate(me%string(my_entry)%text,source=convert_to_string(var) // new_line('a'))
            end associate
        end select

    end procedure element_add_real64

    module procedure element_add_int32
        use misc, only : convert_to_string
        implicit none
        !! this adds data inside of an xml element block
        integer(i4k) :: i
        type(string_dt), dimension(:), allocatable :: tmp_string_dt
        character(len=:), allocatable :: string

        if (.not. allocated(me%string)) then
            allocate(me%string(0))
        end if

        allocate(tmp_string_dt(1:size(me%string)+1))
        tmp_string_dt(1:size(me%string)) = me%string
        call move_alloc(tmp_string_dt, me%string)

        do i = 1, size(var)
            if (i == 1) then
                allocate(string, source=convert_to_string(var(i)))
            else
                string = string // " " // convert_to_string(var(i))
            end if
        end do

        associate (my_entry => ubound(me%string,dim=1))
            allocate(me%string(my_entry)%text,source= string // new_line('a'))
        end associate

    end procedure element_add_int32

    module procedure element_add_int64
        use misc, only : convert_to_string
        implicit none
        !! this adds data inside of an xml element block
        integer(i4k) :: i
        type(string_dt), dimension(:), allocatable :: tmp_string_dt
        character(len=:), allocatable :: string

        if (.not. allocated(me%string)) then
            allocate(me%string(0))
        end if

        allocate(tmp_string_dt(1:size(me%string)+1))
        tmp_string_dt(1:size(me%string)) = me%string
        call move_alloc(tmp_string_dt, me%string)

        do i = 1, size(var)
            if (i == 1) then
                allocate(string, source=convert_to_string(var(i)))
            else
                string = string // " " // convert_to_string(var(i))
            end if
        end do

        associate (my_entry => ubound(me%string,dim=1))
            allocate(me%string(my_entry)%text,source= string // new_line('a'))
        end associate

    end procedure element_add_int64

    module procedure element_add_logical
        use misc, only : convert_to_string
        implicit none
        !! this adds data inside of an xml element block
        integer(i4k) :: i
        type(string_dt), dimension(:), allocatable :: tmp_string_dt
        character(len=:), allocatable :: string

        if (.not. allocated(me%string)) then
            allocate(me%string(0))
        end if

        allocate(tmp_string_dt(1:size(me%string)+1))
        tmp_string_dt(1:size(me%string)) = me%string
        call move_alloc(tmp_string_dt, me%string)

        do i = 1, size(var)
            if (i == 1) then
                allocate(string, source=convert_to_string(var(i)))
            else
                string = string // " " // convert_to_string(var(i))
            end if
        end do

        associate (my_entry => ubound(me%string,dim=1))
            allocate(me%string(my_entry)%text,source= string // new_line('a'))
        end associate

    end procedure element_add_logical

    module procedure element_add_string
        implicit none
        !! this adds data inside of an xml element block
        logical :: add_quotes
        type(string_dt), dimension(:), allocatable :: tmp_string_dt

        if (present(quotes)) then
            add_quotes = quotes
        else
            add_quotes = .true.  !! by default, add quotation marks around a string
        end if

        if (.not. allocated(me%string)) then
            allocate(me%string(0))
        end if

        allocate(tmp_string_dt(1:size(me%string)+1))
        tmp_string_dt(1:size(me%string)) = me%string
        call move_alloc(tmp_string_dt, me%string)

        associate (my_entry => ubound(me%string,dim=1))
            if (add_quotes) then
                allocate(me%string(my_entry)%text,source='"' // string // '"' // new_line('a'))
            else
                allocate(me%string(my_entry)%text,source= string // new_line('a'))
            end if
        end associate

    end procedure element_add_string

    module procedure element_add_element
        implicit none
        !! this adds an element inside of an xml element block
        type(xml_element_dt), dimension(:), allocatable :: tmp_element_dt
        !! this is how this routine should work (and does work w/ intel)
        !        if (.not. allocated(me%element)) then
        !            allocate(me%element(1),source=element)
        !        else
        !            allocate(tmp_element_dt,source=[ me%element(:), element ]) ! this segfaults at runtime
        !            call move_alloc(tmp_element_dt, me%element)
        !        end if
        !! this is a temporary work around
        if (.not. allocated(me%element)) then
            select type (element)
            class is (xml_element_dt)
                call gcc_bug_workaround_allocate(me%element, element)
            end select
        else
            select type (element)
            class is (xml_element_dt)
                call gcc_bug_workaround_allocate(tmp_element_dt, oldfoo=me%element)
                call gcc_bug_workaround_allocate(me%element, element, tmp_element_dt)
            end select
        end if
        call gcc_bug_workaround_deallocate (tmp_element_dt)

    end procedure element_add_element

    module procedure element_end
        implicit none
        !! this ends an xml element block
        character(len=:), allocatable :: tmp_offset

        associate (new_len => len(prior_offset) - len(me%offset))
            allocate(tmp_offset,source=prior_offset(1:new_len))
            call move_alloc(tmp_offset,prior_offset) !! reset the offset length
        end associate

        select case (file_format)
        case (ascii)
            write(unit,'(a)',advance='no') prior_offset // '</' // me%name // '>' // new_line('a')
        case (binary)
            write(unit) '</' // me%name // '>' // new_line('a')
        end select

    end procedure element_end

    module procedure element_write
        implicit none
        !! author: Ian Porter
        !! date: 05/03/2019
        !!
        !! writes the element to the file
        !!
        integer(i4k) :: i, j

        call me%begin(unit)

        if (allocated(me%string)) then
            do i = 1, size(me%string)
                select case (file_format)
                case (ascii)
                    write(unit,'(a)',advance='no') prior_offset // me%string(i)%text
                case (binary)
                    write(unit) me%string(i)%text
                end select
            end do
        else if (allocated(me%real32)) then
            do i = 1, size(me%real32)
                associate (n_vals => size(me%real32(i)%val))
                    write(unit) (me%real32(i)%val(j),j=1,n_vals)
                end associate
            end do
            write(unit) new_line('a')
        else if (allocated(me%real64)) then
            do i = 1, size(me%real64)
                associate (n_vals => size(me%real64(i)%val))
                    do j = 1, n_vals
                        write(unit) me%real64(i)%val(j)
                    end do
                end associate
            end do
            write(unit) new_line('a')
        end if

        if (allocated(me%element)) then
            do i = 1, size(me%element)
                call me%element(i)%write(unit)
            end do
        end if

        call me%end(unit)

    end procedure element_write

    module procedure replace_in_string
        implicit none
        !! replaces the existing value associated with tag with value

        !! find where "tag" is located

        !! find the end of the string associated with "tag"

        !! replace that value with value

        error stop 'error: replace_in_string in xml_procedures is not yet implemented.'

    end procedure replace_in_string

    module procedure xml_file_setup
        use misc, only : to_lowercase
        implicit none
        !! author: Ian Porter
        !! date: 05/02/2019
        !!
        !! establishes the xml file information
        !!

        allocate(me%filename, source=trim(filename))

        if (present(open_status)) then
            allocate(me%open_status, source=open_status)
        else
            allocate(me%open_status, source='unknown')
        end if
        if (present(close_status)) then
            allocate(me%close_status, source=close_status)
        else
            allocate(me%close_status, source='keep')
        end if
        if (present(form)) then
            select case (to_lowercase(form))
            case ('formatted')
                allocate(me%form, source='formatted')    !! ignore the user-defined form, even if present
                file_format = ascii
            case ('unformatted')
                allocate(me%form, source='unformatted')  !! ignore the user-defined form, even if present
                file_format = binary
            case default
                error stop 'error: unknown value for form. terminated in xml_file_setup'
            end select
        else
            select case (file_format)
            case (ascii)
                allocate(me%form, source='formatted')
                file_format = ascii
            case (binary)
                allocate(me%form, source='unformatted')
                file_format = binary
            case default
                error stop 'error: unknown value for file_format. terminated in xml_file_setup'
            end select
        end if

        if (present(unit)) then
            me%unit = unit
        else
            me%unit = 0
        end if

        file_format_text = convert_format_to_string (file_format)
        !        allocate(me%access, source='sequential') !! ignore the user-defined access, even if present
        allocate(me%access, source='stream') !! ignore the user-defined access, even if present

        if (.not. allocated(prior_offset)) allocate(prior_offset,source='')

    end procedure xml_file_setup

    module procedure xml_begin
        implicit none
        !! author: Ian Porter
        !! date: 05/03/2019
        !!
        !! begins the writing of the xml file
        !!

        if (.not. allocated(me%filename)) then
            write(0,*) 'warning: file name has not yet been set in xml_begin'
            call me%setup('dummy')
        end if

        call me%make_file()

        select case (file_format)
        case (ascii)
            write(me%unit,'(a)',advance='no') version // new_line('a')
        case (binary)
            write(me%unit) version // new_line('a')
        end select

    end procedure xml_begin

    module procedure xml_add
        implicit none
        !! author: Ian Porter
        !! date: 05/03/2019
        !!
        !! this adds data inside of the file
        !!
        type(xml_element_dt), dimension(:), allocatable :: tmp_element_dt

        !! this is how this routine should work (and does work w/ intel)
        !        if (.not. allocated(me%element)) then
        !            allocate(me%element(1),source=element)
        !        else
        !            allocate(tmp_element_dt,source=[ me%element(:), element ]) ! this segfaults at runtime
        !            call move_alloc(tmp_element_dt, me%element)
        !        end if
        !! this is a temporary work around
        if (.not. allocated(me%element)) then
            select type (element)
            class is (xml_element_dt)
                call gcc_bug_workaround_allocate(me%element, element)
            end select
        else
            select type (element)
            class is (xml_element_dt)
                call gcc_bug_workaround_allocate(tmp_element_dt, oldfoo=me%element)
                call gcc_bug_workaround_allocate(me%element, element, tmp_element_dt)
            end select
        end if

        call gcc_bug_workaround_deallocate (tmp_element_dt)

    end procedure xml_add

    module procedure xml_end
        implicit none
        !! author: Ian Porter
        !! date: 05/03/2019
        !!
        !! ends the writing of the xml file
        !!

        call me%close_file()

    end procedure xml_end

    module procedure xml_write
        implicit none
        !! author: Ian Porter
        !! date: 05/03/2019
        !!
        !! writes the xml file
        !!
        integer(i4k) :: i

        call me%begin()

        do i = 1, size(me%element)
            call me%element(i)%write(me%unit)
        end do

        call me%end()

    end procedure xml_write

    module procedure gcc_bug_workaround_allocate
        implicit none
        !! gcc work-around for allocating a multi-dimension derived type w/ allocatable character strings
        !! when trying to increase the size of the foo array by 1
        integer(i4k) :: i

        if (allocated(me)) call gcc_bug_workaround_deallocate(me)
        if (present(oldfoo)) then
            if (present(addfoo)) then
                allocate (me(size(oldfoo)+1))
            else
                allocate (me(size(oldfoo)))
            end if
            do i = 1, size(oldfoo)
                if (allocated(oldfoo(i)%name)) allocate(me(i)%name, source=oldfoo(i)%name)
                me(i)%unit = oldfoo(i)%unit
                if (allocated(oldfoo(i)%offset)) allocate(me(i)%offset, source=oldfoo(i)%offset)
                if (allocated(oldfoo(i)%additional_data)) &
                &  allocate(me(i)%additional_data, source=oldfoo(i)%additional_data)
                if (allocated(oldfoo(i)%string)) allocate(me(i)%string, source=oldfoo(i)%string)
                if (allocated(oldfoo(i)%real32)) allocate(me(i)%real32, source=oldfoo(i)%real32)
                if (allocated(oldfoo(i)%real64)) allocate(me(i)%real64, source=oldfoo(i)%real64)
                if (allocated(oldfoo(i)%element)) call gcc_bug_workaround_allocate(me(i)%element, oldfoo=oldfoo(i)%element)
            end do
        else
            allocate(me(1))
        end if
        if (present(addfoo)) then
            i = ubound(me,dim=1)
            if (allocated(addfoo%name)) allocate(me(i)%name, source=addfoo%name)
            me(i)%unit = addfoo%unit
            if (allocated(addfoo%offset)) allocate(me(i)%offset, source=addfoo%offset)
            if (allocated(addfoo%additional_data)) &
                &  allocate(me(i)%additional_data, source=addfoo%additional_data)
            if (allocated(addfoo%string)) allocate(me(i)%string, source=addfoo%string)
            if (allocated(addfoo%real32)) allocate(me(i)%real32, source=addfoo%real32)
            if (allocated(addfoo%real64)) allocate(me(i)%real64, source=addfoo%real64)
            if (allocated(addfoo%element)) call gcc_bug_workaround_allocate(me(i)%element, oldfoo=addfoo%element)
        end if

    end procedure gcc_bug_workaround_allocate

    module procedure gcc_bug_workaround_deallocate_array
        implicit none
        !! gcc work-around for deallocating a multi-dimension derived type w/ allocatable character strings
        integer(i4k) :: i

        if (allocated(me)) then
            do i = lbound(me,dim=1), ubound(me,dim=1)
                call gcc_bug_workaround_deallocate(me(i))
            end do
            if (allocated(me)) deallocate(me)
        end if

    end procedure gcc_bug_workaround_deallocate_array

    module procedure gcc_bug_workaround_deallocate_single
        implicit none
        !! gcc work-around for deallocating a multi-dimension derived type w/ allocatable character strings
        integer(i4k) :: i

        if (allocated(me%name))            deallocate(me%name)
        if (allocated(me%offset))          deallocate(me%offset)
        if (allocated(me%additional_data)) deallocate(me%additional_data)
        if (allocated(me%string)) then
            do i = lbound(me%string,dim=1), ubound(me%string,dim=1)
                call gcc_bug_deallocate_string_dt(me%string(i))
                !if (allocated(me%string(i))) deallocate(me%string(i))
            end do
            if (allocated(me%string)) deallocate(me%string)
        end if
        if (allocated(me%real32))     deallocate(me%real32)
        if (allocated(me%real64))     deallocate(me%real64)
        if (allocated(me%element)) then
            do i = lbound(me%element,dim=1), ubound(me%element,dim=1)
                call gcc_bug_workaround_deallocate (me%element(i))
            end do
            if (allocated(me%element)) deallocate(me%element)
        end if

    end procedure gcc_bug_workaround_deallocate_single

    module procedure gcc_bug_deallocate_string_dt
        implicit none
        !! gcc work-around to de-allocate the string derived type

        if (allocated(me%text)) deallocate(me%text)

    end procedure gcc_bug_deallocate_string_dt

    module procedure gcc_bug_workaround_deallocate_xml_file_dt
        implicit none
        !! gcc work-around to de-allocate the string derived type

        if (allocated(me%element)) call gcc_bug_workaround_deallocate (me%element)

    end procedure gcc_bug_workaround_deallocate_xml_file_dt

    module procedure convert_format_to_string
        implicit none
        !! converts the format integer to string

        select case (format)
        case (ascii)
            allocate(string,source=format_ascii)
        case (binary)
            allocate(string,source=format_binary)
        case (append)
            allocate(string,source=format_append)
        case default
            error stop 'error: undefined format in convert_format_to_string'
        end select

    end procedure convert_format_to_string

    module procedure convert_string_to_format
        use misc, only : to_lowercase
        implicit none
        !! converts the format integer to string

        select case (to_lowercase(string))
        case (format_ascii)
            format = ascii
        case (format_binary)
            format = binary
        case (format_append)
            format = append
        case default
            error stop 'error: undefined string in convert_string_to_format'
        end select

    end procedure convert_string_to_format

end submodule xml_procedures
