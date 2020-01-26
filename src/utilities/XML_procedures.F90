submodule (xml) xml_procedures
    implicit none
    !! author: Ian Porter
    !! date: 04/02/2019
    !!
    !! this implements the simple xml format writer
    !!
    integer, parameter :: def_offset = 4          !! default # of leading spaces
    integer :: prior_offset = -1                  !! number of leading spaces for prior xml block
    character(len=*), parameter :: version = '<?xml version="1.0" encoding="utf-8"?>'
    type(xml_element_dt), dimension(:), allocatable, save :: gcc_bug_tmp_element_dt

contains

    module procedure element_setup
        implicit none
        !! this sets up the information needed to define the xml element block

        call free_me(me)

        allocate(me%name,source=name)
        if (present(string)) then
            if (len_trim(string) > 0) then
                if (string(1:1) == " ") then
                    !! don't add an extra space between the name and the string
                    allocate(me%header, source=string)
                else
                    allocate(me%header, source=" " // string)
                end if
            end if
        end if

        if (present(offset)) then
            me%offset = offset
        else
            me%offset = def_offset
        end if

    end procedure element_setup

    module procedure element_update
        implicit none
        !! this sets up the information needed to define the xml element block

        if (present(name)) then
            deallocate(me%name)
            allocate(me%name,source=name)
        end if
        if (present(string)) then
            if (allocated(me%header)) deallocate(me%header)
            if (len_trim(string) > 0) then
                if (string(1:1) == " ") then
                    !! don't add an extra space between the name and the string
                    allocate(me%header, source=string)
                else
                    allocate(me%header, source=" " // string)
                end if
            end if
            if (.not. allocated(me%header)) allocate(me%header,source='')
        end if

        if (present(offset)) then
            me%offset = offset
        end if

    end procedure element_update

    module procedure element_begin
        implicit none
        !! this begins an xml element block
        character(len=:), allocatable :: fmt

        fmt = get_offset_format(prior_offset)

        select case (file_format)
        case (ascii)
#ifdef INTEL_COMPILER
            if (allocated(me%header)) then
                write(unit,fmt,advance='yes') '<' // me%name // me%header // '>'
            else
                write(unit,fmt,advance='yes') '<' // me%name // '>'
            end if
#else
            if (allocated(me%header)) then
                write(unit,fmt,advance='no') '<' // me%name // me%header // '>' // new_line('a')
            else
                write(unit,fmt,advance='no') '<' // me%name // '>' // new_line('a')
            end if
#endif
        case (binary)
            if (allocated(me%header)) then
                write(unit) '<' // me%name // me%header // '>' // new_line('a')
            else
                write(unit) '<' // me%name // '>' // new_line('a')
            end if
        end select

        prior_offset = prior_offset + me%offset   !! set the new offset length

    end procedure element_begin

    module procedure update_name
        implicit none
        !! This updates the name

        if (allocated(me%name)) deallocate(me%name)
        me%name = name

    end procedure update_name

    module procedure update_names
        implicit none
        !! This updates all of the names
        integer :: i

        if (allocated(me%name)) deallocate(me%name)
        me%name = name
        if (allocated(me%element)) then
            do i = 1, size(me%element)
                call me%element(i)%update_names('P' // me%element(i)%name)
            end do
        end if

    end procedure update_names

    module procedure get_name
        implicit none

        if (allocated(me%name)) then
            name = me%name
        else
            error stop 'error: get_name, me%name is not allocated'
        end if

    end procedure get_name

    module procedure update_additional_data
        implicit none
        !! This updates the name

        if (allocated(me%header)) deallocate(me%header)
        me%header = additional_data

    end procedure update_additional_data

    module procedure get_additional_data
        implicit none

        if (allocated(me%header)) then
            additional_data = me%header
        else
            error stop 'error: get_additional_data, me%header is not allocated'
        end if

    end procedure get_additional_data

    module procedure clear_data
        implicit none
        !! Clears data

        integer(i4k) :: i

        if (allocated(me%data)) deallocate(me%data)
        if (allocated(me%element)) then
            do i = lbound(me%element,dim=1), ubound(me%element,dim=1)
                call me%element(i)%clear_data()
            end do
        end if

    end procedure clear_data

    module procedure clear_elements
        implicit none
        !! Clears any children elements

        integer(i4k) :: i

        if (allocated(me%element)) then
write(output_unit,*) 'start of clear_elements. size: ',size(me%element)
            if (size(me%element) > 0) then
                do i = lbound(me%element,dim=1), ubound(me%element,dim=1)
!write(output_unit,*) 'in loop of clear_elements. i: ',i
                    call free_me(me%element(i))
!                    call gcc_bug_workaround_deallocate_single(me%element(i))
                end do
            end if
            if (allocated(me%element)) deallocate(me%element)
        end if

    end procedure clear_elements

    module procedure element_add_real32
        implicit none
        !! this adds data inside of an xml element block

        call me%add([data])

    end procedure element_add_real32

    module procedure element_add_real32_1d
        implicit none
        !! this adds data inside of an xml element block
        type(real32_dt) :: tmp_data
        type(real32_dt), dimension(:), allocatable :: tmp_data_array

        select type (d => me%data)
        class is (real32_dt)
            if (.not. allocated(me%data)) then
                allocate(real32_dt::me%data(1))
                allocate(d(1)%val,source=data)
            else
                allocate(tmp_data%val, source=data)
                call move_alloc(tmp_data_array,me%data)
                allocate(me%data,source=[tmp_data_array, tmp_data])
            end if
        end select

    end procedure element_add_real32_1d

    module procedure element_add_real32_2d
        implicit none
        !! this adds data inside of an xml element block
        type(real32_dt) :: tmp_data
        type(real32_dt), dimension(:), allocatable :: tmp_data_array

        select type (d => me%data)
        class is (real32_dt)
            if (.not. allocated(me%data)) then
                allocate(real32_dt::me%data(1))
                allocate(d(1)%val_2d,source=data)
            else
                allocate(tmp_data%val_2d, source=data)
                call move_alloc(tmp_data_array,me%data)
                allocate(me%data,source=[tmp_data_array, tmp_data])
            end if
        end select

    end procedure element_add_real32_2d

    module procedure element_add_real64
        implicit none
        !! this adds data inside of an xml element block

        call me%add([data])

    end procedure element_add_real64

    module procedure element_add_real64_1d
        implicit none
        !! this adds data inside of an xml element block
        type(real64_dt) :: tmp_data
        type(real64_dt), dimension(:), allocatable :: tmp_data_array

        select type (d => me%data)
        class is (real64_dt)
            if (.not. allocated(me%data)) then
                allocate(real64_dt::me%data(1))
                allocate(d(1)%val,source=data)
            else
                allocate(tmp_data%val, source=data)
                call move_alloc(tmp_data_array,me%data)
                allocate(me%data,source=[tmp_data_array, tmp_data])
            end if
        end select

    end procedure element_add_real64_1d

    module procedure element_add_real64_2d
        use misc, only : convert_to_string
        implicit none
        !! this adds data inside of an xml element block
        type(real64_dt) :: tmp_data
        type(real64_dt), dimension(:), allocatable :: tmp_data_array

        select type (d => me%data)
        class is (real64_dt)
            if (.not. allocated(me%data)) then
                allocate(real64_dt::me%data(1))
                allocate(d(1)%val_2d,source=data)
            else
                allocate(tmp_data%val_2d, source=data)
                call move_alloc(tmp_data_array,me%data)
                allocate(me%data,source=[tmp_data_array, tmp_data])
            end if
        end select

    end procedure element_add_real64_2d

    module procedure element_add_int32
        implicit none
        !! this adds data inside of an xml element block
        type(int32_dt) :: tmp_data
        type(int32_dt), dimension(:), allocatable :: tmp_data_array

        select type (d => me%data)
        class is (int32_dt)
            if (.not. allocated(me%data)) then
                allocate(int32_dt::me%data(1))
                allocate(d(1)%val,source=data)
            else
                allocate(tmp_data%val, source=data)
                call move_alloc(tmp_data_array,me%data)
                allocate(me%data,source=[tmp_data_array, tmp_data])
            end if
        end select

    end procedure element_add_int32

    module procedure element_add_int64
        use misc, only : convert_to_string
        implicit none
        !! this adds data inside of an xml element block
        type(int64_dt) :: tmp_data
        type(int64_dt), dimension(:), allocatable :: tmp_data_array

        select type (d => me%data)
        class is (int64_dt)
            if (.not. allocated(me%data)) then
                allocate(int64_dt::me%data(1))
                allocate(d(1)%val,source=data)
            else
                allocate(tmp_data%val, source=data)
                call move_alloc(tmp_data_array,me%data)
                allocate(me%data,source=[tmp_data_array, tmp_data])
            end if
        end select

    end procedure element_add_int64

    module procedure element_add_logical
        implicit none
        !! this adds data inside of an xml element block
        type(logical_dt) :: tmp_data
        type(logical_dt), dimension(:), allocatable :: tmp_data_array

        select type (d => me%data)
        class is (logical_dt)
            if (.not. allocated(me%data)) then
                allocate(logical_dt::me%data(1))
                allocate(d(1)%val,source=data)
            else
                allocate(tmp_data%val, source=data)
                call move_alloc(tmp_data_array,me%data)
                allocate(me%data,source=[tmp_data_array, tmp_data])
            end if
        end select

    end procedure element_add_logical

    module procedure element_add_string
        implicit none
        !! this adds data inside of an xml element block
        logical :: add_quotes
        character(len=:), allocatable :: new_string
        type(string_dt) :: tmp_data
        type(string_dt), dimension(:), allocatable :: tmp_data_array

        if (present(quotes)) then
            add_quotes = quotes
        else
            add_quotes = .true.  !! by default, add quotation marks around a string
        end if

        if (add_quotes) then
            allocate(new_string,source='"' // data // '"')
        else
            allocate(new_string,source=data)
        end if

        select type (d => me%data)
        class is (string_dt)
            if (.not. allocated(me%data)) then
                allocate(string_dt::me%data(1))
                allocate(d(1)%val,source=new_string)
            else
                allocate(tmp_data%val, source=new_string)
                call move_alloc(tmp_data_array,me%data)
                allocate(me%data,source=[tmp_data_array, tmp_data])
                !allocate(tmp_data_array, source = [me%data, tmp_data])
!                call move_alloc(tmp_data_array, me%data)
            end if
        end select

    end procedure element_add_string

    module procedure element_add_element
        implicit none
        !! this adds an element inside of an xml element block
        integer :: i
        type(xml_element_dt) :: tmp_element
        type(xml_element_dt), dimension(:), allocatable :: tmp_element_array
write(output_unit,*) 'entering element_add_element.'
write(output_unit,*) ' my name:  ',me%name
        tmp_element = element
write(output_unit,*) 'Adding in: ',tmp_element%name

        if (allocated(me%element)) then
            allocate(tmp_element_array(lbound(me%element,dim=1):ubound(me%element,dim=1)))
            do i = lbound(me%element,dim=1), ubound(me%element,dim=1)
                tmp_element_array(i) = me%element(i)
                call free_me(me%element(i))
            end do
            if (allocated(me%element)) deallocate(me%element)
            allocate(me%element(lbound(tmp_element_array,dim=1):ubound(tmp_element_array,dim=1)+1))
            do i = 1, ubound(tmp_element_array,dim=1)
                me%element(i) = tmp_element_array(i)
            end do
            do i = lbound(tmp_element_array,dim=1),ubound(tmp_element_array,dim=1)
                call free_me(tmp_element_array(i))
            end do
            if (allocated(tmp_element_array)) deallocate(tmp_element_array)
            associate (i => size(me%element))
                me%element(i) = tmp_element
            end associate
        else
            allocate(me%element(1),source=tmp_element)
        end if

        call free_me(tmp_element)
write(output_unit,*) 'leaving element_add_element'
    end procedure element_add_element

    module procedure element_end
        implicit none
        !! this ends an xml element block
        character(len=:), allocatable :: fmt

        prior_offset = prior_offset - me%offset !! reset the offset length

        fmt = get_offset_format(prior_offset)

        select case (file_format)
        case (ascii)
#ifdef INTEL_COMPILER
            write(unit,fmt,advance='yes') '</' // me%name // '>'
#else
            write(unit,fmt,advance='no') '</' // me%name // '>' // new_line('a')
#endif
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
        integer(i4k) :: i, j!, k
        character(len=:), allocatable :: fmt

        if (prior_offset == -1) prior_offset = 0  !! This should only happen if trying to write
                                                  !! an element without an xml file type
        fmt = get_offset_format(prior_offset)

        call me%begin(unit)

        if (allocated(me%data)) then
write(output_unit,*) 'me%name: ',me%name
            select type (d => me%data)
            class is (string_dt)
                do i = 1, size(d)
                    if (allocated(d(i)%val)) then
                        select case (file_format)
                        case (ascii)
#ifdef INTEL_COMPILER
                            write(unit,fmt,advance='yes') d(i)%val
#else
                            write(unit,fmt,advance='no') d(i)%val
#endif
                        case (binary)
                            write(unit) d(i)%val
                        end select
                    end if
                end do
            class is (int32_dt)
                do i = 1, size(d)
                    if (allocated(d(i)%val)) then
                        select case (file_format)
                        case (ascii)
                            write(unit,*) d(i)%val(:)
                        case (binary)
                            write(unit,*) d(i)%val(:)
                        end select
                    else if (allocated(d(i)%val_2d)) then
                        do j = 1, size(d(i)%val_2d,dim=2)
                            select case (file_format)
                            case (ascii)
                                write(unit,*) d(i)%val_2d(:,j)
                            case (binary)
                                write(unit) d(i)%val_2d(:,j)
                            end select
                        end do
                    end if
                    if (file_format == binary) then
                        write(unit) new_line('a')
                    end if
                end do
            class is (int64_dt)
                do i = 1, size(d)
                    if (allocated(d(i)%val)) then
                        select case (file_format)
                        case (ascii)
                            write(unit,*) d(i)%val(:)
                        case (binary)
                            write(unit,*) d(i)%val(:)
                        end select
                    else if (allocated(d(i)%val_2d)) then
                        do j = 1, size(d(i)%val_2d,dim=2)
                            select case (file_format)
                            case (ascii)
                                write(unit,*) d(i)%val_2d(:,j)
                            case (binary)
                                write(unit) d(i)%val_2d(:,j)
                            end select
                        end do
                    end if
                    if (file_format == binary) then
                        write(unit) new_line('a')
                    end if
                end do
            class is (real32_dt)
                do i = 1, size(d)
                    if (allocated(d(i)%val)) then
                        select case (file_format)
                        case (ascii)
                            write(unit,*) d(i)%val(:)
                        case (binary)
                            write(unit,*) d(i)%val(:)
                        end select
                    else if (allocated(d(i)%val_2d)) then
                        do j = 1, size(d(i)%val_2d,dim=2)
                            select case (file_format)
                            case (ascii)
                                write(unit,*) d(i)%val_2d(:,j)
                            case (binary)
                                write(unit) d(i)%val_2d(:,j)
                            end select
                        end do
                    end if
                    if (file_format == binary) then
                        write(unit) new_line('a')
                    end if
                end do
            class is (real64_dt)
                do i = 1, size(d)
                    if (allocated(d(i)%val)) then
                        select case (file_format)
                        case (ascii)
                            write(unit,*) d(i)%val(:)
                        case (binary)
                            write(unit,*) d(i)%val(:)
                        end select
                    else if (allocated(d(i)%val_2d)) then
                        do j = 1, size(d(i)%val_2d,dim=2)
                            select case (file_format)
                            case (ascii)
                                write(unit,*) d(i)%val_2d(:,j)
                            case (binary)
                                write(unit) d(i)%val_2d(:,j)
                            end select
                        end do
                    end if
                    if (file_format == binary) then
                        write(unit) new_line('a')
                    end if
                end do
            class is (logical_dt)
                do i = 1, size(d)
                    if (allocated(d(i)%val)) then
                        select case (file_format)
                        case (ascii)
                            write(unit,*) d(i)%val(:)
                        case (binary)
                            write(unit,*) d(i)%val(:)
                        end select
                    else if (allocated(d(i)%val_2d)) then
                        do j = 1, size(d(i)%val_2d,dim=2)
                            select case (file_format)
                            case (ascii)
                                write(unit,*) d(i)%val_2d(:,j)
                            case (binary)
                                write(unit) d(i)%val_2d(:,j)
                            end select
                        end do
                    end if
                    if (file_format == binary) then
                        write(unit) new_line('a')
                    end if
                end do
            end select
        else
            !! Nothing to write
        end if

        if (allocated(me%element)) then
            do i = 1, size(me%element)
                call me%element(i)%write(unit)
            end do
        end if

        call me%end(unit)

    end procedure element_write

    module procedure assign_xml_element
        implicit none
        integer :: i

        if (allocated(you%name)) me%name = you%name
        me%unit   = you%unit
        me%offset = you%offset
        if (allocated(you%header)) me%header = you%header
        if (allocated(you%data))  me%data = you%data
        if (allocated(you%element)) then
            allocate(me%element(lbound(you%element,dim=1):ubound(you%element,dim=1)))
            do i = lbound(you%element,dim=1),ubound(you%element,dim=1)
                me%element(i) = you%element(i)
            end do
        end if

    end procedure assign_xml_element

    module procedure replace_in_string
        implicit none
        !! replaces the existing value associated with tag with value

        !! find where "tag" is located

        !! find the end of the string associated with "tag"

        !! replace that value with value

        error stop 'Error: replace_in_string in xml_procedures is not yet implemented.'

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
                error stop 'Error: unknown value for form. terminated in xml_file_setup'
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
                error stop 'Error: unknown value for file_format. terminated in xml_file_setup'
            end select
        end if

        if (present(unit)) then
            me%unit = unit
        else
            me%unit = 0
        end if

        file_format_text = convert_format_to_string (file_format)
        !        allocate(me%access, source='sequential') !! ignore the user-defined access, even if present
        allocate(me%access, source='stream')  !! ignore the user-defined access, even if present
        allocate(me%encoding, source='utf-8') !! ignore the user-defined encoding, even if present

        if (prior_offset == -1) prior_offset = 0

    end procedure xml_file_setup

    module procedure xml_file_update
        implicit none

        if (present(filename)) then
            if (allocated(me%filename)) deallocate(me%filename)
            me%filename = filename
        end if

    end procedure xml_file_update

    module procedure xml_begin
        use iso_fortran_env, only : output_unit
        implicit none
        !! author: Ian Porter
        !! date: 05/03/2019
        !!
        !! begins the writing of the xml file
        !!
        character(len=:), allocatable :: fmt

        fmt = get_offset_format(prior_offset)

        if (.not. allocated(me%filename)) then
            write(output_unit,*) 'warning: file name has not yet been set in xml_begin'
            call me%setup('dummy')
        end if

        call me%make_file()

        select case (file_format)
        case (ascii)
#ifdef INTEL_COMPILER
            write(me%unit,fmt,advance='yes') version
#else
            write(me%unit,fmt,advance='no') version // new_line('a')
#endif
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

write(output_unit,*) 'entering xml_add'
        allocate(me%element,source=element)
write(output_unit,*) 'leaving  xml_add'
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

        call me%element%write(me%unit)

        call me%end()

    end procedure xml_write

    module procedure gcc_bug_workaround_allocate
        implicit none
        !! gcc work-around for allocating a multi-dimension derived type w/ allocatable character strings
        !! when trying to increase the size of the foo array by 1
        integer(i4k) :: i
write(output_unit,*) 'entering gcc_bug_workaround_allocate'
        if (allocated(me)) call gcc_bug_workaround_deallocate_array_type(me)

        if (present(oldfoo)) then
write(output_unit,*) 'oldfoo is present'
            if (present(addfoo)) then
                allocate (me(size(oldfoo)+1))
            else
                allocate (me(size(oldfoo)))
            end if
            do i = 1, size(oldfoo)
write(output_unit,*) 'i: ',i
                if (allocated(oldfoo(i)%name)) allocate(me(i)%name, source=oldfoo(i)%name)
write(output_unit,*) 'oldfoo(i)%name: ',oldfoo(i)%name
write(output_unit,*) 'oldfoo(i)%unit: ',oldfoo(i)%unit
                me(i)%unit = oldfoo(i)%unit
                me(i)%offset = oldfoo(i)%offset
                if (allocated(oldfoo(i)%header)) &
                    &  allocate(me(i)%header, source=oldfoo(i)%header)
                if (allocated(oldfoo(i)%data))  allocate(me(i)%data,  source=oldfoo(i)%data)
                if (allocated(oldfoo(i)%element)) then
write(output_unit,*) 'oldfoo(i)%element is allocated. calling gcc_bug_workaround_allocate'
                    call gcc_bug_workaround_allocate(me(i)%element, oldfoo=oldfoo(i)%element)
write(output_unit,*) 'oldfoo(i)%element was allocated. finished calling gcc_bug_workaround_allocate'
                end if
            end do
        else
            allocate(me(1))
        end if
        if (present(addfoo)) then
write(output_unit,*) 'addfoo is present'
            i = ubound(me,dim=1)
write(output_unit,*) 'i',i
write(output_unit,*) 'addfoo%name: ',addfoo%name
            if (allocated(addfoo%name)) allocate(me(i)%name, source=addfoo%name)
write(output_unit,*) 'addfoo%unit: ',addfoo%unit
            me(i)%unit = addfoo%unit
            me(i)%offset = addfoo%offset
            if (allocated(addfoo%header)) &
                &  allocate(me(i)%header, source=addfoo%header)
            if (allocated(addfoo%data))  allocate(me(i)%data,  source=addfoo%data)
            if (allocated(addfoo%element)) then
write(output_unit,*) 'addfoo(i)%element is allocated. calling gcc_bug_workaround_allocate'
              call gcc_bug_workaround_allocate(me(i)%element, oldfoo=addfoo%element)
write(output_unit,*) 'addfoo(i)%element was allocated. finished calling gcc_bug_workaround_allocate'
            end if
        end if
write(output_unit,*) 'leaving gcc_bug_workaround_allocate'
    end procedure gcc_bug_workaround_allocate

    module procedure gcc_bug_workaround_deallocate_array
        implicit none
        !! gcc work-around for deallocating a multi-dimension derived type w/ allocatable character strings
        integer(i4k) :: i

        if (allocated(me)) then
            if (size(me) > 0) then
                do i = lbound(me,dim=1), ubound(me,dim=1)
                    call gcc_bug_workaround_deallocate(me(i))
                end do
                deallocate(me)
            end if
        end if

    end procedure gcc_bug_workaround_deallocate_array

    module procedure gcc_bug_workaround_deallocate_array_type
        implicit none
        !! gcc work-around for deallocating a multi-dimension derived type w/ allocatable character strings
        integer(i4k) :: i

        if (allocated(me)) then
            if (size(me) > 0) then
                do i = lbound(me,dim=1), ubound(me,dim=1)
                    call gcc_bug_workaround_deallocate(me(i))
                end do
                deallocate(me)
            end if
        end if

    end procedure gcc_bug_workaround_deallocate_array_type

    module procedure gcc_bug_workaround_deallocate_single
        implicit none
        !! gcc work-around for deallocating a multi-dimension derived type w/ allocatable character strings
        integer(i4k) :: i

        if (allocated(me%element)) then
            if (size(me%element) > 0) then
write(output_unit,*) 'me%element is allocated. size: ',size(me%element),' name: ',me%name
                do i = lbound(me%element,dim=1), ubound(me%element,dim=1)
                    !call gcc_bug_workaround_deallocate_single (me%element(i))
                    call free_me(me%element(i))
                end do
            end if
            if (allocated(me%element)) deallocate(me%element)
        else
            if (allocated(me%name)) then
write(output_unit,*) 'me%name: ',me%name
                deallocate(me%name)
            end if
            me%unit = output_unit
            me%offset = 0
            if (allocated(me%header)) then
write(output_unit,*) 'me%header: ',me%header
                deallocate(me%header)
            end if
            if (allocated(me%data))       deallocate(me%data)
        end if

    end procedure gcc_bug_workaround_deallocate_single

    module procedure gcc_bug_deallocate_string_dt
        implicit none
        !! gcc work-around to de-allocate the string derived type

        if (allocated(me%val)) deallocate(me%val)

    end procedure gcc_bug_deallocate_string_dt

    module procedure gcc_bug_workaround_deallocate_xml_file_dt
        implicit none
        !! gcc work-around to de-allocate the string derived type

        if (allocated(me%element)) then
            call free_me(me%element)
            deallocate(me%element)
        end if

    end procedure gcc_bug_workaround_deallocate_xml_file_dt

    recursive subroutine destroy (me)
        type(xml_element_dt), intent(out) :: me
        integer :: i

        if (allocated(me%name))            deallocate(me%name)
        me%unit = output_unit
        me%offset = 0
        if (allocated(me%header)) deallocate(me%header)
        if (allocated(me%data))   deallocate(me%data)
        if (allocated(me%element)) then
            if (ubound(me%element,dim=1) > 0) then
                do i = lbound(me%element,dim=1),ubound(me%element,dim=1)
                    call destroy(me%element(i))
                end do
            end if
            deallocate(me%element)
        end if

    end subroutine destroy

    recursive subroutine free_me (me)
        type(xml_element_dt), intent(inout) :: me
        integer :: i

        if (allocated(me%element)) then
            do i = lbound(me%element,dim=1),ubound(me%element,dim=1)
                call free_me(me%element(i))
            end do
            deallocate(me%element)
        else
write(output_unit,*) 'getting freed in free_me. me%data: '
            if (allocated(me%data)) deallocate(me%data)
write(output_unit,*) 'getting freed in free_me. me%name: ',me%name
            if (allocated(me%name)) deallocate(me%name)
write(output_unit,*) 'getting freed in free_me. me%header: ',me%header
            if (allocated(me%header)) deallocate(me%header)

            me%unit = output_unit
            me%offset = 0

        end if

    end subroutine free_me

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
            error stop 'Error: undefined format in convert_format_to_string'
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
            error stop 'Error: undefined string in convert_string_to_format'
        end select

    end procedure convert_string_to_format

    function get_offset_format (offset) result (fmt)
        implicit none
        !! Ian Porter
        !! Takes an integer string for offset spacing and converts it to a proper format statement
        integer(i4k), intent(in) :: offset
        character(len=:), allocatable :: fmt
        character(len=20) :: string

        select case (offset)
        case (:-1)
            error stop 'Error in get_offset_format. spacing is < 0'
        case (0)
            fmt = '(a)'
        case (1:)
            write(string,'("(",I0,"x,a)")') prior_offset+1
            fmt = trim(adjustl(string))
        end select

    end function get_offset_format

end submodule xml_procedures
