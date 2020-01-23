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
        integer(i4k) :: i, my_offset

        call me%deallocate()

        allocate(me%name,source=name)
        if (present(string)) then
            if (len_trim(string) > 0) then
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
                me%offset = 0
            else
                me%offset = me%offset + 1
            end if
        end do

    end procedure element_setup

    module procedure element_begin
        implicit none
        !! this begins an xml element block
        character(len=:), allocatable :: fmt

        fmt = get_offset_format(prior_offset)

        select case (file_format)
        case (ascii)
#ifdef INTEL_COMPILER
            if (allocated(me%additional_data)) then
                write(unit,fmt,advance='yes') '<' // me%name // me%additional_data // '>'
            else
                write(unit,fmt,advance='yes') '<' // me%name // '>'
            end if
#else
            if (allocated(me%additional_data)) then
                write(unit,fmt,advance='no') '<' // me%name // me%additional_data // '>' // new_line('a')
            else
                write(unit,fmt,advance='no') '<' // me%name // '>' // new_line('a')
            end if
#endif
        case (binary)
            if (allocated(me%additional_data)) then
                write(unit) '<' // me%name // me%additional_data // '>' // new_line('a')
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

        if (allocated(me%additional_data)) deallocate(me%additional_data)
        me%additional_data = additional_data

    end procedure update_additional_data

    module procedure get_additional_data
        implicit none

        if (allocated(me%additional_data)) then
            additional_data = me%additional_data
        else
            error stop 'error: get_additional_data, me%additional_data is not allocated'
        end if

    end procedure get_additional_data

    module procedure clear_data
        implicit none

        integer(i4k) :: i

        if (allocated(me%string)) then
            if (size(me%string) > 0) then
                do i = lbound(me%string,dim=1), ubound(me%string,dim=1)
                    call gcc_bug_deallocate_string_dt(me%string(i))
                end do
            end if
            if (allocated(me%string)) deallocate(me%string)
        end if
        if (allocated(me%int32))  deallocate(me%int32)
        if (allocated(me%int64))  deallocate(me%int64)
        if (allocated(me%real32)) deallocate(me%real32)
        if (allocated(me%real64)) deallocate(me%real64)
        if (allocated(me%element)) then
            do i = lbound(me%element,dim=1), ubound(me%element,dim=1)
                call me%element(i)%clear_data()
            end do
        end if

    end procedure clear_data

    module procedure clear_elements
        implicit none

        integer(i4k) :: i

        if (allocated(me%element)) then
write(output_unit,*) 'start of clear_elements. size: ',size(me%element)
            if (size(me%element) > 0) then
                do i = lbound(me%element,dim=1), ubound(me%element,dim=1)
write(output_unit,*) 'in loop of clear_elements. i: ',i
                    call gcc_bug_workaround_deallocate_single(me%element(i))
                end do
            end if
            if (allocated(me%element)) deallocate(me%element)
        end if

    end procedure clear_elements

    module procedure element_add_real32
        use misc, only : convert_to_string
        implicit none
        !! this adds data inside of an xml element block
        integer(i4k) :: i
        type(real32_dt) :: real32
        type(string_dt), dimension(:), allocatable :: tmp_string
        type(real32_dt), dimension(:), allocatable :: tmp_real32_dt
        character(len=:), allocatable :: string

        select case (file_format)
        case (binary)
            if (.not. allocated(me%real32)) then
                allocate(me%real32(0))
            end if
            allocate(real32%val, source=data)
            allocate(tmp_real32_dt, source = [me%real32, real32])
            call move_alloc(tmp_real32_dt, me%real32)

        case (ascii)
            if (.not. allocated(me%string)) then
                allocate(me%string(0))
            end if

            allocate(tmp_string(1:size(me%string)+1))
            tmp_string(1:size(me%string)) = me%string
            call move_alloc(tmp_string, me%string)

            do i = 1, size(data)
                if (i == 1) then
                    allocate(string, source=convert_to_string(data(i)))
                else
                    string = string // " " // convert_to_string(data(i))
                end if
            end do

            associate (my_entry => ubound(me%string,dim=1))
                allocate(me%string(my_entry)%text,source=string)
            end associate
        end select

    end procedure element_add_real32

    module procedure element_add_real64
        use misc, only : convert_to_string
        implicit none
        !! this adds data inside of an xml element block
        type(real64_dt) :: real64
        type(string_dt), dimension(:), allocatable :: tmp_string
        type(real64_dt), dimension(:), allocatable :: tmp_real64_dt

!        select case (file_format)
!        case (binary)
            if (.not. allocated(me%real64)) then
                allocate(me%real64(0))
            end if
            allocate(real64%val, source=data)
            allocate(tmp_real64_dt, source = [me%real64, real64])
            call move_alloc(tmp_real64_dt, me%real64)
!        case (ascii)
!            if (.not. allocated(me%string)) then
!                allocate(me%string(0))
!            end if
!
!            allocate(tmp_string(1:size(me%string)+1))
!            tmp_string(1:size(me%string)) = me%string
!            call move_alloc(tmp_string, me%string)
!
!            associate (my_entry => ubound(me%string,dim=1))
!                allocate(me%string(my_entry)%text,source=convert_to_string(data))
!            end associate
!        end select

    end procedure element_add_real64

    module procedure element_add_real64_2d
        use misc, only : convert_to_string
        implicit none
        !! this adds data inside of an xml element block
        type(real64_dt) :: real64
        type(real64_dt), dimension(:), allocatable :: tmp_real64

        if (.not. allocated(me%real64)) then
            allocate(me%real64(0))
            allocate(me%real64(0)%val_2d,source=data)
        else
            allocate(real64%val_2d, source=data)
            allocate(tmp_real64, source = [me%real64, real64])
            call move_alloc(tmp_real64, me%real64)
        end if

    end procedure element_add_real64_2d

    module procedure element_add_int32
!        use misc, only : convert_to_string
        implicit none
        !! this adds data inside of an xml element block
        type(int32_dt) :: int32
        type(int32_dt), dimension(:), allocatable :: tmp_int32

        if (.not. allocated(me%int32)) then
            allocate(me%int32(0))
        end if
        allocate(int32%val, source=data)
        allocate(tmp_int32, source = [me%int32, int32])
        call move_alloc(tmp_int32, me%int32)

    end procedure element_add_int32

    module procedure element_add_int64
        use misc, only : convert_to_string
        implicit none
        !! this adds data inside of an xml element block
        type(int64_dt) :: int64
        type(int64_dt), dimension(:), allocatable :: tmp_int64

        if (.not. allocated(me%int64)) then
            allocate(me%int64(0))
        end if
        allocate(int64%val, source=data)
        allocate(tmp_int64, source = [me%int64, int64])
        call move_alloc(tmp_int64, me%int64)

    end procedure element_add_int64

    module procedure element_add_logical
        use misc, only : convert_to_string
        implicit none
        !! this adds data inside of an xml element block
        integer(i4k) :: i
        type(string_dt), dimension(:), allocatable :: tmp_string
        character(len=:), allocatable :: string

        if (.not. allocated(me%string)) then
            allocate(me%string(0))
        end if

        allocate(tmp_string(1:size(me%string)+1))
        tmp_string(1:size(me%string)) = me%string
        call move_alloc(tmp_string, me%string)

        do i = 1, size(data)
            if (i == 1) then
                allocate(string, source=convert_to_string(data(i)))
            else
                string = string // " " // convert_to_string(data(i))
            end if
        end do

        associate (my_entry => ubound(me%string,dim=1))
            allocate(me%string(my_entry)%text,source= string)
        end associate

    end procedure element_add_logical

    module procedure element_add_string
        implicit none
        !! this adds data inside of an xml element block
        logical :: add_quotes
        type(string_dt), dimension(:), allocatable :: tmp_string

        if (present(quotes)) then
            add_quotes = quotes
        else
            add_quotes = .true.  !! by default, add quotation marks around a string
        end if

        if (.not. allocated(me%string)) then
            allocate(me%string(0))
        end if

        allocate(tmp_string(1:size(me%string)+1))
        tmp_string(1:size(me%string)) = me%string
        call move_alloc(tmp_string, me%string)

        associate (my_entry => ubound(me%string,dim=1))
            if (add_quotes) then
                allocate(me%string(my_entry)%text,source='"' // string // '"')
            else
                allocate(me%string(my_entry)%text,source=string)
            end if
        end associate

    end procedure element_add_string

    module procedure element_add_element
        implicit none
        !! this adds an element inside of an xml element block
        integer :: i
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
            write(output_unit,*) 'element_add_element not allocated(me%element)'
            select type (element)
            class is (xml_element_dt)
                call gcc_bug_workaround_allocate(me%element, element)
            class default
                error stop 'error in element_add_element. undefined class'
            end select
        else
            select type (element)
            class is (xml_element_dt)
write(output_unit,*) 'element_add_element 1'
                call gcc_bug_workaround_allocate(tmp_element_dt, oldfoo=me%element)
write(output_unit,*) 'element_add_element 2'
                call gcc_bug_workaround_allocate(me%element, element, tmp_element_dt)
write(output_unit,*) 'element_add_element 3'
            class default
                error stop 'error in element_add_element. undefined class'
            end select
        end if
write(output_unit,*) 'element_add_element 4'
        if (allocated(tmp_element_dt)) then
            call gcc_bug_workaround_deallocate_array_type(tmp_element_dt)
    write(output_unit,*) 'element_add_element 5'
            if (allocated(tmp_element_dt)) deallocate(tmp_element_dt)
      end if
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
        integer(i4k) :: i, j, k
        character(len=:), allocatable :: fmt

        if (prior_offset == -1) prior_offset = 0  !! This should only happen if trying to write
                                                  !! an element without an xml file type
        fmt = get_offset_format(prior_offset)

        call me%begin(unit)
write(output_unit,*) me%name
        if (allocated(me%string)) then
            do i = 1, size(me%string)
                select case (file_format)
                case (ascii)
#ifdef INTEL_COMPILER
                    write(unit,fmt,advance='yes') me%string(i)%text
#else
                    write(unit,fmt,advance='no') me%string(i)%text
#endif
                case (binary)
                    write(unit) me%string(i)%text
                end select
            end do
        else if (allocated(me%int32)) then
            do i = 1, size(me%int32)
                if (allocated(me%int32(i)%val)) then
                    associate (n_vals => size(me%int32(i)%val))
                        write(unit,*) (me%int32(i)%val(j),j=1,n_vals)
                    end associate
                end if
                if (allocated(me%int32(i)%val_2d)) then
                    associate (n_vals_1 => size(me%int32(i)%val_2d,dim=1), n_vals_2 => size(me%int32(i)%val_2d,dim=2))
                        do k = 1, n_vals_2
                            write(unit,*) (me%int32(i)%val_2d(j,k),j=1,n_vals_1)
                        end do
                    end associate
                end if
            end do
            if (file_format == binary) then
                write(unit) new_line('a')
            end if
        else if (allocated(me%int64)) then
            do i = 1, size(me%int64)
                if (allocated(me%int64(i)%val)) then
                    associate (n_vals => size(me%int64(i)%val))
                        write(unit) (me%int64(i)%val(j),j=1,n_vals)
                    end associate
                end if
                if (allocated(me%int64(i)%val_2d)) then
                    associate (n_vals_1 => size(me%int64(i)%val_2d,dim=1), n_vals_2 => size(me%int64(i)%val_2d,dim=2))
                        do k = 1, n_vals_2
                            write(unit) (me%int64(i)%val_2d(j,k),j=1,n_vals_1)
                        end do
                    end associate
                end if
            end do
            if (file_format == binary) then
                write(unit) new_line('a')
            end if
        else if (allocated(me%real32)) then
            do i = 1, size(me%real32)
                associate (n_vals => size(me%real32(i)%val))
                    write(unit) (me%real32(i)%val(j),j=1,n_vals)
                end associate
            end do
            if (file_format == binary) then
                write(unit) new_line('a')
            end if
        else if (allocated(me%real64)) then
            do i = 1, size(me%real64)
                associate (n_vals => size(me%real64(i)%val))
                    select case (file_format)
                    case (ascii)
                        write(unit,*) (me%real64(i)%val(j),j=1,n_vals)
                    case (binary)
                        write(unit) (me%real64(i)%val(j),j=1,n_vals)
                    end select
                end associate
            end do
            if (file_format == binary) then
                write(unit) new_line('a')
            end if
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
        type(xml_element_dt), dimension(:), allocatable :: tmp_element_dt
#ifdef INTEL_COMPILER
        !! this is how this routine should work (and does work w/ intel)
        if (.not. allocated(me%element)) then
            allocate(me%element(1),source=element)
        else
            allocate(tmp_element_dt,source=[ me%element(:), element ]) ! this segfaults at runtime for gfortran
            call move_alloc(tmp_element_dt, me%element)
        end if
#else
        !! this is a temporary work around
        write(output_unit,*) 'xml_add 0'
        if (.not. allocated(me%element)) then
            select type (element)
            class is (xml_element_dt)
                call gcc_bug_workaround_allocate(me%element, element)
            end select
        else
            write(output_unit,*) 'xml_add 0.5'
            select type (element)
            class is (xml_element_dt)
              write(output_unit,*) 'xml_add 1'
                call gcc_bug_workaround_allocate(tmp_element_dt, oldfoo=me%element)
              write(output_unit,*) 'xml_add 2'
                call gcc_bug_workaround_allocate(me%element, element, tmp_element_dt)
              write(output_unit,*) 'xml_add 3'
            end select
        end if

        call gcc_bug_workaround_deallocate_array_type(tmp_element_dt)
#endif
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
                if (allocated(oldfoo(i)%additional_data)) &
                    &  allocate(me(i)%additional_data, source=oldfoo(i)%additional_data)
                if (allocated(oldfoo(i)%int32))  allocate(me(i)%int32,  source=oldfoo(i)%int32)
                if (allocated(oldfoo(i)%int64))  allocate(me(i)%int64,  source=oldfoo(i)%int64)
                if (allocated(oldfoo(i)%string)) allocate(me(i)%string, source=oldfoo(i)%string)
                if (allocated(oldfoo(i)%real32)) allocate(me(i)%real32, source=oldfoo(i)%real32)
                if (allocated(oldfoo(i)%real64)) allocate(me(i)%real64, source=oldfoo(i)%real64)
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
            if (allocated(addfoo%additional_data)) &
                &  allocate(me(i)%additional_data, source=addfoo%additional_data)
            if (allocated(addfoo%int32))  allocate(me(i)%int32,  source=addfoo%int32)
            if (allocated(addfoo%int64))  allocate(me(i)%int64,  source=addfoo%int64)
            if (allocated(addfoo%string)) allocate(me(i)%string, source=addfoo%string)
            if (allocated(addfoo%real32)) allocate(me(i)%real32, source=addfoo%real32)
            if (allocated(addfoo%real64)) allocate(me(i)%real64, source=addfoo%real64)
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

        if (allocated(me%name)) then
write(output_unit,*) 'me%name: ',me%name
            deallocate(me%name)
        end if
        me%unit = output_unit
        me%offset = 0
        if (allocated(me%additional_data)) then
write(output_unit,*) 'me%additional_data: ',me%additional_data
            deallocate(me%additional_data)
        end if
        if (allocated(me%string)) then
write(output_unit,*) 'me%string is allocated '
            if (size(me%string) > 0) then
                do i = lbound(me%string,dim=1), ubound(me%string,dim=1)
                    call gcc_bug_deallocate_string_dt(me%string(i))
                end do
            end if
            if (allocated(me%string)) deallocate(me%string)
        end if
        if (allocated(me%int32))      deallocate(me%int32)
        if (allocated(me%int64))      deallocate(me%int64)
        if (allocated(me%real32))     deallocate(me%real32)
        if (allocated(me%real64))     deallocate(me%real64)
        if (allocated(me%element)) then
write(output_unit,*) 'me%element is allocated. size: ',size(me%element)
            if (size(me%element) > 0) then
                do i = lbound(me%element,dim=1), ubound(me%element,dim=1)
                    call gcc_bug_workaround_deallocate_single (me%element(i))
                end do
            end if
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
        integer :: i

        if (allocated(me%element)) then
            do i = lbound(me%element,dim=1), ubound(me%element,dim=1)
!                associate (e => me%element(i))
!                select type (me%element(i))
!                class is (xml_element_dt)
                    call me%element(i)%deallocate()
!                end select
!                end associate
            end do
            deallocate(me%element)
        end if

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
