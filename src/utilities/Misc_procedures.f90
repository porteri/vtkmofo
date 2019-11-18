submodule (misc) misc_procedures
    use precision, only : i4k, i8k, r8k
    implicit none

contains

    module procedure interpret_string
        implicit none
        !! interprets a string (typically read from an input file) into a user-defined # of character and/or integer inputs
        integer(i4k) :: i
        character(len=:), allocatable :: string, sep, char
        type :: counter
          integer(i4k) :: t = 0, i = 0, r = 0, c = 0
        end type counter
        type (counter) :: cnt

        if (present(ignore)) then
            string = trim(adjustl(line(index(line,ignore)+len(ignore):)))
        else
            string = trim(adjustl(line))
        end if
        if (present(separator)) then
            allocate(sep, source=separator)
        else
            allocate(sep, source=' ')
        end if
        if (present(ints)) then
            if (allocated(ints)) deallocate(ints)
            allocate(ints(1:size(datatype)),source=0_i4k)
        end if
        if (present(reals)) then
            if (allocated(reals)) deallocate(reals)
            allocate(reals(1:size(datatype)),source=0.0_r8k)
        end if
        if (present(chars)) then
            if (allocated(chars)) deallocate(chars)
            allocate(chars(1:size(datatype)))
        end if

        do i = 1, size(datatype)
            select case (datatype(i))
            case ('i', 'I')
                !! integer
                cnt%i = cnt%i + 1
                call get_string_value (string, sep, ints(cnt%i))
            case ('r', 'R')
                !! real
                cnt%r = cnt%r + 1
                call get_string_value (string, sep, reals(cnt%r))
            case ('c', 'C')
                !! character
                cnt%c = cnt%c + 1
                call get_string_value (string, sep, char)
                allocate(chars(cnt%c)%text, source=char)
            end select
            call reduce_string (string, sep)
            cnt%t = cnt%t + 1
        end do

        line = string

    end procedure interpret_string

    module procedure reduce_string
        implicit none

        if (index(string,sep) == 0) then
            string = ''
        else
            string = adjustl(string(index(string,sep)+len(sep):))
        end if

    end procedure reduce_string

    module procedure get_string_char
        implicit none

        if (index(string,sep) == 0) then
            name = string(1:)                    !! read to end of string
        else
            name = string(1:index(string,sep)-1) !! read until sep is found
        end if

    end procedure get_string_char

    module procedure get_string_int
        implicit none

        character(len=:), allocatable :: text

        if (index(string,sep) == 0) then
            allocate(text, source=string(1:))                    !! read to end of string
        else
            allocate(text, source=string(1:index(string,sep)-1)) !! read until sep is found
        end if
        read(text,'(i8)') name                                   !! store value

    end procedure get_string_int

    module procedure get_string_real
        implicit none
        character(len=:), allocatable :: text

        if (index(string,sep) == 0) then
            allocate(text, source=string(1:))                    !! read to end of string
        else
            allocate(text, source=string(1:index(string,sep)-1)) !! read until sep is found
        end if
        read(text,'(es13.6)') name                               !! store value

    end procedure get_string_real

    module procedure convert_real32_to_string
        implicit none
        !! converts a real to a character string
        character(len=20) :: tmp_string = '                    '

        write(tmp_string,*) var
        allocate(string,source=trim(adjustl(tmp_string)))

    end procedure convert_real32_to_string

    module procedure convert_real64_to_string
        implicit none
        !! converts a real to a character string
        character(len=30) :: tmp_string = '                              '

        write(tmp_string,*) var
        allocate(string,source=trim(adjustl(tmp_string)))

    end procedure convert_real64_to_string

    module procedure convert_real64_array_to_string
        implicit none
        !! converts a real to a character string
        integer(i4k) :: i
        character(len=:), allocatable :: tmp_string

        do i = 1, size(var)
            allocate(tmp_string, source=convert_real64_to_string(var(i)))
            if (.not. allocated(string)) then
                allocate(string,source=tmp_string)
            else
                string = string // " " // tmp_string
            end if
            deallocate(tmp_string)
        end do

    end procedure convert_real64_array_to_string

    module procedure convert_int32_to_string
        implicit none
        character(len=20) :: tmp_string = '                    '

        write(tmp_string,*) var
        allocate(string,source=trim(adjustl(tmp_string)))

    end procedure convert_int32_to_string

    module procedure convert_int64_to_string
        implicit none
        character(len=30) :: tmp_string = '                              '

        write(tmp_string,*) var
        allocate(string,source=trim(adjustl(tmp_string)))

    end procedure convert_int64_to_string

    module procedure convert_logical_to_string
        implicit none

        if (var) then
            allocate(string,source='true')
        else
            allocate(string,source='false')
        end if

    end procedure convert_logical_to_string

    module procedure to_uppercase
        implicit none
        !! author: Ian Porter
        !! date: 01/23/2019
        !!
        !! this function changes lowercase text in a string to uppercase text
        !!
        integer(i4k) :: i, j
        character(len=26), parameter    :: capl = 'abcdefghijklmnopqrstuvwxyz'
        character(len=26), parameter    :: lowl = 'abcdefghijklmnopqrstuvwxyz'

        new_string = string(1:len_trim(string))

        do i = 1, len_trim(string)
            j = index(lowl, string(i:i))
            if (j > 0) then
                new_string(i:i) = capl(j:j)
            else
                new_string(i:i) = string(i:i)
            end if
        end do

    end procedure to_uppercase

    module procedure to_lowercase
        implicit none
        !! author: Ian Porter
        !! date: 01/23/2019
        !!
        !! this function changes uppercase text in a string to lowercase text
        !!
        integer(i4k) :: i, j
        character(len=26), parameter    :: capl = 'abcdefghijklmnopqrstuvwxyz'
        character(len=26), parameter    :: lowl = 'abcdefghijklmnopqrstuvwxyz'

        new_string = string(1:len_trim(string))

        do i = 1, len_trim(string)
            j = index(capl, string(i:i))
            if (j > 0) then
                new_string(i:i) = lowl(j:j)
            else
                new_string(i:i) = string(i:i)
            end if
        end do

    end procedure to_lowercase

    module procedure trim_from_string
        implicit none
        !! author: Ian Porter
        !! date: 11/06/2019
        !!
        !! this function trims <item> from a string
        !!
        integer(i4k) :: start_len  !! length to the start of where to trim string from
        integer(i4k) :: string_len !! length of the string
        integer(i4k) :: item_len   !! length of the item to trim
        logical :: search_by_case  !! flag to determine whether consider case sensitivity in search

        if (present(case_sensitive)) then
            search_by_case = case_sensitive
        else
            search_by_case = .true.
        end if

        if (search_by_case) then
            start_len = index(string,item,back=.true.)
        else
            start_len = index(to_uppercase(string),to_uppercase(item),back=.true.)
        end if

        if (start_len > 0) then
            item_len = len(item)
            string_len = len(string)
            if (string_len == item_len) then
                new_string = ''
            else
                new_string = string(1:start_len - 1)
                if (len(new_string) + item_len < string_len) then
                    new_string = new_string // string(start_len + item_len:)
                end if
            end if
        else
            new_string = string
        end if

    end procedure trim_from_string

    module procedure sleep_for
        implicit none
        !! author: zaak beekman, paratools
        !! date: 8/8/2018
        !!
        !! this performs a 'sleep' for a specified amount of time
        !!
        integer(i4k), dimension(8) :: time
        integer(i8k) :: ms_t1, ms_t2, msecs_big

        call date_and_time(values=time)

        ms_t1=(time(5)*3600+time(6)*60+time(7))*1000+time(8)
        msecs_big = msecs

        do !! spin until elapsed time is greater than msecs
            call date_and_time(values=time)
            ms_t2=(time(5)*3600+time(6)*60+time(7))*1000+time(8)
            if ( ms_t2 - ms_t1 >= msecs_big ) exit
        end do

    end procedure

end submodule misc_procedures
