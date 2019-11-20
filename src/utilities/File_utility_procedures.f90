submodule (file_utility) file_utility_procedures
    use precision, only : i1k
    implicit none
    !! author: Ian Porter
    !! date: 04/04/2018
    !!
    !! this module contains a derived type for file information
    !!
contains

    module procedure setup_file_information
        implicit none
        !! author: Ian Porter
        !! date: 04/16/2018
        !!
        !! establishes the file information
        !!
        character(len=:), allocatable :: tmp_filename

        allocate(tmp_filename,source=filename)
        if (allocated(me%filename)) deallocate(me%filename)
        allocate(me%filename, source=tmp_filename)
        if (present(open_status)) then
            if (allocated(me%open_status)) deallocate(me%open_status)
            allocate(me%open_status, source=open_status)
        else
            if (.not. allocated(me%open_status)) allocate(me%open_status, source='unknown')
        end if
        if (present(close_status)) then
            if (allocated(me%close_status)) deallocate(me%close_status)
            allocate(me%close_status, source=close_status)
        else
            if (.not. allocated(me%close_status)) allocate(me%close_status, source='keep')
        end if
        if (present(form)) then
            if (allocated(me%form)) deallocate(me%form)
            allocate(me%form, source=form)
        else
            if (.not. allocated(me%form)) allocate(me%form, source='unknown')
        end if
        if (present(access)) then
            if (allocated(me%access)) deallocate(me%access)
            allocate(me%access, source=access)
        else
            if (.not. allocated(me%access)) allocate(me%access, source='unknown')
        end if
        if (present(unit)) then
            me%unit = unit
        else
            me%unit = 0
        end if
        if (present(encoding)) then
            if (allocated(me%encoding)) deallocate(me%encoding)
            allocate(me%encoding, source=encoding)
        else
            if (.not. allocated(me%encoding)) allocate(me%encoding, source='default')
        end if

    end procedure setup_file_information

    module procedure check_if_exists
        use misc, only : to_uppercase
        implicit none
        !! author: Ian Porter
        !! date: 04/04/2018
        !!
        !! checks to see if the file is open
        !!
        character(len=:), allocatable :: check_by_type

        if (present(check)) then
            allocate(check_by_type, source=to_uppercase(check))
        else
            allocate(check_by_type, source='filename')
        end if

        select case (check_by_type)
        case ('filename')
            inquire (file=me%filename, exist=file_exists)
        case ('unit')
            inquire (unit=me%unit, exist=file_exists)
        case default
            error stop 'unrecognized check_by_type in module procedure check_if_exists'
        end select

    end procedure check_if_exists

    module procedure check_if_open
        use misc, only : to_uppercase
        implicit none
        !! author: Ian Porter
        !! date: 04/04/2018
        !!
        !! checks to see if the file is open by file name
        !!
        character(len=:), allocatable :: check_by_type

        if (present(check)) then
            allocate(check_by_type, source=to_uppercase(check))
        else
            allocate(check_by_type, source='filename')
        end if

        select case (check_by_type)
        case ('filename')
            inquire (file=me%filename, opened=is_open)
        case ('unit')
            inquire (unit=me%unit, opened=is_open)
        case default
            error stop 'unrecognized check_by_type in module procedure check_if_open'
        end select

    end procedure check_if_open

    module procedure open_file
        implicit none
        !! author: Ian Porter
        !! date: 04/04/2018
        !!
        !! opens the file if it already exists
        !!
        integer(i4k) :: inputstat

        if (.not. me%exists()) then
            error stop 'file does not exist. execution terminated in module: file_utility, subroutine: open_file'
        end if

        if (.not. me%check_if_open(check='filename')) then
            open (newunit=me%unit, file=me%filename, iostat=inputstat, status=me%open_status, form=me%form, &
                & encoding=me%encoding)
            if (inputstat > 0) call me%file_read_error (inputstat)
        else
            !! file is already open. rewind to start from the beginning
            rewind (me%unit)
        end if

    end procedure open_file

    module procedure close_file
        implicit none
        !! author: Ian Porter
        !! date: 10/25/2018
        !!
        !! closes the file
        !!

        if (me%check_if_open()) then
            !! close the file
            close(unit=me%unit,status=me%close_status)
        end if

    end procedure close_file

    module procedure make_file
        implicit none
        !! author: Ian Porter
        !! date: 10/26/2018
        !!
        !! makes a new file
        !!
        integer(i4k) :: inputstat
        logical :: make_it
        character(len=132) :: file_name

        make_it = .true.
        if (me%unit /= 0) then
            if (me%exists('unit')) then
                !! the number is an allowable number. keep checking.
                if (me%check_if_open('unit')) then
                    !! the file is open. check to make sure the name is valid.
                    inquire(unit=me%unit,name=file_name)
                    if (file_name == me%filename) then
                        make_it = .false.
                    else
                        !! the file open has the wrong filename. close it.
                        call me%close_file()
                    end if
                end if
            else if (me%unit < 0) then
                me%unit = 0 !! re-set this to a non-negative number for a gfortran-8.3 bug w/ newunit
            end if
        end if

        if (make_it) then
            open (newunit=me%unit, file=me%filename, iostat=inputstat, status='replace', form=me%form, access=me%access)
        end if

    end procedure make_file

    module procedure file_read_error
        use iso_fortran_env, only : output_unit
        implicit none
        !! author: Ian Porter
        !! date: 04/04/2018
        !!
        !! prints that a file i/o error as occurred
        !!
        write(output_unit,100) me%unit, inputstat, me%filename, me%open_status, me%form, me%access
        error stop 'error in opening file. execution terminated in subroutine: iofiles.'

        100     format(/' iofiles: error on trying to open unit ',i0,/, &
        &     '  error message number = ',i0              ,/, &
        &     '  file name = ',a                           ,/, &
        &     '  status    = ',a                           ,/, &
        &     '  form      = ',a                           ,/, &
        &     '  access    = ',a)

    end procedure file_read_error

    module procedure wait_for_file
        use misc, only : sleep_for
        implicit none
        !! author: Ian Porter
        !! date: 08/10/2018
        !!
        !! waits for a file to exist
        !!

        check_if_exists: do
            if (me%exists()) then
                !! file exists. now check to see if it is open
                if (me%check_if_open()) then
                    !! it is still being written-to by another file
                    !                    call sleep_for(10)
                else
                    call me%open_file()
                    !                    exit check_if_exists
                end if
                exit check_if_exists
            end if
            call sleep_for(10) !! code waits 10 milliseconds before checking again
        end do check_if_exists

    end procedure wait_for_file

    module procedure get_unit
        implicit none
        !! author: Ian Porter
        !! date: 04/03/2019
        !!
        !! function returns the unit of a file
        !!

        unit = me%unit

    end procedure get_unit

    module procedure is_little_endian
        implicit none
        !! author: Ian Porter
        !! date: 04/03/2019
        !!
        !! checks the type of bit ordering to determine if the running architecture is little endian.
        !!
        integer(i1k) :: int1(1:4) !! one byte integer array for casting 4 bytes integer.

        int1 = transfer(1_i4k, int1)
        is_little = (int1(1) == 1_i1k)

    end procedure is_little_endian

end submodule file_utility_procedures
