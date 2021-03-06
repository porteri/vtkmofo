submodule (vtk_io) vtk_io_procedures
    implicit none
    !! author: Ian Porter
    !! date: 12/1/2017
    !!
    !! this module implements the ability to read/write vtk formatted files
    !!

    logical,      save :: file_was_already_open     = .false.
    logical,      save :: printed_cell_data_header  = .false.
    logical,      save :: printed_point_data_header = .false.
    integer(i4k), save :: newunit
    character(len=:), allocatable, save :: form

contains

    module procedure vtk_legacy_full_write
        use misc,     only : to_uppercase, trim_from_string
        use vtk_vars, only : default_fn, default_title, vtkfilename, vtktitle, version, fcnt, vtk_extension
        use xml,      only : convert_format_to_string, file_format_text, file_format, ascii, binary, format_ascii, format_binary
        implicit none
        !! author: Ian Porter
        !! date: 12/1/2017
        !!
        !! this subroutine writes the legacy vtk output file
        !!
        integer(i4k) :: i, inputstat
        character(len=11) :: fm

        ! clear out any pre-existing data
        if (allocated(vtkfilename))      deallocate(vtkfilename)
        if (allocated(form))             deallocate(form)
        if (allocated(vtktitle))         deallocate(vtktitle)
        if (allocated(file_format_text)) deallocate(file_format_text)

        if (present(format)) then
            file_format = format                                !! calling program provided what file type to use for vtk file
        else
            file_format = ascii                                 !! default to ascii
        end if
        if (present(filename)) then
            allocate(vtkfilename, source=trim_from_string(filename,vtk_extension) // vtk_extension)
            !! calling program provided a filename
        else
            allocate(vtkfilename, source=default_fn // vtk_extension)
            !! calling program did not provide a filename. use default
        end if
        if (present(multiple_io)) then
            if (multiple_io) then
                mio_filename: block
                    character(len=8) :: fcnt_char = ''          !! file count character
                    character(len=:), allocatable :: base_fn    !! base file name
                    write(fcnt_char,fmt='(i8)') fcnt
                    !allocate(base_fn, source=vtkfilename(1:index(to_uppercase(vtkfilename),to_uppercase(file_extension))-1))
                    allocate(base_fn, source=trim_from_string(vtkfilename,vtk_extension))
                    deallocate(vtkfilename)
                    allocate(vtkfilename, source=base_fn // "_" // trim(adjustl(fcnt_char)) // vtk_extension)
                    fcnt = fcnt + 1                             !! increase timestep file counter by 1
                end block mio_filename
            end if
        end if

        if (present(title)) then
            allocate(vtktitle, source=title)                    !! calling program provided a title
        else
            allocate(vtktitle, source=default_title)            !! calling program did not provide a title. use default
        end if

        if (present(unit)) then
            newunit = unit
            inquire(unit=newunit, opened=file_was_already_open) !! check to see if file is already open
            if (.not. file_was_already_open) then               !! file is not yet open. determine format from file_format
                select case (file_format)
                case (ascii)
                    allocate(form, source='formatted')
                case (binary)
                    allocate(form, source='unformatted')
                case default
                    error stop 'Error: file_format is incorrectly defined in vtk_legacy_full_write'
                end select
                allocate(file_format_text, source=convert_format_to_string(file_format))
                open(unit=newunit, file=vtkfilename, iostat=inputstat, status='replace', form=form)
                !! open the vtk file
            else                                                !! file is already open. determine format based on file format
                inquire(unit=newunit,form=fm)
                select case (to_uppercase(trim(fm)))
                case ('formatted')
                    allocate(file_format_text, source=format_ascii)
                case default
                    allocate(file_format_text, source=format_binary)
                end select
            end if
        else
            !! no unit # provided. make determination by value set for file_format
            select case (file_format)
            case (ascii)
                allocate(form, source='formatted')
            case (binary)
                allocate(form, source='unformatted')
            case default
                write(*,*) 'warning: file_format is incorrectly defined. will default to ascii'
                allocate(form, source='formatted')
            end select
            allocate(file_format_text, source=convert_format_to_string(file_format))
            open(newunit=newunit, file=vtkfilename, iostat=inputstat, status='replace', form=form)
            !! open the vtk file
        end if

        write(newunit,100) version                              !! vtk version (currently, 3.0)
        write(newunit,100) vtktitle                             !! vtk title card
        write(newunit,100) file_format_text                     !! vtk file type

        call geometry%write(newunit)                            !! write the geometry information

        if (present(celldatasets)) then
            write(newunit,101) celldatasets(1)%attribute%nvals
            do i = 1, size(celldatasets)
                call celldatasets(i)%attribute%write(newunit)   !! write the cell data values
            end do
        else if (present(celldata)) then
            write(newunit,101) celldata%nvals
            call celldata%write(newunit)                        !! write the cell data values
        end if

        if (present(pointdatasets)) then
            write(newunit,102) pointdatasets(1)%attribute%nvals
            do i = 1, size(pointdatasets)
                call pointdatasets(i)%attribute%write(newunit)
            end do
        else if (present(pointdata)) then
            write(newunit,102) pointdata%nvals
            call pointdata%write(newunit)                       !! write the point data values
        end if

        if (any([ present(celldatasets), present(celldata), present(pointdatasets), present(pointdata) ])) then
            call vtk_legacy_finalize (finished=.true.)          !! full legacy write w/ data. close file.
        else
            call vtk_legacy_finalize (finished=.false.)         !! no data was provided, only geometry info. do not close file.
        end if

100     format(a)
101     format('CELL_DATA ',i0)
102     format('POINT_DATA ',i0)

    end procedure vtk_legacy_full_write

    module procedure vtk_legacy_append
        implicit none
        !! author: Ian Porter
        !! date: 12/1/2017
        !!
        !! this subroutine writes the legacy vtk output file
        !!
        integer(i4k) :: i, inputstat
        logical :: file_is_still_open

        inquire(unit=newunit,opened=file_is_still_open)
        if (.not. file_is_still_open) then
            !! for some reason, file was closed. re-open the file to a new unit
            open(newunit=newunit, file=vtkfilename, iostat=inputstat, status='replace', form=form, position='append')
        end if

        if (present(celldatasets)) then
            if (.not. printed_cell_data_header) then
                write(newunit,101) celldatasets(1)%attribute%nvals
            end if
            do i = 1, size(celldatasets)
                call celldatasets(i)%attribute%write(newunit)   !! write the cell data values
            end do
        else if (present(celldata)) then
            if (.not. printed_cell_data_header) then
                write(newunit,101) celldata%nvals
                printed_cell_data_header = .true.
            end if
            call celldata%write(newunit)                        !! write the cell data values
        end if

        if (present(pointdatasets)) then
            if (.not. printed_point_data_header) then
                write(newunit,102) pointdatasets(1)%attribute%nvals
            end if
            do i = 1, size(pointdatasets)
                call pointdatasets(i)%attribute%write(newunit)
            end do
        else if (present(pointdata)) then
            if (.not. printed_point_data_header) then
                write(newunit,102) pointdata%nvals
                printed_point_data_header = .true.
            end if

            call pointdata%write(newunit)                       !! write the point data values
        end if

101     format('CELL_DATA ',i0)
102     format('POINT_DATA ',i0)

    end procedure vtk_legacy_append

    module procedure vtk_legacy_finalize
        implicit none
        !! author: Ian Porter
        !! date: 06/03/2019
        !!
        !! this subroutine is a finalizer for the vtk file write
        !!

        if (finished) then
            if (.not. file_was_already_open) then
                close(newunit)                                  !! close the vtk file if file was not open prior to calling vtkmofo
            end if
            printed_cell_data_header  = .false.
            printed_point_data_header = .false.
        end if

    end procedure vtk_legacy_finalize

    module procedure vtk_legacy_read
        use misc,     only : def_len
        use vtk_vars, only : vtkfilename, vtktitle, version
        use xml,      only : convert_string_to_format, ascii, binary, file_format, file_format_text
        implicit none
        !! author: Ian Porter
        !! date: 12/20/2017
        !!
        !! this subroutine reads the legacy vtk output file
        !!
        integer(i4k) :: i, inputstat
        logical      :: file_is_open
        character(len=:), allocatable :: form, vtk_version
        character(len=def_len) :: line

        inquire(unit = unit, opened = file_is_open)        !! check to see if file is already open
        if (.not. file_is_open) then                       !! file is not yet open. determine format to open file
            open(unit=unit, file=filename, iostat=inputstat, status='old')
            !! open the vtk file
        end if

        read(unit,100) line                                !! vtk version (currently, 3.0)
        allocate(vtk_version, source=trim(line))
        line = ''
        if (vtk_version /= version) then
            !! if the file is not version 3.0, abort read
            error stop 'Error: vtkmofo only works with version 3.0'
        end if

        read(unit,100) title                               !! vtk title card
        read(unit,100) line                                !! vtk file type
        allocate(file_format_text, source=trim(line))
        line = ''

        close(unit)                                        !! close the file to re-open it in the proper format

        file_format = convert_string_to_format(file_format_text)
        select case (file_format)
        case (ascii)
            allocate(form, source='formatted')
        case (binary)
            allocate(form, source='unformatted')
        case default
            error stop 'unsupported file type. must be ascii or binary. terminated in vtk_legacy_read'
        end select

        open(unit=unit, file=filename, iostat=inputstat, status='old', form=form)
        !! open the vtk file in the proper format
        read(unit,*)                                       !! skip over this line
        read(unit,*)                                       !! skip over this line
        read(unit,*)                                       !! skip over this line

        call geometry%read(unit)                           !! read the information from the file

        if (present(celldatasets)) then
            read(unit,*)
            do i = 1, size(celldatasets)
                call celldatasets(i)%attribute%read(unit)  !! write the cell data values
            end do
        else if (present(celldata)) then
            read(unit,*)
            call celldata%write(unit)                      !! write the cell data values
        end if
        if (present(pointdatasets)) then
            read(unit,*)
            do i = 1, size(pointdatasets)
                call pointdatasets(i)%attribute%read(unit)
            end do
        else if (present(pointdata)) then
            read(unit,*)
            call pointdata%write(unit)                     !! write the point data values
        end if

        close(unit)                                        !! close the vtk file

        allocate(vtkfilename, source=filename)             !! save the filename for future internal use
        allocate(vtktitle, source=title)                   !! save the title for future internal use

100     format(a)

    end procedure vtk_legacy_read

    module procedure vtk_serial_full_write
        use vtk_datasets,    only : struct_pts, struct_grid, rectlnr_grid, polygonal_data, unstruct_grid
        use vtk_serial_file, only : serial_file
        use vtk_serial_grid, only : vtk_serial_rectilineargrid_dt, vtk_serial_structuredgrid_dt, &
            &                       vtk_serial_unstructuredgrid_dt, vtk_serial_imagedata_dt
        use vtk_vars,        only : vtk_extension
        use xml,             only : file_format, file_format_text, convert_format_to_string, ascii, format_ascii
        use misc,            only : trim_from_string
        implicit none
        !! author: Ian Porter
        !! date: 5/08/2019
        !!
        !! this subroutine writes the modern serial vtk output file
        !!
        integer(i4k) :: file_unit = 0

        ! clear out any pre-existing data
        if (allocated(vtkfilename))      deallocate(vtkfilename)
        if (allocated(form))             deallocate(form)
        if (allocated(file_format_text)) deallocate(file_format_text)

        if (present(format)) then
            allocate(file_format_text, source=convert_format_to_string(format))
            file_format = format
        else
            allocate(file_format_text,source=format_ascii)      !! default to binary
            file_format = ascii
        end if
        if (present(filename)) then                             !! calling program provided a filename
            allocate(vtkfilename, source=trim_from_string(filename,vtk_extension))
            if (present(unit)) then
                file_unit = unit
            else
                inquire(file=vtkfilename, number=file_unit)
                if (file_unit == -1) file_unit = 0
            end if
        else if (present(unit)) then
            !! calling program provided a unit # but no filename
            block
                character(len=132) :: dummy_name
                inquire(unit=unit,name=dummy_name)
                file_unit = unit
                allocate(vtkfilename,source=trim_from_string(trim(adjustl(dummy_name)),vtk_extension))
            end block
        else
            allocate(vtkfilename, source=default_fn)            !! calling program did not provide a filename or unit #. use default
            file_unit = 0
        end if

        if (present(multiple_io)) then
            if (multiple_io) then
                mio_filename: block
                    character(len=8) :: fcnt_char = ''          !! file count character
                    character(len=:), allocatable :: base_fn    !! base file name
                    write(fcnt_char,fmt='(i8)') fcnt
                    allocate(base_fn, source=vtkfilename)
                    deallocate(vtkfilename)
                    allocate(vtkfilename, source=base_fn // "_" // trim(adjustl(fcnt_char)))
                    fcnt = fcnt + 1                             !! increase timestep file counter by 1
                end block mio_filename
            end if
        end if

        allocate(serial_file)

        select type (geometry)
        class is (struct_pts)
            allocate(vtk_serial_imagedata_dt::serial_file%vtk_dataset)
        class is (struct_grid)
            allocate(vtk_serial_structuredgrid_dt::serial_file%vtk_dataset)
        class is (rectlnr_grid)
            allocate(vtk_serial_rectilineargrid_dt::serial_file%vtk_dataset)
        class is (polygonal_data)
            error stop 'procedure not yet implemented for: polygonal grid. termination in subroutine: vtk_serial_full_write'
        class is (unstruct_grid)
            allocate(vtk_serial_unstructuredgrid_dt::serial_file%vtk_dataset)
        class default
            error stop 'unsupported geometry type. termination in subroutine: vtk_serial_full_write'
        end select

        call serial_file%vtk_dataset%set_grid(geometry)

        if (file_unit /= 0) then
            call serial_file%setup(filename=vtkfilename // trim(serial_file%vtk_dataset%file_extension),unit=file_unit)
        else
            call serial_file%setup(filename=vtkfilename // trim(serial_file%vtk_dataset%file_extension))
        end if
        write(0,*) serial_file%filename
        write(0,*) serial_file%unit
        !! append data
        call vtk_serial_append (celldata, pointdata, celldatasets, pointdatasets)
        !! finalize the write
        if (any([ present(celldatasets), present(celldata), present(pointdatasets), present(pointdata) ])) then
            call vtk_serial_finalize (finished=.true.)          !! full legacy write w/ data. close file.
        else
            call vtk_serial_finalize (finished=.false.)         !! no data was provided, only geometry info. do not close file.
        end if

    end procedure vtk_serial_full_write

    module procedure vtk_serial_append
        use vtk_serial_file, only : serial_file
        implicit none
        !! author: Ian Porter
        !! date: 06/24/2019
        !!
        !! this subroutine appends data to the legacy vtk output file
        !!

        if (.not. allocated(serial_file%vtk_dataset%piece)) allocate(serial_file%vtk_dataset%piece)
        call serial_file%vtk_dataset%piece%add_data(celldata, pointdata, celldatasets, pointdatasets)

    end procedure vtk_serial_append

    module procedure vtk_serial_finalize
        use vtk_serial_file, only : serial_file
        implicit none
        !! author: Ian Porter
        !! date: 06/24/2019
        !!
        !! this subroutine is a finalizer for the legacy vtk file write
        !!

        if (finished) then
            call serial_file%vtk_dataset%finalize()
            !! this should write everything inside of the piece
            call serial_file%add(serial_file%vtk_dataset)
            call serial_file%write()
            call serial_file%close_file()                    !! close the vtk file
            call serial_file%me_deallocate()
            if (allocated(serial_file)) deallocate(serial_file)
        end if

    end procedure vtk_serial_finalize

end submodule vtk_io_procedures
