submodule (vtk_element) vtk_element_procedures
    implicit none
    !! author: Ian Porter
    !! date: 05/06/2019
    !!
    !! this is the basic file for a serial vtk file
    !!

contains

    module procedure vtk_element_setup
        use file_utility, only : is_little_endian
        use VTK_vars,     only : parallel_container_file
        implicit none
        !! author: Ian Porter
        !! date: 05/06/2019
        !!
        !! this writes the header for a vtk file
        !!
        !! example:
        !! <VTKFile type=”ImageData” version=”0.1” byte_order=”LittleEndian”>
        character(len=*), parameter   :: name = 'VTKFile'
        character(len=:), allocatable :: string
        character(len=:), allocatable :: type_string
        character(len=*), parameter   :: version_string = ' version="' // def_version // '"'
        character(len=:), allocatable :: byte_order_string
        character(len=:), allocatable :: compression_string

        if (allocated(me%type)) then
            if (parallel_container_file) then
                allocate(type_string,source=' type="P' // me%type // '"')
            else
                allocate(type_string,source=' type="' // me%type // '"')
            end if
        else
            error stop "error. can't create vtk file without a known type. terminated in vtk_element_setup"
        end if
        if (is_little_endian()) then
            allocate(byte_order_string,source=' byte_order="LittleEndian"')
        else
            allocate(byte_order_string,source=' byte_order="BigEndian"')
        end if
        if (allocated(me%compression)) then
            allocate(compression_string,source=' compression="' // me%compression // '"')
        else
            allocate(compression_string,source='')
        end if

        allocate(string, source=type_string // version_string // byte_order_string // compression_string)

        call me%setup(name=name,string=string,offset=4)

    end procedure vtk_element_setup

    module procedure initialize
        use vtk_vars, only : parallel_container_file
        implicit none
        !! author: Ian Porter
        !! date: 05/07/2019
        !!
        !! this is an external interface to allow the user or other routines to set the internal variables
        !!

        if (present(type))           allocate(me%type,source=type)
        if (present(compression))    allocate(me%compression,source=compression)
        if (present(file_extension)) then
            if (parallel_container_file) then
                allocate(me%file_extension,source='p' // file_extension)
            else
                allocate(me%file_extension,source=file_extension)
            end if
        end if

        call me%vtk_element_setup()

    end procedure initialize

    module procedure finalize
        implicit none
        !! author: Ian Porter
        !! date: 05/07/2019
        !!
        !!
        !!
        if (allocated(me%vtk_element)) then
            call me%vtk_element%finalize()
            call me%add(me%vtk_element)
        end if

    end procedure finalize

    module procedure gcc_bug_workaround_deallocate_vtk_element_single
        use iso_fortran_env, only : output_unit
        implicit none
        !! gcc work-around for deallocating a multi-dimension derived type w/ allocatable character strings

        if (allocated(foo%type))           deallocate(foo%type)
        if (allocated(foo%version))        deallocate(foo%version)
        if (allocated(foo%compression))    deallocate(foo%compression)
        if (allocated(foo%file_extension)) deallocate(foo%file_extension)
        if (allocated(foo%filename))       deallocate(foo%filename)
        !call foo%piece%deallocate_piece_dt()
        if (allocated(foo%vtk_element)) then
            call foo%vtk_element%me_deallocate()
            !call foo%vtk_element%deallocate()
            deallocate(foo%vtk_element)
        end if
        write(output_unit,*) 'gcc_bug_workaround_deallocate_vtk_element_single 8'
        !call foo%deallocate()
        write(output_unit,*) 'gcc_bug_workaround_deallocate_vtk_element_single 9'
    end procedure gcc_bug_workaround_deallocate_vtk_element_single

end submodule vtk_element_procedures
