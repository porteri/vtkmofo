submodule (vtk_xml_grid) vtk_xml_grid_procedures
    use precision, only : i4k
    !! author: Ian Porter
    !! date: 05/06/2019
    !!
    !! this submodule implements the procedures for a serial rectilinear grid
    !!

contains

    module procedure parallel_fix
        implicit none
        !! author: Ian Porter
        !! date: 01/11/2020
        !!
        !! Performs a parallel copy from a serial_file
        !! by bringing the "piece" element up one level
        !!
        character(len=:), allocatable :: my_grid_type

        call me%vtk_element_setup()
!        call me%clear_elements()
        my_grid_type = 'P' // me%grid_type
        deallocate(me%grid_type)
        me%grid_type = my_grid_type
write(0,*) 'parallel_fix 1'
        if (allocated(me%piece)) then
            call me%piece%clear_data()
            call me%piece%clear_elements()
            if (allocated(me%piece%pointdata)) then
                call me%piece%pointdata%clear_elements()
                me%pointdata = me%piece%pointdata
                call me%piece%pointdata%data_deallocate()
                deallocate(me%piece%pointdata)
            end if
write(0,*) 'parallel_fix 2'
            if (allocated(me%piece%celldata)) then
                call me%piece%celldata%clear_elements()
                me%celldata = me%piece%celldata
                call me%piece%celldata%data_deallocate()
                deallocate(me%piece%celldata)
            end if
            if (allocated(me%piece%coordinates)) then
                call me%piece%coordinates%clear_elements()
                me%coordinates = me%piece%coordinates
                call me%coordinates%update_names('P' // me%coordinates%get_name())
                call me%piece%coordinates%coordinates_deallocate()
                deallocate(me%piece%coordinates)
            end if
            if (allocated(me%piece%points)) then
                call me%piece%points%clear_elements()
                me%points = me%piece%points
                !call me%points%update_names('P' // me%points%get_name())
                call me%piece%points%data_deallocate()
                deallocate(me%piece%points)
            end if
            if (allocated(me%piece%cells)) then
                me%cells = me%piece%cells
                call me%cells%update_names('P' // me%cells%get_name())
                call me%piece%cells%cells_deallocate()
                deallocate(me%piece%cells)
            end if
            if (present(pieces)) then
                allocate(me%parallel_pieces(1:size(pieces)))
                block
                    integer :: i
                    do i = 1, size(pieces)
                        write(0,*) 'in parallel_pieces(i)%setup. name: ',me%piece%get_name()
                        write(0,*) 'in parallel_pieces(i)%setup. string: ',pieces(i)%get_header() &
                            &                                    // ' Source="' // pieces(i)%source // '"'
                        call me%parallel_pieces(i)%setup(name=me%piece%get_name(),string=pieces(i)%get_header() &
                            &                                    // ' Source="' // pieces(i)%source // '"')
                    end do
                end block
            end if
write(0,*) 'parallel_fix 6'
            call me%piece%piece_deallocate()
            deallocate(me%piece)
        end if
write(0,*) 'in parallel_fix. before finalize'
        call me%finalize()
write(0,*) 'in parallel_fix. after finalize, before clear_data'
        call me%clear_data()             !! Clear actual stored data
write(0,*) 'in parallel_fix. after clear_data'
    end procedure parallel_fix

    module procedure finalize
        use xml,      only : xml_element_dt
        use vtk_vars, only : parallel_container_file
        implicit none
        !! author: Ian Porter
        !! date: 05/06/2019
        !!
        !! writes data inside of itself
        !!
        integer :: i
        type(vtk_element_dt) :: grid
write(0,*) 'in vtk_xml_grid_procedures: finalize'
        if (allocated(me%wholeextent)) then
            if (parallel_container_file) then
                call grid%setup(name=me%grid_type,string= "WholeExtent=" // '"' // me%wholeextent // '" GhostLevel="0"')
            else
                call grid%setup(name=me%grid_type,string= "WholeExtent=" // '"' // me%wholeextent // '"')
            end if
        else
            if (parallel_container_file) then
                call grid%setup(name=me%grid_type,string= 'GhostLevel="0"')
            else
                call grid%setup(name=me%grid_type)
            end if
        end if
        if (allocated(me%extra_string)) call grid%add(me%extra_string, quotes=.false.)
write(0,*) 'before if allocated(me%piece)'
        if (allocated(me%piece)) then
            call me%piece%finalize()
            call grid%add(me%piece)
        end if
        if (allocated(me%parallel_pieces)) then
write(0,*) '2'
            do i = 1, size(me%parallel_pieces)
                call me%parallel_pieces(i)%finalize()
                call grid%add(me%parallel_pieces(i))
            end do
        endif
write(0,*) '5'
        if (allocated(me%points)) then
write(0,*) '5-1'
            call me%points%finalize()
write(0,*) '5-2'
            call grid%add(me%points)
write(0,*) '5-3'
        end if
write(0,*) '3'
        if (allocated(me%pointdata)) then
write(0,*) '3-1'
            call me%pointdata%finalize()
write(0,*) '3-2'
            call grid%add(me%pointdata)
write(0,*) '3-3'
        end if
write(0,*) '4'
        if (allocated(me%celldata)) then
write(0,*) '4-1'
            call me%celldata%finalize()
write(0,*) '4-2'
            call grid%add(me%celldata)
write(0,*) '4-3'
        end if
write(0,*) '6'
        call me%add(grid)
write(0,*) '7'
        call grid%me_deallocate()

write(0,*) '8 - end of finalize'
    end procedure finalize

    module procedure vtk_dataset_deallocate
        use iso_fortran_env, only : output_unit
        implicit none
        !! author: Ian Porter
        !! date: 05/06/2019
        !!
        !! gcc work-around for deallocating a multi-dimension derived type w/ allocatable character strings
        !!
        integer :: i

        if (allocated(foo%wholeextent))  deallocate(foo%WholeExtent)
        if (allocated(foo%grid_type))    deallocate(foo%grid_type)
        if (allocated(foo%extra_string)) deallocate(foo%extra_string)
        if (allocated(foo%piece)) then
            call foo%piece%piece_deallocate()
            deallocate(foo%piece)
        end if
write(output_unit,*) 'vtk_dataset_deallocate 5'
        if (allocated(foo%parallel_pieces)) then
            do i = lbound(foo%parallel_pieces,dim=1), ubound(foo%parallel_pieces,dim=1)
                call foo%parallel_pieces(i)%piece_deallocate()
            end do
            deallocate(foo%parallel_pieces)
        end if
write(output_unit,*) 'vtk_dataset_deallocate 6'
        call foo%piece_deallocate()
write(output_unit,*) 'vtk_dataset_deallocate 7'
    end procedure vtk_dataset_deallocate

    module procedure imagedata_set_grid
        use vtk_datasets, only : struct_pts
        use misc,         only : convert_to_string
        implicit none
        !! author: Ian Porter
        !! date: 08/08/2019
        !!
        !! this writes the grid information for an image data grid
        !!
        character(len=10) :: tmp_string = '          '
        character(len=:), allocatable :: range_string, origin_string, spacing_string
        integer(i4k) :: i, j
        integer(i4k), dimension(2,3)  :: range
        character(len=*), parameter :: file_extension = 'vti'
        character(len=*), parameter :: grid_type = 'ImageData'

        call me%initialize(type=grid_type,file_extension=file_extension)

        range = geometry%get_range_cnt()

        do i = 1, 3
            do j = 1, 2
                write(tmp_string,'(i10)') range(j,i)
                if (.not. allocated(range_string)) then
                    allocate(range_string,source=trim(adjustl(tmp_string)))
                else
                    range_string = range_string // ' ' // trim(adjustl(tmp_string))
                end if
            end do
        end do

        allocate(me%wholeextent, source=range_string)
        allocate(me%grid_type, source=grid_type)

        !! still need to set the following line of information:
        !! origin=”x0 y0 z0” spacing=”dx dy dz”>
        select type(geometry)
        class is (struct_pts)
            allocate(origin_string, source=convert_to_string(geometry%get_origin()))
            allocate(spacing_string, source=convert_to_string(geometry%get_spacing()))
            allocate(me%extra_string, source='Origin="' // origin_string // '", Spacing="' // spacing_string // '"')
        class default
            error stop 'Error: bad geometry type for imagedata. terminated in imagedata_set_grid'
        end select

        !        error stop 'imagedata_set_grid is not yet implemented. need to set origin, spacing'

        !! for now, don't allow "pieces" but instead force the piece to be the whole extent
        if (.not. allocated(me%piece)) allocate(me%piece)
        call me%piece%set(geometry)

    end procedure imagedata_set_grid

    module procedure rectilineargrid_set_grid
        implicit none
        !! author: Ian Porter
        !! date: 05/06/2019
        !!
        !! this sets parameters specific to the dt
        !!
        character(len=10) :: tmp_string = '          '
        character(len=:), allocatable :: range_string
        integer(i4k) :: i, j
        integer(i4k), dimension(2,3)  :: range
        character(len=*), parameter :: file_extension = 'vtr'
        character(len=*), parameter :: grid_type = 'RectilinearGrid'

        call me%initialize(type=grid_type,file_extension=file_extension)
        range = geometry%get_range_cnt()

        do i = 1, 3
            do j = 1, 2
                write(tmp_string,'(i10)') range(j,i)
                if (.not. allocated(range_string)) then
                    allocate(range_string,source=trim(adjustl(tmp_string)))
                else
                    range_string = range_string // ' ' // trim(adjustl(tmp_string))
                end if
            end do
        end do

        allocate(me%wholeextent, source=range_string)
        allocate(me%grid_type, source=grid_type)

        !! for now, don't allow "pieces" but instead force the piece to be the whole extent
        if (.not. allocated(me%piece)) allocate(me%piece)
        call me%piece%set(geometry)

    end procedure rectilineargrid_set_grid

    module procedure structuredgrid_set_grid
        implicit none
        !! author: Ian Porter
        !! date: 05/06/2019
        !!
        !! this sets parameters specific to the dt
        !!
        character(len=10) :: tmp_string = '          '
        character(len=:), allocatable :: range_string
        integer(i4k) :: i, j
        integer(i4k), dimension(2,3)  :: range
        character(len=*), parameter :: file_extension = 'vts'
        character(len=*), parameter :: grid_type = 'StructuredGrid'

        call me%initialize(type=grid_type,file_extension=file_extension)
        range = geometry%get_range_cnt()

        do i = 1, 3
            do j = 1, 2
                write(tmp_string,'(i10)') range(j,i)
                if (.not. allocated(range_string)) then
                    allocate(range_string,source=trim(adjustl(tmp_string)))
                else
                    range_string = range_string // ' ' // trim(adjustl(tmp_string))
                end if
            end do
        end do

        allocate(me%wholeextent, source=range_string)
        allocate(me%grid_type, source=grid_type)

        !! for now, don't allow "pieces" but instead force the piece to be the whole extent
        if (.not. allocated(me%piece)) allocate(me%piece)
        call me%piece%set(geometry)

    end procedure structuredgrid_set_grid

    module procedure unstructuredgrid_set_grid
        implicit none
        !! author: Ian Porter
        !! date: 05/06/2019
        !!
        !! this sets parameters specific to the dt
        !!
        character(len=*), parameter :: file_extension = 'vtu'
        character(len=*), parameter :: grid_type = 'UnstructuredGrid'

        call me%initialize(type=grid_type,file_extension=file_extension)

        allocate(me%grid_type, source=grid_type)

        !! for now, don't allow "pieces" but instead force the piece to be the whole extent
        if (.not. allocated(me%piece)) allocate(me%piece)
        call me%piece%set(geometry)

    end procedure unstructuredgrid_set_grid

end submodule vtk_xml_grid_procedures
