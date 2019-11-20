submodule (vtk_serial_grid) vtk_serial_grid_procedures
    use precision, only : i4k
    !! author: Ian Porter
    !! date: 05/06/2019
    !!
    !! this submodule implements the procedures for a serial rectilinear grid
    !!

contains

    module procedure finalize
        use xml, only : xml_element_dt
        implicit none
        !! author: Ian Porter
        !! date: 05/06/2019
        !!
        !! writes data inside of itself
        !!
        type(vtk_element_dt) :: grid

        if (allocated(me%piece)) then
            call me%piece%finalize()
            if (allocated(me%wholeextent)) then
                call grid%setup(name=me%grid_type,string= "wholeextent=" // '"' // me%wholeextent // '"')
            else
                call grid%setup(name=me%grid_type)
            end if
            if (allocated(me%extra_string)) call grid%add(me%extra_string, quotes=.false.)
            call grid%add(me%piece)
            call me%add(grid)
            call grid%me_deallocate()
        end if

    end procedure finalize

    module procedure vtk_dataset_deallocate
        implicit none
        !! author: Ian Porter
        !! date: 05/06/2019
        !!
        !! gcc work-around for deallocating a multi-dimension derived type w/ allocatable character strings
        !!

        if (allocated(foo%piece)) call foo%piece%piece_deallocate()

        call foo%me_deallocate()

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
        character(len=*), parameter :: file_extension = ".vti"
        character(len=*), parameter :: grid_type = "imagedata"

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
            allocate(me%extra_string, source='ORIGIN="' // origin_string // '", SPACING="' // spacing_string // '"')
        class default
            error stop 'bad geometry type for imagedata. terminated in imagedata_set_grid'
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
        character(len=*), parameter :: file_extension = '.vtr'
        character(len=*), parameter :: grid_type = 'RECTILINEARGRID'

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
        character(len=*), parameter :: file_extension = '.vts'
        character(len=*), parameter :: grid_type = 'STRUCTUREDGRID'

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
        character(len=*), parameter :: file_extension = '.vtu'
        character(len=*), parameter :: grid_type = 'UNSTRUCTUREDGRID'

        call me%initialize(type=grid_type,file_extension=file_extension)

        allocate(me%grid_type, source=grid_type)

        !! for now, don't allow "pieces" but instead force the piece to be the whole extent
        if (.not. allocated(me%piece)) allocate(me%piece)
        call me%piece%set(geometry)

    end procedure unstructuredgrid_set_grid

end submodule vtk_serial_grid_procedures
