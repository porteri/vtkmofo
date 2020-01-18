submodule (vtk_piece_element) vtk_piece_element_procedures
    use precision, only : i4k, r8k
    use vtk_formats_types, only : type_float64
    use xml, only : file_format_text
    implicit none
    !! author: Ian Porter
    !! date: 06/07/2019
    !!
    !! this is the basic file piece elements
    !!
    !! data storage formats

contains

    module procedure data_setup
        implicit none
        !! author: Ian Porter
        !! date: 06/06/2019
        !!
        !! this writes the header for a data
        !!
        character(len=*), parameter   :: pointdata_name = 'PointData'
        character(len=*), parameter   :: celldata_name  = 'CellData'
        character(len=:), allocatable :: string
        character(len=:), allocatable :: my_name

        if (allocated(me%scalars)) then
            allocate(string,source=' Scalars="' // me%scalars // '"')
        else if (allocated(me%vectors)) then
            allocate(string,source=' Vectors="' // me%vectors // '"')
        else if (allocated(me%normals)) then
            allocate(string,source=' Normals="' // me%vectors // '"')
        else if (allocated(me%tensors)) then
            allocate(string,source=' Tensors="' // me%vectors // '"')
        else if (allocated(me%tcoords)) then
            allocate(string,source=' TCoords="' // me%vectors // '"')
        else
            allocate(string,source='')
        end if

        select type (me)
        class is (pointdata_dt)
            allocate(my_name,source=pointdata_name)
        class is (celldata_dt)
            allocate(my_name,source=celldata_name)
        class default
            error stop 'Error: undefined type in submodule: vtk_piece_element_procedures, procedure: data_setup'
        end select

        call me%setup(name=my_name, string=string)

    end procedure data_setup

    module procedure data_initialize
        use misc, only : convert_to_string
        implicit none
        !! author: Ian Porter
        !! date: 06/07/2019
        !!
        !! this converts the vtk_element_dt header into xml format
        !!

        if (present(scalar)) then
            allocate(me%scalars,source=scalar)
!        else if (present(vector)) then
!            allocate(me%vectors,source=vector)
!        else
!            error stop 'finish setting this up'
        end if

        call me%data_setup()

    end procedure data_initialize

    module procedure data_add_attribute
        use vtk_dataarray_element, only : dataarray_dt
        implicit none
        !! author: Ian Porter
        !! date: 06/07/2019
        !!
        type(dataarray_dt), dimension(:), allocatable :: tmp_dataarray

        !call me%add(cell%convert_to_dataarray())
#ifdef INTEL_COMPILER
        if (.not. allocated(me%dataarray)) then
            allocate(me%dataarray(1), source=cell%convert_to_dataarray())
        else
            allocate(tmp_dataarray,source=[me%dataarray(:), cell%convert_to_dataarray()])
            call move_alloc(tmp_dataarray, me%dataarray)
        end if
#else
        if (.not. allocated(me%dataarray)) then
            allocate(me%dataarray(1), source=cell%convert_to_dataarray())
        else
            allocate(tmp_dataarray,source=[me%dataarray(:), cell%convert_to_dataarray()])
            call move_alloc(tmp_dataarray, me%dataarray)
        end if
!        error stop 'need to implement data_add_attribute for gcc'
#endif

    end procedure data_add_attribute

    module procedure data_add_attributes
        implicit none
        !! author: Ian Porter
        !! date: 06/07/2019
        integer(i4k) :: i

        do i = 1, size(cell)
            call me%add_cell(cell(i)%attribute)
            !me%dataarray(i) = cell(i)%attribute%convert_to_dataarray()
        end do

    end procedure data_add_attributes

    module procedure data_finalize
        use vtk_vars, only : parallel_container_file
        implicit none
        !! author: Ian Porter
        !! date: 06/07/2019
        !!
        integer :: i

        if (parallel_container_file) then
            call me%update_name('P' // me%get_name())
        end if
        if (allocated(me%dataarray)) then
            do i = 1, size(me%dataarray)
                if (parallel_container_file) then
                    call me%dataarray(i)%update_name('P' // me%dataarray(i)%get_name())
                end if
                call me%add(me%dataarray(i))
            end do
        end if

    end procedure data_finalize

    module procedure data_deallocate
        implicit none
        !! author: Ian Porter
        !! date: 06/07/2019
        !!
        !! explicitly deallocate a data_dt
        !!
        integer :: i

        if (allocated(foo%scalars)) deallocate(foo%scalars)
        if (allocated(foo%vectors)) deallocate(foo%vectors)
        if (allocated(foo%normals)) deallocate(foo%normals)
        if (allocated(foo%tensors)) deallocate(foo%tensors)
        if (allocated(foo%tcoords)) deallocate(foo%tcoords)

        if (allocated(foo%dataarray)) then
            do i = lbound(foo%dataarray,dim=1),ubound(foo%dataarray,dim=1)
                call foo%dataarray(i)%dataarray_deallocate()
            end do
            deallocate(foo%dataarray)
        end if

        call foo%deallocate()

    end procedure data_deallocate

    module procedure points_initialize
        use vtk_datasets, only : struct_grid, unstruct_grid
        use vtk_vars,     only : parallel_container_file
        use misc,         only : convert_to_string
        implicit none
        !! author: Ian Porter
        !! date: 07/09/2019
        !!
        !! initializes a piece dt with the geometry information
        !!
        integer(i4k) :: i
        character(len=*), parameter :: points_name = 'Points'

        if (parallel_container_file) then
            call me%setup(name='P' // points_name)
        else
            call me%setup(name=points_name)
        end if

        select type (geometry)
        class is (struct_grid)
            !! for now, don't allow "pieces" but instead force the piece to be the whole extent
            allocate(me%dataarray(1))
            call me%dataarray(1)%initialize(type=type_float64,format=file_format_text,numberofcomponents=3)
            do i = 1, geometry%n_points
                call me%dataarray(1)%add(geometry%get_point(i)) !! new procedure under works to append an array of reals
            end do
!            call me%add(me%dataarray(1))
!            call me%dataarray(1)%dataarray_deallocate()
!            deallocate(me%dataarray)
        class is (unstruct_grid)
            !! for now, don't allow "pieces" but instead force the piece to be the whole extent
            allocate(me%dataarray(1))
            call me%dataarray(1)%initialize(type=type_float64,format=file_format_text,numberofcomponents=3)
            do i = 1, geometry%n_points
                call me%dataarray(1)%add(geometry%get_point(i)) !! new procedure under works to append an array of reals
            end do
!            call me%add(me%dataarray(1))
!            call me%dataarray(1)%dataarray_deallocate()
!            deallocate(me%dataarray)
        class default
            error stop 'Error: in points_initialize, the geometry is not defined.'
        end select

    end procedure points_initialize

    module procedure cells_initialize
        use vtk_datasets, only : unstruct_grid
        use vtk_vars,     only : parallel_container_file
        implicit none
        !! author: Ian Porter
        !! date: 07/09/2019
        !!
        !! initializes a piece dt with the geometry information
        !!
        integer(i4k) :: i, cnt
        character(len=*), parameter :: my_name = 'Cells'

        if (parallel_container_file) then
            call me%setup(name='P' // my_name)
        else
            call me%setup(name=my_name)
        end if

        select type (geometry)
        class is (unstruct_grid)
            !! set up cell connectivity
            call me%connectivity%initialize(name='connectivity',type=type_float64,format=file_format_text)
            do i = 1, geometry%n_cells
                call me%connectivity%add(geometry%get_connectivity(i)) !! new procedure under works to append an array of reals
            end do
            call me%add(me%connectivity)
            call me%connectivity%dataarray_deallocate()
            !! set up cell offsets
            call me%offsets%initialize(name='offsets',type=type_float64,format=file_format_text)
            cnt = 0
            do i = 1, geometry%n_cells
                cnt = cnt + geometry%get_offset(i)
                call me%offsets%add([cnt]) !! new procedure under works to append an array of reals
            end do
            call me%add(me%offsets)
            call me%offsets%dataarray_deallocate()
            !! set up cell types
            call me%types%initialize(name='types',type=type_float64,format=file_format_text)
            do i = 1, geometry%n_cells
                call me%types%add([geometry%get_type(i)]) !! new procedure under works to append an array of reals
            end do
            call me%add(me%types)
            call me%types%dataarray_deallocate()
        class default
            error stop 'Error: in cells_initialize, the geometry is not yet defined.'
        end select

    end procedure cells_initialize

    module procedure cells_deallocate
        implicit none
        !! author: Ian Porter
        !! date: 06/07/2019
        !!
        !! gcc work-around for deallocating a multi-dimension derived type w/ allocatable character strings
        !!

        call foo%connectivity%dataarray_deallocate()
        call foo%offsets%dataarray_deallocate()
        call foo%types%dataarray_deallocate()

        call foo%deallocate()

    end procedure cells_deallocate

    module procedure coordinates_initialize
        use vtk_datasets, only : dataset, struct_pts, struct_grid, rectlnr_grid, polygonal_data, unstruct_grid
        use misc,         only : convert_to_string
        implicit none
        !! author: Ian Porter
        !! date: 07/09/2019
        !!
        !! initializes a piece dt with the geometry information
        !!
        real(r8k),   dimension(2,3) :: range
        character(len=*), parameter :: coordinate_name = 'Coordinates'

        call me%setup(name=coordinate_name)
        !! todo: figure out why gfortran requires this
        select type (geometry)
        class is (dataset)
            range = geometry%get_range()
        end select
        !! end todo

        select type (geometry)
        class is (rectlnr_grid)
            !! for now, don't allow "pieces" but instead force the piece to be the whole extent
            call me%dataarray_x%initialize(type=type_float64,format=file_format_text,range_min=range(1,1),range_max=range(2,1))
            call me%dataarray_x%add(geometry%get_coord(1))
            call me%dataarray_y%initialize(type=type_float64,format=file_format_text,range_min=range(1,2),range_max=range(2,2))
            call me%dataarray_y%add(geometry%get_coord(2))
            call me%dataarray_z%initialize(type=type_float64,format=file_format_text,range_min=range(1,3),range_max=range(2,3))
            call me%dataarray_z%add(geometry%get_coord(3))

            call me%add(me%dataarray_x)
            call me%add(me%dataarray_y)
            call me%add(me%dataarray_z)
        class default
            error stop 'Error: in coordinates_initialize, the geometry is not yet defined.'
        end select

    end procedure coordinates_initialize

    module procedure coordinates_deallocate
        implicit none
        !! author: Ian Porter
        !! date: 06/07/2019
        !!
        !! gcc work-around for deallocating a multi-dimension derived type w/ allocatable character strings
        !!

        call foo%dataarray_x%dataarray_deallocate()
        call foo%dataarray_y%dataarray_deallocate()
        call foo%dataarray_z%dataarray_deallocate()

        call foo%deallocate()

    end procedure coordinates_deallocate

    module procedure piece_set_grid
        use vtk_datasets, only : dataset, struct_pts, struct_grid, rectlnr_grid, polygonal_data, unstruct_grid
        implicit none
        !! author: Ian Porter
        !! date: 07/09/2019
        !!
        !! initializes a piece dt with the geometry information
        !!
        character(len=10) :: tmp_string = '          '
        character(len=10) :: n_points   = '          '
        character(len=10) :: n_cells    = '          '
        character(len=:), allocatable :: range_string
        integer(i4k) :: i, j
        integer(i4k), dimension(2,3)  :: range

        !! todo: figure out why gfortran requires this
        select type (geometry)
        class is (dataset)
            range = geometry%get_range_cnt()
            !! end todo
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
        class is (unstruct_grid)
            write(n_points,'(i10)') geometry%n_points
            write(n_cells,'(i10)') geometry%n_cells
        end select

        select type (geometry)
        class is (struct_pts)
            call me%setup(name="Piece",string="Extent=" // '"' // range_string // '"')
        class is (struct_grid)
            !! for now, don't allow "pieces" but instead force the piece to be the whole extent
            call me%setup(name="Piece",string="Extent=" // '"' // range_string // '"')
            allocate(me%points)
            call me%points%initialize(geometry)
!            call me%add(me%points)
        class is (rectlnr_grid)
            !! for now, don't allow "pieces" but instead force the piece to be the whole extent
            call me%setup(name="Piece",string="Extent=" // '"' // range_string // '"')
            allocate(me%coordinates)
            call me%coordinates%initialize(geometry)
            call me%add(me%coordinates)
        class is (polygonal_data)
            error stop 'Error: polygonal_data is not yet implemented in piece_set_grid'
        class is (unstruct_grid)
            !! for now, don't allow "pieces" but instead force the piece to be the whole extent
            call me%setup(name="Piece",string="NumberOfPoints=" // '"' // trim(adjustl(n_points)) // '"' // &
                &                             " NumberOfCells=" // '"' // trim(adjustl(n_cells)) // '"')
            allocate(me%points)
            call me%points%initialize(geometry)
!            call me%add(me%points)
            allocate(me%cells)
            call me%cells%initialize(geometry)
            call me%add(me%cells)
        class default
            error stop 'Error: unknown geometry type in piece_set_grid'
        end select

    end procedure piece_set_grid

    module procedure piece_add_data
        implicit none
        !! author: Ian Porter
        !! date: 07/28/2019
        !!
        !! this is a deferred routine for each grid type to implement its own routine to set grid dependent data / info
        !!

        if (present(celldatasets)) then
            if (.not. allocated(me%celldata)) then
                allocate(me%celldata)
                call me%celldata%initialize()
            end if
            call me%celldata%add_cell(celldatasets)
        else if (present(celldata)) then
            if (.not. allocated(me%celldata)) then
                allocate(me%celldata)
                call me%celldata%initialize()
            end if
            call me%celldata%add_cell(celldata)
        end if
        if (present(pointdatasets)) then
            if (.not. allocated(me%pointdata)) then
                allocate(me%pointdata)
                call me%pointdata%initialize()
            end if
            call me%pointdata%add_cell(pointdatasets)
        else if (present(pointdata)) then
            if (.not. allocated(me%pointdata)) then
                allocate(me%pointdata)
                call me%pointdata%initialize()
            end if
            call me%pointdata%add_cell(pointdata)
        end if

    end procedure piece_add_data

    module procedure piece_finalize
        implicit none
        !! author: Ian Porter
        !! date: 06/07/2019

        if (allocated(me%points)) then
            call me%points%finalize()
            call me%add(me%points)
        end if
        if (allocated(me%pointdata)) then
            call me%pointdata%finalize()
            call me%add(me%pointdata)
        end if
        if (allocated(me%celldata)) then
            call me%celldata%finalize()
            call me%add(me%celldata)
        end if

    end procedure piece_finalize

    module procedure piece_deallocate
        implicit none
        !! author: Ian Porter
        !! date: 06/07/2019
        !!
        !! gcc work-around for deallocating a multi-dimension derived type w/ allocatable character strings
        !!

        if (allocated(foo%pointdata))   call foo%pointdata%data_deallocate()
        if (allocated(foo%celldata))    call foo%celldata%data_deallocate()
        if (allocated(foo%coordinates)) call foo%coordinates%coordinates_deallocate()
        if (allocated(foo%points))      call foo%points%data_deallocate()
        if (allocated(foo%cells))       call foo%cells%cells_deallocate()
        if (allocated(foo%source))      deallocate(foo%source)

        call foo%me_deallocate()

    end procedure piece_deallocate

end submodule vtk_piece_element_procedures
