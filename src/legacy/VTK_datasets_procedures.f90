submodule (vtk_datasets) vtk_datasets_procedures
    implicit none
    !! author: Ian Porter
    !! date: 12/1/2017
    !!
    !! this module contains the dataset formats for vtk format
    !!
    !! the following dataset formats are available:
    !! 1) structured points
    !! 2) structured grid
    !! 3) rectilinear grid
    !! 4) polygonal data
    !! 5) unstructured grid
    !!
contains
    !! ****************
    !! abstract dataset
    !! ****************

    module procedure init
        use misc, only : to_lowercase
        implicit none
        !! initializes the dataset with information
        character(len=:), allocatable :: data_type  !! internal variable for datatype

        if (present(datatype)) then
            allocate(data_type, source=datatype)
        else
            allocate(data_type, source='double')
        end if

        select type (me)
        class is (struct_pts)
            call me%setup(dims, origin, spacing)
        class is (struct_grid)
            call me%setup(dims, points)
        class is (rectlnr_grid)
            call me%setup(dims, x_coords, y_coords, z_coords, data_type)
        class is (polygonal_data)
            call me%setup(points, vertices, lines, polygons, triangles)
        class is (unstruct_grid)
            if (present(cell_list)) then
                call me%setup(points, cell_list)
            else
                call me%setup(points, cells)
            end if
        class default
            error stop 'generic class not defined for vtkmofo class dataset'
        end select

        me%datatype = to_lowercase(data_type)

    end procedure init

    module procedure check_for_diffs
        implicit none
        !! author: Ian Porter
        !! date: 12/18/2017
        !!
        !! function checks for differences in a dataset
        !!
        diffs = .false.
        if      (.not. same_type_as(me,you))  then
            diffs = .true.
        else if (me%name          /= you%name) then
            diffs = .true.
        else if (me%dimensions(1) /= you%dimensions(1) .or. &
            &      me%dimensions(2) /= you%dimensions(2) .or. &
            &      me%dimensions(3) /= you%dimensions(3)) then
            diffs = .true.
        end if

    end procedure check_for_diffs

    module procedure get_range_cnt
        implicit none
        !! function returns the number of variables in x,y,z coordinates

        range(1,1) = 1
        range(2,1) = max(me%dimensions(1),1)
        range(1,2) = 1
        range(2,2) = max(me%dimensions(2),1)
        range(1,3) = 1
        range(2,3) = max(me%dimensions(3),1)

    end procedure get_range_cnt

    module procedure get_range
        implicit none
        !! function returns the min / max range of values in x,y,z coordinates

        error stop 'generic get_range should not be called. will be moved to abstract.'

    end procedure get_range

    module procedure get_coord
        implicit none
        !! function returns the min / max range of values in x,y,z coordinates

        error stop 'generic get_coord should not be called. will be moved to abstract.'

    end procedure get_coord
    ! *****************
    ! structured points
    ! *****************
    module procedure struct_pts_read
        use misc, only : interpret_string, def_len, char_dt
        implicit none
        !! reads the structured points dataset information from the .vtk file
        integer(i4k)                   :: iostat
        character(len=def_len)         :: line
        integer(i4k),  dimension(:), allocatable :: ints
        real(r8k),     dimension(:), allocatable :: reals
        type(char_dt), dimension(:), allocatable :: chars

        read(unit,100,iostat=iostat) line
        call interpret_string (line=line, datatype=[ 'c' ],         ignore='dataset ',    separator=' ', chars=chars)
        me%name = trim(chars(1)%text)

        read(unit,100,iostat=iostat) line
        call interpret_string (line=line, datatype=[ 'i','i','i' ], ignore='dimensions ', separator=' ', ints=ints)
        me%dimensions = ints(1:3)

        read(unit,100,iostat=iostat) line
        call interpret_string (line=line, datatype=[ 'r','r','r' ], ignore='origin ',     separator=' ', reals=reals)
        me%origin = reals(1:3)

        read(unit,100,iostat=iostat) line
        call interpret_string (line=line, datatype=[ 'r','r','r' ], ignore='spacing ',    separator=' ', reals=reals)
        me%spacing = reals(1:3)

100     format((a))
    end procedure struct_pts_read

    module procedure struct_pts_write
        implicit none
        !! writes the structured points dataset information to the .vtk file

        write(unit,100) me%name
        write(unit,101) me%dimensions
        write(unit,102) me%origin
        write(unit,103) me%spacing

100     format('dataset ',(a))
101     format('dimensions ',*(i0,' '))
102     format('origin ',*(es13.6))
103     format('spacing ',*(es13.6))

    end procedure struct_pts_write

    module procedure struct_pts_get_origin
        implicit none
        !! gets the private dt data for origin

        origin = me%origin

    end procedure struct_pts_get_origin

    module procedure struct_pts_get_spacing
        implicit none
        !! gets the private dt data for spacing

        spacing = me%spacing

    end procedure struct_pts_get_spacing

    module procedure struct_pts_setup
        implicit none
        !! sets up the structured points dataset with information

        me%name       = 'structured_points'
        me%dimensions = dims
        me%origin     = origin
        me%spacing    = spacing
        me%firstcall  = .false.

    end procedure struct_pts_setup

    module procedure struct_pts_check_for_diffs
        implicit none
        !! function checks for differences in a structured points dataset

        diffs = .false.
        if      (.not. same_type_as(me,you))  then
            diffs = .true.
        else if (me%name          /= you%name) then
            diffs = .true.
        else if (me%dimensions(1) /= you%dimensions(1) .or. &
            &    me%dimensions(2) /= you%dimensions(2) .or. &
            &    me%dimensions(3) /= you%dimensions(3)) then
            diffs = .true.
        else
            select type (you)
            class is (struct_pts)
                if      (me%origin(1)  /= you%origin(1)  .or. &
                    &    me%origin(2)  /= you%origin(2)  .or. &
                    &    me%origin(3)  /= you%origin(3))  then
                    diffs = .true.
                else if (me%spacing(1) /= you%spacing(1) .or. &
                    &      me%spacing(2) /= you%spacing(2) .or. &
                    &      me%spacing(3) /= you%spacing(3)) then
                    diffs = .true.
                end if
            end select
        end if

    end procedure struct_pts_check_for_diffs

    module procedure struct_pts_get_range
        implicit none
        !! function returns the min / max range of values in x,y,z coordinates

    end procedure struct_pts_get_range
    ! ***************
    ! structured grid
    ! ***************
    module procedure struct_grid_read
        use misc, only : interpret_string, def_len, char_dt
        implicit none
        !! reads the structured grid dataset information from the .vtk file
        integer(i4k)            :: i, iostat
        integer(i4k), parameter :: dim = 3
        logical                 :: end_of_file
        character(len=def_len)  :: line
        integer(i4k),  dimension(:), allocatable :: ints
        real(r8k),     dimension(:), allocatable :: reals
        type(char_dt), dimension(:), allocatable :: chars

        read(unit,100,iostat=iostat) line
        call interpret_string (line=line, datatype=[ 'c' ],         ignore='dataset ',    separator=' ', chars=chars)
        me%name = trim(chars(1)%text)
        if (allocated(chars)) deallocate(chars)

        read(unit,100,iostat=iostat) line
        call interpret_string (line=line, datatype=[ 'i','i','i' ], ignore='dimensions ', separator=' ', ints=ints)
        me%dimensions = ints(1:3)

        read(unit,100,iostat=iostat) line
        call interpret_string (line=line, datatype=[ 'i','c' ],     ignore='points ',     separator=' ', ints=ints, chars=chars)
        me%n_points= ints(1); me%datatype = trim(chars(1)%text)

        allocate(me%points(1:dim,1:me%n_points)); end_of_file  = .false.

        get_points: do i = 1, me%n_points
            read(unit,100,iostat=iostat) line
            end_of_file = (iostat < 0)
            if (end_of_file) then
                exit get_points
            else if (trim(line) == '') then
                cycle     !! skip blank lines
            else
                call interpret_string (line=line, datatype=[ 'r','r','r' ], separator=' ', reals=reals)
                me%points(1:dim,i) = reals(1:dim)
            end if
        end do get_points

100     format((a))
    end procedure struct_grid_read

    module procedure struct_grid_write
        implicit none
        !! writes the structured grid dataset information to the .vtk file
        integer(i4k) :: i

        write(unit,100) me%name
        write(unit,101) me%dimensions
        write(unit,102) me%n_points, me%datatype
        do i = 1, me%n_points
            write(unit,103) me%points(1:3,i)
        end do

100     format('dataset ',(a))
101     format('dimensions ',*(i0,' '))
102     format('points ',(i0),' ',(a))
103     format(*(es13.6))

    end procedure struct_grid_write

    module procedure struct_grid_setup
        implicit none
        !! sets up the structured grid dataset with information

        me%name       = 'structured_grid'
        me%dimensions = dims
        me%n_points   = size(points,dim=2)
        me%points     = points
        me%firstcall  = .false.

    end procedure struct_grid_setup

    module procedure struct_grid_check_for_diffs
        implicit none
        !! function checks for differences in a structured grid dataset

        integer(i4k) :: i, j

        diffs = .false.
        if      (.not. same_type_as(me,you))  then
            diffs = .true.
        else if (me%name          /= you%name) then
            diffs = .true.
        else if (me%dimensions(1) /= you%dimensions(1) .or. &
            &      me%dimensions(2) /= you%dimensions(2) .or. &
            &      me%dimensions(3) /= you%dimensions(3)) then
            diffs = .true.
        else
            select type (you)
            class is (struct_grid)
                if      (me%n_points /= you%n_points) then
                    diffs = .true.
                else if (me%datatype /= you%datatype) then
                    diffs = .true.
                else if (size(me%points,dim=1) /= size(you%points,dim=1) .or. &
                    &      size(me%points,dim=2) /= size(you%points,dim=2)) then
                    diffs = .true.
                else
                    do i = 1, size(me%points,dim=1)
                        do j = 1, size(me%points,dim=2)
                            if (me%points(i,j) /= you%points(i,j)) then
                                diffs = .true.
                            end if
                        end do
                    end do
                end if
            end select
        end if

    end procedure struct_grid_check_for_diffs

    module procedure struct_grid_get_point
        implicit none
        !! function returns the min / max range of values in x,y,z coordinates

        if (i < lbound(me%points,dim=2) .or. i > ubound(me%points,dim=2)) then
            error stop 'error: array bounds have been exceeded in struct_grid_get_range'
        end if

        coord = me%points(1:3,i)

    end procedure struct_grid_get_point

    module procedure struct_grid_get_range
        implicit none
        !! function returns the min / max range of values in x,y,z coordinates

    end procedure struct_grid_get_range
    ! ****************
    ! rectilinear grid
    ! ****************
    module procedure rectlnr_grid_read
        use misc, only : interpret_string, def_len, char_dt, to_lowercase
        implicit none
        !! reads the rectilinear grid dataset information from the .vtk file

        integer(i4k)            :: i, j, iostat
        integer(i4k), parameter :: dim = 3
        logical                 :: end_of_file
        character(len=def_len)  :: line
        integer(i4k),      dimension(:), allocatable :: ints
        real(r8k),         dimension(:), allocatable :: reals
        type(char_dt),     dimension(:), allocatable :: chars
        character(len=13), dimension(3), parameter   :: descr_coord = &
        & [ 'x_coordinates', 'y_coordinates', 'z_coordinates' ]

        read(unit,100,iostat=iostat) line
        call interpret_string (line=line, datatype=[ 'c' ],         ignore='dataset ',     separator=' ', chars=chars)
        me%name = trim(chars(1)%text)
        if (allocated(chars)) deallocate(chars)

        read(unit,100,iostat=iostat) line
        call interpret_string (line=line, datatype=[ 'i','i','i' ], ignore='dimensions ',  separator=' ', ints=ints)
        me%dimensions = ints(1:3); allocate(me%x%coord(1:ints(1)), me%y%coord(1:ints(2)), me%z%coord(1:ints(3)))

        end_of_file = .false.; i = 0

        get_coords: do
            i = i + 1
            read(unit,100,iostat=iostat) line
            call interpret_string (line=line, datatype=[ 'i','c' ], ignore=descr_coord(i), separator=' ', ints=ints, chars=chars)
            select case (i)
            case (1)
                me%x%datatype = to_lowercase(trim(chars(1)%text))
            case (2)
                me%y%datatype = to_lowercase(trim(chars(1)%text))
            case (3)
                me%z%datatype = to_lowercase(trim(chars(1)%text))
            end select

            if (allocated(chars)) deallocate(chars)

            read(unit,100,iostat=iostat) line
            end_of_file = (iostat < 0)
            if (end_of_file) then
                exit get_coords
            else
                j = 0
                get_vals: do
                j = j + 1
                call interpret_string (line=line, datatype=[ 'r' ], separator=' ', reals=reals)
                select case (i)
                case (1)
                    me%x%coord(j) = reals(1)
                case (2)
                    me%y%coord(j) = reals(1)
                case (3)
                    me%z%coord(j) = reals(1)
                end select
                if (line == '') exit get_vals
            end do get_vals
            end if
            if (i == dim) exit get_coords  !! filled up array points
        end do get_coords

100     format((a))
    end procedure rectlnr_grid_read

    module procedure rectlnr_grid_write
        implicit none
        !! writes the rectilinear grid dataset information to the .vtk file

        write(unit,100) me%name
        write(unit,101) me%dimensions
        write(unit,102) me%dimensions(1), me%x%datatype
        write(unit,110) me%x%coord
        write(unit,103) me%dimensions(2), me%y%datatype
        write(unit,110) me%y%coord
        write(unit,104) me%dimensions(3), me%z%datatype
        write(unit,110) me%z%coord

100     format('dataset ',(a))
101     format('dimensions ',*(i0,' '))
102     format('x_coordinates ',i0,' ',(a))
103     format('y_coordinates ',i0,' ',(a))
104     format('z_coordinates ',i0,' ',(a))
110     format(*(es13.6))

    end procedure rectlnr_grid_write

    module procedure rectlnr_grid_setup
        implicit none
        !! sets up the rectilinear grid dataset with information

        if (dims(1) /= size(x_coords) .or. dims(2) /= size(y_coords) .or. dims(3) /= size(z_coords)) then
            error stop 'bad inputs for rectlnr_grid_setup. dims is not equal to size of coords.'
        end if

        me%name       = 'rectilinear_grid'
        me%dimensions = dims
        me%x%datatype = datatype
        me%y%datatype = datatype
        me%z%datatype = datatype
        me%x%coord    = x_coords
        me%y%coord    = y_coords
        me%z%coord    = z_coords
        me%firstcall  = .false.

    end procedure rectlnr_grid_setup

    module procedure rectlnr_grid_check_for_diffs
        implicit none
        !! function checks for differences in a rectilinear grid dataset
        integer(i4k) :: i

        diffs = .false.
        if      (.not. same_type_as(me,you))  then
            diffs = .true.
        else if (me%name          /= you%name) then
            diffs = .true.
        else if (me%dimensions(1) /= you%dimensions(1) .or. &
            &      me%dimensions(2) /= you%dimensions(2) .or. &
            &      me%dimensions(3) /= you%dimensions(3)) then
            diffs = .true.
        else
            select type (you)
            class is (rectlnr_grid)
                if      (me%x%datatype /= you%x%datatype .or. &
                    &    me%y%datatype /= you%y%datatype .or. &
                    &    me%z%datatype /= you%z%datatype) then
                    diffs = .true.
                else if (size(me%x%coord) /= size(you%x%coord) .or. &
                    &    size(me%y%coord) /= size(you%y%coord) .or. &
                    &    size(me%z%coord) /= size(you%z%coord)) then
                    diffs = .true.
                else
                    do i = 1, size(me%x%coord)
                        if (me%x%coord(i) /= you%x%coord(i)) then
                            diffs = .true.
                        end if
                    end do
                    do i = 1, size(me%y%coord)
                        if (me%y%coord(i) /= you%y%coord(i)) then
                            diffs = .true.
                        end if
                    end do
                    do i = 1, size(me%z%coord)
                        if (me%z%coord(i) /= you%z%coord(i)) then
                            diffs = .true.
                        end if
                    end do
                end if
            end select
        end if

    end procedure rectlnr_grid_check_for_diffs

    module procedure rectlnr_grid_get_range
        implicit none
        !! function returns the min / max range of values in x,y,z coordinates
        associate (x_min => range(1,1), x_max => range(2,1), &
            &      y_min => range(1,2), y_max => range(2,2), &
            &      z_min => range(1,3), z_max => range(2,3))
            x_min = minval(me%x%coord,dim=1); x_max = maxval(me%x%coord,dim=1)
            y_min = minval(me%y%coord,dim=1); y_max = maxval(me%y%coord,dim=1)
            z_min = minval(me%z%coord,dim=1); z_max = maxval(me%z%coord,dim=1)
        end associate

    end procedure rectlnr_grid_get_range

    module procedure rectlnr_grid_get_coord
        implicit none

        select case (dim)
        case (1)
            allocate(coord,source=me%x%coord)
        case (2)
            allocate(coord,source=me%y%coord)
        case (3)
            allocate(coord,source=me%z%coord)
        case default
            error stop 'error: invalid dimension (dim) requested in rectlnr_grid_get_coord'
        end select

    end procedure rectlnr_grid_get_coord
    ! **************
    ! polygonal data
    ! **************
    module procedure polygonal_data_read
        use misc,      only : interpret_string, def_len, char_dt, to_lowercase
        use vtk_cells, only : poly_vertex, poly_line, polygon, triangle_strip
        implicit none
        !! reads the polygonal data dataset information from the .vtk file
        integer(i4k)                       :: i, j, iostat, n, descr_size, n_points
        integer(i4k), parameter            :: dim = 3
        logical                            :: end_of_file
        character(len=def_len)             :: line
        character(len=:), allocatable      :: descr
        integer(i4k),  dimension(:), allocatable :: ints, dummy, points
        real(r8k),     dimension(:), allocatable :: reals
        type(char_dt), dimension(:), allocatable :: chars

        read(unit,100,iostat=iostat) line
        call interpret_string (line=line, datatype=[ 'c' ],         ignore='dataset ',     separator=' ', chars=chars)
        me%name = trim(chars(1)%text)
        if (allocated(chars)) deallocate(chars)

        read(unit,100,iostat=iostat) line
        call interpret_string (line=line, datatype=[ 'i','c' ],     ignore='points ',     separator=' ', ints=ints, chars=chars)
        me%n_points= ints(1); me%datatype = to_lowercase(trim(chars(1)%text))
        if (allocated(chars)) deallocate(chars)

        allocate(me%points(1:3,1:me%n_points)); end_of_file = .false.; i = 0

        get_points: do
            read(unit,100,iostat=iostat) line
            end_of_file = (iostat < 0)
            if (end_of_file) then
                exit get_points
            else if (trim(line) == '') then
                cycle     !! skip blank lines
            else
                i = i + 1
                call interpret_string (line=line, datatype=[ 'r','r','r' ], separator=' ', reals=reals)
                me%points(1:dim,i) = reals(1:dim)
            end if
            if (i == size(me%points,dim=2)) exit get_points  !! filled up array points
        end do get_points

        end_of_file = .false.; i = 0

        get_keywords: do
            read(unit,100,iostat=iostat) line
            end_of_file = (iostat < 0)
            if (end_of_file) then
                exit get_keywords
            else if (trim(line) == '') then
                cycle     !! skip blank lines
            end if

            call interpret_string (line=line, datatype=[ 'c','i','i' ], separator=' ', ints=ints, chars=chars)
            descr = trim(chars(1)%text); n = ints(1); descr_size = ints(2)
            if (allocated(chars)) deallocate(chars)

            select case (descr)
            case ('vertices')
                allocate(poly_vertex::me%vertices(1:n))
            case ('lines')
                allocate(poly_line::me%lines(1:n))
            case ('polygons')
                allocate(polygon::me%polygons(1:n))
            case ('triangle_strips')
                allocate(triangle_strip::me%triangles(1:n))
            case ('point_data', 'cell_data')
                exit get_keywords !! no additional information was provided for polygonal_data
            case default
                error stop 'bad value read for additional polygon information. terminated in polygonal_data_read'
            end select

            do i = 1, n
                read(unit,100,iostat=iostat) line
                end_of_file = (iostat < 0)
                if (end_of_file) then
                    exit get_keywords
                else
                    ! extract values
                    j = 0; if(allocated(points)) deallocate(points)
                    get_vals: do
                        j = j + 1
                        call interpret_string (line=line, datatype=[ 'i' ], separator=' ', ints=ints)
                        if (j == 1) then
                            n_points = ints(1)
                        else
                            allocate(dummy(1:j-1))
                            dummy(j-1) = ints(1)
                            if (j > 2) dummy(1:j-2) = points
                            if (allocated(points)) deallocate(points)
                            call move_alloc(dummy, points)
                        end if
                        if (line == '') exit get_vals
                    end do get_vals

                    ! save values
                    select case (descr)
                    case ('vertices')
                        me%vertices(i)%n_points  = n_points
                        me%vertices(i)%points    = points
                    case ('lines')
                        me%lines(i)%n_points     = n_points
                        me%lines(i)%points       = points
                    case ('polygons')
                        me%polygons(i)%n_points  = n_points
                        me%polygons(i)%points    = points
                    case ('triangle_strips')
                        me%triangles(i)%n_points = n_points
                        me%triangles(i)%points   = points
                    end select
                end if
            end do
        end do get_keywords

100     format((a))
    end procedure polygonal_data_read

    module procedure polygonal_data_write
        implicit none
        !! writes the polygonal data dataset information to the .vtk file
        integer(i4k) :: i, n, size_cnt

        write(unit,100) me%name
        write(unit,101) me%n_points, me%datatype

        do i = 1, me%n_points
            write(unit,102) me%points(1:3,i)
        end do

        !! the following options are not required inputs
        if (allocated(me%vertices)) then
            ! calculate sizes
            n        = size(me%vertices,dim=1)
            size_cnt = n
            do i = 1, n
                size_cnt = size_cnt + me%vertices(i)%n_points
            end do
            ! write values
            write(unit,103) n, size_cnt
            do i = 1, n
                call me%vertices(i)%write(unit)
            end do
        end if

        if (allocated(me%lines)) then
            ! calculate sizes
            n        = size(me%lines,dim=1)
            size_cnt = n
            do i = 1, n
                size_cnt = size_cnt + me%lines(i)%n_points
            end do
            ! write values
            write(unit,104) n, size_cnt
            do i = 1, n
                call me%lines(i)%write(unit)
            end do
        end if

        if (allocated(me%polygons)) then
            ! calculate sizes
            n        = size(me%polygons,dim=1)
            size_cnt = n
            do i = 1, n
                size_cnt = size_cnt + me%polygons(i)%n_points
            end do
            ! write values
            write(unit,105) n, size_cnt
            do i = 1, n
                call me%polygons(i)%write(unit)
            end do
        end if

        if (allocated(me%triangles)) then
            ! calculate sizes
            n        = size(me%triangles,dim=1)
            size_cnt = n
            do i = 1, n
                size_cnt = size_cnt + me%triangles(i)%n_points
            end do
            ! write values
            write(unit,106) n, size_cnt
            do i = 1, n
                call me%triangles(i)%write(unit)
            end do
        end if

100     format('dataset ',(a))
101     format('points ',(i0),' ',(a))
102     format(*(es13.6))
103     format('vertices ',(i0),' ',(i0))
104     format('lines ',(i0),' ',(i0))
105     format('polygons ',(i0),' ',(i0))
106     format('triangle_strips ',(i0),' ',(i0))

    end procedure polygonal_data_write

    module procedure polygonal_data_setup
        implicit none
        !! sets up the polygonal data dataset with information

        me%name       = 'polydata'
        me%n_points   = size(points,dim=2)
        me%points     = points
        me%firstcall  = .false.
        if (present(vertices )) allocate(me%vertices, source=vertices )
        if (present(lines    )) allocate(me%lines,    source=lines    )
        if (present(polygons )) allocate(me%polygons, source=polygons )
        if (present(triangles)) allocate(me%triangles,source=triangles)

    end procedure polygonal_data_setup

    module procedure polygonal_data_get_range
        implicit none
        !! function returns the min / max range of values in x,y,z coordinates

    end procedure polygonal_data_get_range
    ! *****************
    ! unstructured grid
    ! *****************
    module procedure unstruct_grid_read
        use misc,      only : interpret_string, def_len, char_dt, to_lowercase
        use vtk_cells, only : vtkcell, poly_vertex, set_cell_type
        implicit none
        !! reads the unstructured grid dataset information from the .vtk file
        class(vtkcell), allocatable :: dummy_cell
        integer(i4k)                :: i, iostat
        integer(i4k), parameter     :: dim = 3
        logical                     :: end_of_file
        character(len=def_len)      :: line
        integer(i4k),  dimension(:), allocatable :: ints
        real(r8k),     dimension(:), allocatable :: reals
        type(char_dt), dimension(:), allocatable :: chars
        type temp_points
            integer(i4k), dimension(:), allocatable  :: points
        end type temp_points
        type (temp_points), dimension(:), allocatable :: read_points

        read(unit,100,iostat=iostat) line
        call interpret_string (line=line, datatype=[ 'c' ],         ignore='dataset ',     separator=' ', chars=chars)
        me%name = trim(chars(1)%text)
        if (allocated(chars)) deallocate(chars)

        read(unit,100,iostat=iostat) line
        call interpret_string (line=line, datatype=[ 'i','c' ],     ignore='points ',     separator=' ', ints=ints, chars=chars)
        me%n_points= ints(1); me%datatype = to_lowercase(trim(chars(1)%text))

        allocate(me%points(1:3,1:me%n_points)); end_of_file = .false.

        get_points: do i = 1, me%n_points
            read(unit,100,iostat=iostat) line
            end_of_file = (iostat < 0)
            if (end_of_file) then
                exit get_points
            else if (trim(line) == '') then
                cycle     !! skip blank lines
            else
                call interpret_string (line=line, datatype=[ 'r','r','r' ], separator=' ', reals=reals)
                me%points(1:dim,i) = reals(1:dim)
            end if
        end do get_points

        ! get the cell information
        read(unit,100,iostat=iostat) line
        call interpret_string (line=line, datatype=[ 'i','i' ], ignore='points ', separator=' ', ints=ints)
        me%n_cells = ints(1); me%size = ints(2); allocate(read_points(1:ints(1)))
        allocate(me%cell_list(1:me%n_cells))

        end_of_file = .false.
        get_cells: do i = 1, me%n_cells
            !! temporary work around
            if (allocated(dummy_cell)) deallocate(dummy_cell)
            allocate(poly_vertex::dummy_cell)
            call dummy_cell%read(unit)
            read_points(i)%points = dummy_cell%points
        end do get_cells

        ! get the cell type information
        read(unit,100,iostat=iostat) line
        call interpret_string (line=line, datatype=[ 'i' ], ignore='cell_types ', separator=' ', ints=ints)
        me%n_cell_types = ints(1)

        end_of_file = .false.
        get_cell_type: do i = 1, me%n_cell_types
            read(unit,100,iostat=iostat) line
            end_of_file = (iostat < 0)
            if (end_of_file) then
                exit get_cell_type
            else if (trim(line) == '') then
                cycle     !! skip blank lines
            else
                call interpret_string (line=line, datatype=[ 'i' ], separator=' ', ints=ints)
                dummy_cell = set_cell_type (ints(1))
                allocate(me%cell_list(i)%cell,mold=dummy_cell)
                call me%cell_list(i)%cell%setup (read_points(i)%points)
                deallocate(dummy_cell)
            end if
        end do get_cell_type

100     format((a))
    end procedure unstruct_grid_read

    module procedure unstruct_grid_write
        implicit none
        !! writes the unstructured grid dataset information from the .vtk file
        integer(i4k) :: i

        write(unit,100) me%name
        write(unit,101) me%n_points, me%datatype

        do i = 1, me%n_points
            write(unit,102) me%points(1:3,i)
        end do

        write(unit,103) me%n_cells, me%size
        do i = 1, me%n_cells
            call me%cell_list(i)%cell%write(unit)
        end do

        write(unit,104) me%n_cell_types
        do i = 1, me%n_cell_types
            write(unit,105) me%cell_list(i)%cell%type
        end do

100     format('dataset ',(a))
101     format('points ',(i0),' ',(a))
102     format(*(es13.6))
103     format('cells ',(i0),' ',(i0))
104     format('cell_types ',(i0))
105     format(' ',i0)

    end procedure unstruct_grid_write

    module procedure unstruct_grid_setup
        implicit none
        !! sets up the unstructured grid dataset with information
        integer(i4k) :: i, size_cnt

        me%name         = 'unstructured_grid'
        me%n_points     = size(points, dim=2)
        me%n_cells      = size(cells,  dim=1)
        me%n_cell_types = size(cells,  dim=1)
        me%points       = points
        allocate(me%cell_list(1:me%n_cells))
        size_cnt = me%n_cells
        do i = 1, me%n_cells
            allocate(me%cell_list(i)%cell,source=cells(i))
            size_cnt = size_cnt + me%cell_list(i)%cell%n_points
        end do
        me%size         = size_cnt
        me%firstcall    = .false.

    end procedure unstruct_grid_setup

    module procedure unstruct_grid_setup_multiclass
        implicit none
        !! sets up the unstructured grid dataset with information
        integer(i4k) :: i, size_cnt

        me%name         = 'unstructured_grid'
        me%n_points     = size(points,    dim=2)
        me%n_cells      = size(cell_list, dim=1)
        me%n_cell_types = size(cell_list, dim=1)
        me%points       = points
        me%cell_list    = cell_list
        size_cnt        = me%n_cells
        do i = 1, me%n_cells
            size_cnt = size_cnt + me%cell_list(i)%cell%n_points
        end do
        me%size         = size_cnt
        me%firstcall    = .false.

    end procedure unstruct_grid_setup_multiclass

    module procedure unstruct_grid_get_range
        implicit none
        !! function returns the min / max range of values in x,y,z coordinates

    end procedure unstruct_grid_get_range

    module procedure unstruct_grid_get_point
        implicit none
        !! function returns the point (i)

        if (i < lbound(me%points,dim=2) .or. i > ubound(me%points,dim=2)) then
            error stop 'error: array bounds have been exceeded in unstruct_grid_get_range'
        end if

        coord = me%points(1:3,i)

    end procedure unstruct_grid_get_point

    module procedure unstruct_grid_get_connectivity
        implicit none
        !! function returns the connectivity for cell (i)

        if (i < lbound(me%cell_list,dim=1) .or. i > ubound(me%cell_list,dim=1)) then
            error stop 'error: array bounds have been exceeded in unstruct_grid_get_connectivity'
        end if

        allocate(connectivity, source=me%cell_list(i)%cell%points)

    end procedure unstruct_grid_get_connectivity

    module procedure unstruct_grid_get_offset
        implicit none
        !! function returns the offset for cell (i)
        !! the offset is just the # of points

        if (i < lbound(me%cell_list,dim=1) .or. i > ubound(me%cell_list,dim=1)) then
            error stop 'error: array bounds have been exceeded in unstruct_grid_get_offset'
        end if

        offset = me%cell_list(i)%cell%n_points

    end procedure unstruct_grid_get_offset

    module procedure unstruct_grid_get_type
        implicit none
        !! function returns the type for cell (i)

        if (i < lbound(me%cell_list,dim=1) .or. i > ubound(me%cell_list,dim=1)) then
            error stop 'error: array bounds have been exceeded in unstruct_grid_get_type'
        end if

        type = me%cell_list(i)%cell%type

    end procedure unstruct_grid_get_type

end submodule vtk_datasets_procedures
