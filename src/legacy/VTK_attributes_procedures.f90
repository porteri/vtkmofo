submodule (vtk_attributes) vtk_attributes_procedures
    use misc, only : def_len, char_dt
    implicit none
    !! author: Ian Porter
    !! date: 12/13/2017
    !!
    !! this module contains the dataset attributes for vtk format
    !!
    !! the following dataset attributes are available:
    !! 1) scalars
    !! 2) vectors
    !! 3) normals
    !! 4) texture coordinates (1d, 2d & 3d)
    !! 5) 3x3 tensors
    !! 6) field data
    !!
    !! possible data types:
    !! bit, unsigned_char, char, unsigned_short, short, unsigned_int, int,
    !! unsigned_long, long, float, or double.
    character(len=*), parameter :: default = 'default'     !! default table name

contains

    module procedure abs_read
        implicit none
        !! author: Ian Porter
        !! date: 12/13/2017
        !!
        !! abstract for reading an attribute
        !!
        select type (me)
        class is (attribute)
            read(unit,*) me%dataname !! workaround for ifort 2018 linux compiler error (not error for 2018 on windows)
            !! that a class with intent(out) was not provided a value
        end select
    end procedure abs_read

    module procedure abs_write
        implicit none
        !! author: Ian Porter
        !! date: 12/13/2017
        !!
        !! abstract for writing an attribute
        !!
        select type (me)
        class is (attribute)
            write(unit,*) me%dataname
        end select
    end procedure abs_write

    module procedure initialize
        implicit none
        !! author: Ian Porter
        !! date: 12/13/2017
        !!
        !! abstract for performing the set-up of an attribute
        !!
        select type (me)
        class is (scalar)
            call me%setup(dataname, datatype, numcomp, tablename, int1d, real1d)
        class is (vector)
            call me%setup(dataname, datatype, int2d, real2d)
        class is (normal)
            call me%setup(dataname, datatype, int2d, real2d)
        class is (texture)
            call me%setup(dataname, datatype, int2d, real2d)
        class is (tensor)
            call me%setup(dataname, datatype, int3d, real3d)
        class is (field)
            call me%setup(dataname, datatype, field_arrays)
        class default
            error stop 'generic class not defined for vtkmofo class attribute'
        end select

    end procedure initialize

    module procedure check_for_diffs
        implicit none
        !! author: Ian Porter
        !! date: 12/13/2017
        !!
        !! function checks for differences in an attribute
        !!
        diffs = .false.
        if      (.not. same_type_as(me,you))  then
            diffs = .true.
        else if (me%dataname /= you%dataname) then
            diffs = .true.
        end if

    end procedure check_for_diffs

    module procedure convert_to_dataarray
        implicit none
        !! author: Ian Porter
        !! date: 07/20/2019
        !!
        !! function converts an attribute to a dataarray
        !!
        !! array, me
        call array%initialize(name=me%dataname, type=me%datatype)

    end procedure convert_to_dataarray

    !********
    ! scalars
    !********
    module procedure scalar_read
        use misc, only : interpret_string, to_lowercase
        implicit none
        !! author: Ian Porter
        !! date: 12/13/2017
        !!
        !! subroutine performs the read for a scalar attribute
        !!
        integer(i4k)               :: i, iostat
        logical                    :: end_of_file
        character(len=def_len)     :: line
        integer(i4k),  dimension(:), allocatable :: ints
        real(r8k),     dimension(:), allocatable :: reals, dummy
        type(char_dt), dimension(:), allocatable :: chars

        read(unit,100) line
        call interpret_string (line=line, datatype=[ 'c','c','i' ], ignore='scalars ', separator=' ', &
            &                  ints=ints, chars=chars)
        me%numcomp = ints(1); me%dataname = trim(chars(1)%text); me%datatype = to_lowercase(trim(chars(2)%text))
        if (allocated(ints)) deallocate(ints)
        if (allocated(chars)) deallocate(chars)

        read(unit,100) line
        call interpret_string (line=line, datatype=[ 'c' ], ignore='lookup_table ', separator=' ', chars=chars)
        me%tablename = trim(chars(1)%text)

        select case (me%datatype)
        case ('unsigned_int', 'int')
            allocate(me%ints(0))
        case ('float', 'double')
            allocate(me%reals(0))
        case default
            error stop 'datatype not supported in scalar_read'
        end select

        end_of_file  = .false.; i = 0

        get_scalars: do
            read(unit,100,iostat=iostat) line
            end_of_file = (iostat < 0)
            if (end_of_file) then
                exit get_scalars
            else if (trim(line) == '') then
                cycle     !! skip blank lines
            else
                select case (me%datatype)
                case ('unsigned_int', 'int')
                    allocate(ints(1:ubound(me%ints,dim=1)+1),source=0_i4k)
                    if (i > 0) ints(1:ubound(me%ints,dim=1)) = me%ints
                    call move_alloc(ints, me%ints)
                    i = i + 1

                    call interpret_string (line=line, datatype=[ 'i' ], separator=' ', ints=ints)
                    me%ints(i) = ints(1)
                    deallocate(ints)
                case ('float', 'double')
                    allocate(dummy(1:ubound(me%reals,dim=1)+1),source=0.0_r8k)
                    if (i > 0) dummy(1:ubound(me%reals,dim=1)) = me%reals
                    call move_alloc(dummy, me%reals)
                    i = i + 1

                    call interpret_string (line=line, datatype=[ 'r' ], separator=' ', reals=reals)
                    me%reals(i) = reals(1)
                case default
                    error stop 'datatype not supported in scalar_read'
                end select
            end if
        end do get_scalars

        if (allocated(me%ints)) then
            me%nvals = size(me%ints)
        else if (allocated(me%reals)) then
            me%nvals = size(me%reals)
        end if

100     format((a))
    end procedure scalar_read

    module procedure scalar_write
        implicit none
        !! author: Ian Porter
        !! date: 12/13/2017
        !!
        !! subroutine performs the write for a scalar attribute
        !!
        integer(i4k) :: i

        write(unit,100) me%dataname, me%datatype, me%numcomp
        write(unit,101) me%tablename
        if (allocated(me%reals)) then
            do i = 1, size(me%reals)
                write(unit,102) me%reals(i)
            end do
        else if (allocated(me%ints)) then
            do i = 1, size(me%ints)
                write(unit,103) me%ints(i)
            end do
        else
            error stop 'neither real or integer arrays are allocated for scalar_write'
        end if

100     format('scalars ',(a),' ',(a),' ',(i1))
101     format('lookup_table ',(a))
102     format(es13.6)
103     format(i0)

    end procedure scalar_write

    module procedure scalar_setup
        implicit none
        !! author: Ian Porter
        !! date: 12/13/2017
        !!
        !! subroutine performs the set-up for a scalar attribute
        !!
        me%dataname = dataname
        if (present(datatype)) then
            me%datatype = datatype
        else if (present(int1d)) then
            me%datatype = 'int'
        else
            me%datatype = 'double'
        end if
        if (present(numcomp)) then
            me%numcomp = numcomp
        else
            me%numcomp = 1
        end if
        if (present(tablename)) then
            me%tablename = tablename
        else
            me%tablename = default
        end if
        if (present(int1d)) then
            me%ints = int1d
            me%nvals = size(me%ints)
        else if (present(real1d)) then
            me%reals = real1d
            me%nvals = size(me%reals)
        else
            error stop 'must provide either array of integers or reals in scalar_setup'
        end if

    end procedure scalar_setup

    module procedure scalar_check_for_diffs
        implicit none
        !! author: Ian Porter
        !! date: 12/13/2017
        !!
        !! function checks for differences in a scalar attribute
        !!
        integer(i4k) :: i

        diffs = .false.
        if (.not. same_type_as(me,you)) then
            diffs = .true.
        else
            select type (you)
            class is (scalar)
                if (me%dataname /= you%dataname)         then
                    diffs = .true.
                else if (me%datatype /= you%datatype)    then
                    diffs = .true.
                else if (me%nvals /= you%nvals)          then
                    diffs = .true.
                else if (me%numcomp /= you%numcomp)      then
                    diffs = .true.
                else if (me%tablename /= you%tablename)  then
                    diffs = .true.
                else if (allocated(me%reals))            then
                    do i = 1, ubound(me%reals,dim=1)
                        if (me%reals(i) /= you%reals(i)) then
                            diffs = .true.
                        end if
                    end do
                else if (allocated(me%ints))             then
                    do i = 1, ubound(me%ints,dim=1)
                        if (me%ints(i) /= you%ints(i))   then
                            diffs = .true.
                        end if
                    end do
                end if
            end select
        end if

    end procedure scalar_check_for_diffs

    module procedure scalar_convert_to_dataarray
        implicit none
        !! author: Ian Porter
        !! date: 07/20/2019
        !!
        !! function converts an attribute to a dataarray
        !!
        integer(i4k) :: i

        call array%initialize(name=trim(adjustl(me%dataname)), type=me%datatype)

        do i = 1, me%nvals
            if (allocated(me%ints)) then
                call array%add([me%ints(i)])
            else if (allocated(me%reals)) then
                call array%add([me%reals(i)])
            end if
        end do

    end procedure scalar_convert_to_dataarray

    !********
    ! vectors
    !********
    module procedure vector_read
        use misc, only : interpret_string, to_lowercase
        implicit none
        !! author: Ian Porter
        !! date: 12/14/2017
        !!
        !! subroutine performs the read for a vector attribute
        !!
        integer(i4k)             :: i, iostat
        integer(i4k),  parameter :: dim = 3
        logical                  :: end_of_file
        character(len=def_len)   :: line
        integer(i4k),  dimension(:),   allocatable :: ints
        real(r8k),     dimension(:),   allocatable :: reals
        type(char_dt), dimension(:),   allocatable :: chars
        integer(i4k),  dimension(:,:), allocatable :: i_dummy
        real(r8k),     dimension(:,:), allocatable :: r_dummy

        read(unit,100) line
        call interpret_string (line=line, datatype=[ 'c','c' ], ignore='vectors ', separator=' ', chars=chars)
        me%dataname = trim(chars(1)%text); me%datatype = to_lowercase(trim(chars(2)%text))

        select case (me%datatype)
        case ('unsigned_int', 'int')
            allocate(me%i_vector(0,0))
        case ('float', 'double')
            allocate(me%r_vector(0,0))
        case default
            error stop 'datatype not supported in scalar_read'
        end select

        end_of_file = .false.; i = 0

        get_vectors: do
            read(unit,100,iostat=iostat) line
            end_of_file = (iostat < 0)
            if (end_of_file) then
                exit get_vectors
            else if (trim(line) == '') then
                cycle     !! skip blank lines
            else
                select case (me%datatype)
                case ('unsigned_int', 'int')
                    allocate(i_dummy(1:ubound(me%i_vector,dim=1)+1,1:dim),source=0_i4k)
                    if (i > 0) i_dummy(1:ubound(me%i_vector,dim=1),1:dim) = me%i_vector
                    call move_alloc(i_dummy, me%i_vector)
                    i = i + 1

                    call interpret_string (line=line, datatype=[ 'i','i','i' ], separator=' ', ints=ints)
                    me%i_vector(i,1:dim) = ints(1:dim)
                case ('float', 'double')
                    allocate(r_dummy(1:ubound(me%r_vector,dim=1)+1,1:dim),source=0.0_r8k)
                    if (i > 0) r_dummy(1:ubound(me%r_vector,dim=1),1:dim) = me%r_vector
                    call move_alloc(r_dummy, me%r_vector)
                    i = i + 1

                    call interpret_string (line=line, datatype=[ 'r','r','r' ], separator=' ', reals=reals)
                    me%r_vector(i,1:dim) = reals(1:dim)
                end select
            end if
        end do get_vectors

        if (allocated(me%i_vector)) then
            me%nvals = size(me%i_vector, dim=1)
        else if (allocated(me%r_vector)) then
            me%nvals = size(me%r_vector, dim=1)
        end if

100     format((a))
    end procedure vector_read

    module procedure vector_write
        implicit none
        !! author: Ian Porter
        !! date: 12/13/2017
        !!
        !! subroutine performs the write for a vector attribute
        !!
        integer(i4k) :: i

        write(unit,100) me%dataname, me%datatype
        if (allocated(me%i_vector)) then
            do i = 1, size(me%i_vector,dim=1)
                write(unit,101) me%i_vector(i,1:3)
            end do
        else if (allocated(me%r_vector)) then
            do i = 1, size(me%r_vector,dim=1)
                write(unit,102) me%r_vector(i,1:3)
            end do
        end if

100     format('vectors ',(a),' ',(a))
101     format(*(i8,' '))
102     format(*(es13.6,' '))
    end procedure vector_write

    module procedure vector_setup
        implicit none
        !! author: Ian Porter
        !! date: 12/14/2017
        !!
        !! subroutine performs the set-up for a vector attribute
        !!
        me%dataname = dataname
        if (present(datatype)) then
            me%datatype = datatype
        else
            me%datatype = 'double'
        end if
        if (present(int2d)) then
            if (me%datatype == 'double') me%datatype = 'int'
            allocate(me%i_vector, source=int2d)
            me%nvals = size(me%i_vector,dim=1)
        else if (present(real2d)) then
            allocate(me%r_vector, source=real2d)
            me%nvals = size(me%r_vector,dim=1)
        else
            error stop 'error: must provide either int2d or real2d in vector_setup'
        end if

    end procedure vector_setup

    module procedure vector_check_for_diffs
        implicit none
        !! author: Ian Porter
        !! date: 12/14/2017
        !!
        !! function checks for differences in a vector attribute
        !!
        integer(i4k) :: i, j

        diffs = .false.
        if (.not. same_type_as(me,you)) then
            diffs = .true.
        else
            select type (you)
            class is (vector)
                if (me%dataname /= you%dataname)      then
                    diffs = .true.
                else if (me%datatype /= you%datatype) then
                    diffs = .true.
                else if (me%nvals /= you%nvals)       then
                    diffs = .true.
                else if (allocated(me%i_vector))      then
                    do i = 1, ubound(me%i_vector,dim=1)
                        do j = 1, ubound(me%i_vector,dim=2)
                            if (me%i_vector(i,j) /= you%i_vector(i,j)) then
                                diffs = .true.
                            end if
                        end do
                    end do
                else if (allocated(me%r_vector))      then
                    do i = 1, ubound(me%r_vector,dim=1)
                        do j = 1, ubound(me%r_vector,dim=2)
                            if (me%r_vector(i,j) /= you%r_vector(i,j)) then
                                diffs = .true.
                            end if
                        end do
                    end do
                else
                    diffs = .true.
                end if
            end select
        end if

    end procedure vector_check_for_diffs
    !********
    ! normals
    !********
    module procedure normal_read
        use misc, only : interpret_string, to_lowercase
        implicit none
        !! author: Ian Porter
        !! date: 12/14/2017
        !!
        !! subroutine performs the read for a normal attribute
        !!
        integer(i4k)               :: i, iostat
        integer(i4k),  parameter   :: dim = 3
        logical                    :: end_of_file
        character(len=def_len)     :: line
        integer(i4k),  dimension(:),   allocatable :: ints
        real(r8k),     dimension(:),   allocatable :: reals
        type(char_dt), dimension(:),   allocatable :: chars
        integer(i4k),  dimension(:,:), allocatable :: i_dummy
        real(r8k),     dimension(:,:), allocatable :: r_dummy

        read(unit,100) line
        call interpret_string (line=line, datatype=[ 'c','c' ], ignore='normals ', separator=' ', chars=chars)
        me%dataname = trim(chars(1)%text); me%datatype = to_lowercase(trim(chars(2)%text))

        select case (me%datatype)
        case ('unsigned_int', 'int')
            allocate(me%i_normal(0,0))
        case ('float', 'double')
            allocate(me%r_normal(0,0))
        case default
            error stop 'datatype not supported in normal_read'
        end select

        end_of_file = .false.; i = 0

        get_normals: do
            read(unit,100,iostat=iostat) line
            end_of_file = (iostat < 0)
            if (end_of_file) then
                exit get_normals
            else if (trim(line) == '') then
                cycle     !! skip blank lines
            else
                select case (me%datatype)
                case ('unsigned_int', 'int')
                    allocate(i_dummy(1:ubound(me%i_normal,dim=1)+1,1:dim),source=0_i4k)
                    if (i > 0) i_dummy(1:ubound(me%i_normal,dim=1),1:dim) = me%i_normal
                    call move_alloc(i_dummy, me%i_normal)
                    i = i + 1

                    call interpret_string (line=line, datatype=[ 'i','i','i' ], separator=' ', ints=ints)
                    me%i_normal(i,1:dim) = ints(1:dim)
                case ('float', 'double')
                    allocate(r_dummy(1:ubound(me%r_normal,dim=1)+1,1:dim),source=0.0_r8k)
                    if (i > 0) r_dummy(1:ubound(me%r_normal,dim=1),1:dim) = me%r_normal
                    call move_alloc(r_dummy, me%r_normal)
                    i = i + 1

                    call interpret_string (line=line, datatype=[ 'r','r','r' ], separator=' ', reals=reals)
                    me%r_normal(i,1:dim) = reals(1:dim)
                end select
            end if
        end do get_normals

        if (allocated(me%i_normal)) then
            me%nvals = size(me%i_normal, dim=1)
        else if (allocated(me%r_normal)) then
            me%nvals = size(me%r_normal, dim=1)
        end if

100     format((a))
    end procedure normal_read

    module procedure normal_write
        implicit none
        !! author: Ian Porter
        !! date: 12/13/2017
        !!
        !! subroutine performs the write for a normal attribute
        !!
        integer(i4k) :: i

        write(unit,100) me%dataname, me%datatype
        if (allocated(me%i_normal)) then
            do i = 1, size(me%i_normal,dim=1)
                write(unit,101) me%i_normal(i,1:3)
            end do
        else if (allocated(me%r_normal)) then
            do i = 1, size(me%r_normal,dim=1)
                write(unit,102) me%r_normal(i,1:3)
            end do
        end if

100     format('normals ',(a),' ',(a))
101     format(*(i8,' '))
102     format(*(es13.6,' '))
    end procedure normal_write

    module procedure normal_setup
        implicit none
        !! author: Ian Porter
        !! date: 12/14/2017
        !!
        !! subroutine performs the set-up for a normal attribute
        !!
        me%dataname = dataname
        if (present(datatype)) then
            me%datatype = datatype
        else
            me%datatype = 'double'
        end if
        if (present(int2d)) then
            if (me%datatype == 'double') me%datatype = 'int'
            allocate(me%i_normal, source=int2d)
            me%nvals = size(me%i_normal,dim=1)
        else if (present(real2d)) then
            allocate(me%r_normal, source=real2d)
            me%nvals = size(me%r_normal,dim=1)
        else
            error stop 'error: must provide either int2d or real2d in normal_setup'
        end if

    end procedure normal_setup

    module procedure normal_check_for_diffs
        implicit none
        !! author: Ian Porter
        !! date: 12/14/2017
        !!
        !! function checks for differences in a normal attribute
        !!
        integer(i4k) :: i, j

        diffs = .false.
        if (.not. same_type_as(me,you)) then
            diffs = .true.
        else
            select type (you)
            class is (normal)
                if (me%dataname /= you%dataname)      then
                    diffs = .true.
                else if (me%datatype /= you%datatype) then
                    diffs = .true.
                else if (me%nvals /= you%nvals)       then
                    diffs = .true.
                else if (allocated(me%i_normal))      then
                    do i = 1, ubound(me%i_normal,dim=1)
                        do j = 1, ubound(me%i_normal,dim=2)
                            if (me%i_normal(i,j) /= you%i_normal(i,j)) then
                                diffs = .true.
                            end if
                        end do
                    end do
                else if (allocated(me%r_normal))      then
                    do i = 1, ubound(me%r_normal,dim=1)
                        do j = 1, ubound(me%r_normal,dim=2)
                            if (me%r_normal(i,j) /= you%r_normal(i,j)) then
                                diffs = .true.
                            end if
                        end do
                    end do
                else
                    diffs = .true.
                end if
            end select
        end if

    end procedure normal_check_for_diffs
    !********
    ! textures
    !********
    module procedure texture_read
        use misc, only : interpret_string, to_lowercase
        implicit none
        !! author: Ian Porter
        !! date: 12/14/2017
        !!
        !! subroutine performs the read for a texture attribute
        !!
        integer(i4k)           :: i, iostat, dim
        logical                :: end_of_file
        character(len=def_len) :: line
        integer(i4k),     dimension(:),   allocatable :: ints
        real(r8k),        dimension(:),   allocatable :: reals
        type(char_dt),    dimension(:),   allocatable :: chars
        integer(i4k),     dimension(:,:), allocatable :: i_dummy
        real(r8k),        dimension(:,:), allocatable :: r_dummy
        character(len=1), dimension(3),   parameter   :: i_datatype = [ 'i','i','i' ]
        character(len=1), dimension(3),   parameter   :: r_datatype = [ 'r','r','r' ]

        read(unit,100) line
        call interpret_string (line=line, datatype=[ 'c','i','c' ], ignore='texture_coordinates ', separator=' ', &
            &                  ints=ints, chars=chars)
        me%dataname = trim(chars(1)%text); dim = ints(1); me%datatype = to_lowercase(trim(chars(2)%text))

        select case (me%datatype)
        case ('unsigned_int', 'int')
            allocate(me%i_texture(0,0))
        case ('float', 'double')
            allocate(me%r_texture(0,0))
        case default
            error stop 'datatype not supported in texture_read'
        end select

        end_of_file = .false.; i = 0

        get_textures: do
            read(unit,100,iostat=iostat) line
            end_of_file = (iostat < 0)
            if (end_of_file) then
                exit get_textures
            else if (trim(line) == '') then
                cycle     !! skip blank lines
            else
                select case (me%datatype)
                case ('unsigned_int', 'int')
                    allocate(i_dummy(1:ubound(me%i_texture,dim=1)+1,1:dim),source=0_i4k)
                    if (i > 0) i_dummy(1:ubound(me%i_texture,dim=1),1:dim) = me%i_texture
                    call move_alloc(i_dummy, me%i_texture)
                    i = i + 1

                    call interpret_string (line=line, datatype=i_datatype(1:dim), separator=' ', ints=ints)
                    me%i_texture(i,1:dim) = ints(1:dim)
                case ('float', 'double')
                    allocate(r_dummy(1:ubound(me%r_texture,dim=1)+1,1:dim),source=0.0_r8k)
                    if (i > 0) r_dummy(1:ubound(me%r_texture,dim=1),1:dim) = me%r_texture
                    call move_alloc(r_dummy, me%r_texture)
                    i = i + 1

                    call interpret_string (line=line, datatype=r_datatype(1:dim), separator=' ', reals=reals)
                    me%r_texture(i,1:dim) = reals(1:dim)
                end select
            end if
        end do get_textures

        if (allocated(me%i_texture)) then
            me%nvals = size(me%i_texture, dim=1)
        else if (allocated(me%r_texture)) then
            me%nvals = size(me%r_texture, dim=1)
        end if

100     format((a))
    end procedure texture_read

    module procedure texture_write
        implicit none
        !! author: Ian Porter
        !! date: 12/13/2017
        !!
        !! subroutine performs the write for a texture attribute
        !!
        integer(i4k) :: i, dim

        if (allocated(me%i_texture)) then
            dim = size(me%i_texture,dim=2)
        else if (allocated(me%r_texture)) then
            dim = size(me%r_texture,dim=2)
        else
            dim = 0
        end if

        write(unit,100) me%dataname, dim, me%datatype
        if (allocated(me%i_texture)) then
            do i = 1, size(me%i_texture,dim=1)
                write(unit,101) me%i_texture(i,:)
            end do
        else if (allocated(me%r_texture)) then
            do i = 1, size(me%r_texture,dim=1)
                write(unit,102) me%r_texture(i,:)
            end do
        end if

100     format('texture_coordinates ',(a),' ',(i1),' ',(a))
101     format(*(i8,' '))
102     format(*(es13.6,' '))
    end procedure texture_write

    module procedure texture_setup
        implicit none
        !! author: Ian Porter
        !! date: 12/14/2017
        !!
        !! subroutine performs the set-up for a texture attribute
        !!
        me%dataname = dataname
        if (present(datatype)) then
            me%datatype = datatype
        else
            me%datatype = 'double'
        end if
        if (present(int2d)) then
            if (me%datatype == 'double') me%datatype = 'int'
            allocate(me%i_texture, source=int2d)
            me%nvals = size(me%i_texture,dim=1)
        else if (present(real2d)) then
            allocate(me%r_texture, source=real2d)
            me%nvals = size(me%r_texture,dim=1)
        else
            error stop 'error: must provide either int2d or real2d in texture_setup'
        end if

    end procedure texture_setup

    module procedure texture_check_for_diffs
        implicit none
        !! author: Ian Porter
        !! date: 12/14/2017
        !!
        !! function checks for differences in a texture attribute
        !!
        integer(i4k) :: i, j

        diffs = .false.
        if (.not. same_type_as(me,you)) then
            diffs = .true.
        else
            select type (you)
            class is (texture)
                if (me%dataname /= you%dataname)      then
                    diffs = .true.
                else if (me%datatype /= you%datatype) then
                    diffs = .true.
                else if (me%nvals /= you%nvals)       then
                    diffs = .true.
                else if (allocated(me%i_texture))     then
                    do i = 1, ubound(me%i_texture,dim=1)
                        do j = 1, ubound(me%i_texture,dim=2)
                            if (me%i_texture(i,j) /= you%i_texture(i,j)) then
                                diffs = .true.
                            end if
                        end do
                    end do
                else if (allocated(me%r_texture))     then
                    do i = 1, ubound(me%r_texture,dim=1)
                        do j = 1, ubound(me%r_texture,dim=2)
                            if (me%r_texture(i,j) /= you%r_texture(i,j)) then
                                diffs = .true.
                            end if
                        end do
                    end do
                else
                    diffs = .true.
                end if
            end select
        end if

    end procedure texture_check_for_diffs
    !********
    ! tensors
    !********
    module procedure tensor_read
        use misc, only : interpret_string, to_lowercase
        implicit none
        !! author: Ian Porter
        !! date: 12/14/2017
        !!
        !! subroutine performs the read for a tensor attribute
        !!
        integer(i4k)               :: i, j, iostat
        logical                    :: end_of_file
        character(len=def_len)     :: line
        integer(i4k),         dimension(:), allocatable :: ints
        real(r8k),            dimension(:), allocatable :: reals
        type(char_dt),        dimension(:), allocatable :: chars
        type(r_tensor_array), dimension(:), allocatable :: r_dummy
        type(i_tensor_array), dimension(:), allocatable :: i_dummy

        read(unit,100) line
        call interpret_string (line=line, datatype=[ 'c','c' ], ignore='tensors ', separator=' ', &
            &                  chars=chars)
        me%dataname = trim(chars(1)%text); me%datatype = to_lowercase(trim(chars(2)%text))

        select case (me%datatype)
        case ('unsigned_int', 'int')
            allocate(me%i_tensor(0))
        case ('float', 'double')
            allocate(me%r_tensor(0))
        case default
            error stop 'unsupported data type for tensor_read.'
        end select

        end_of_file = .false.; i = 0

        get_tensors: do
            read(unit,100,iostat=iostat) line
            end_of_file = (iostat < 0)
            if (end_of_file) then
                exit get_tensors
            else if (trim(line) == '') then
                cycle      !! skip blank lines
            else
                select case (me%datatype)
                case ('unsigned_int', 'int')
                    !! integers
                    allocate(i_dummy(1:ubound(me%i_tensor,dim=1)+1))
                    i_dummy(1:ubound(me%i_tensor,dim=1)) = me%i_tensor
                    call move_alloc(i_dummy, me%i_tensor)
                    i = i + 1

                    do j = 1, ubound(me%i_tensor(i)%val,dim=1)
                        if (j > 1) read(unit,100,iostat=iostat) line
                        call interpret_string (line=line, datatype=[ 'i','i','i' ], separator=' ', ints=ints)
                        me%i_tensor(i)%val(j,1:3) = ints(1:3)
                    end do
                case ('float', 'double')
                    !! reals
                    allocate(r_dummy(1:ubound(me%r_tensor,dim=1)+1))
                    r_dummy(1:ubound(me%r_tensor,dim=1)) = me%r_tensor
                    call move_alloc(r_dummy, me%r_tensor)
                    i = i + 1

                    do j = 1, ubound(me%r_tensor(i)%val,dim=1)
                        if (j > 1) read(unit,100,iostat=iostat) line
                        call interpret_string (line=line, datatype=[ 'r','r','r' ], separator=' ', reals=reals)
                        me%r_tensor(i)%val(j,1:3) = reals(1:3)
                    end do
                end select
            end if
        end do get_tensors

        if (allocated(me%i_tensor)) then
            me%nvals = size(me%i_tensor, dim=1)
        else if (allocated(me%r_tensor)) then
            me%nvals = size(me%r_tensor, dim=1)
        end if

100     format((a))
    end procedure tensor_read

    module procedure tensor_write
        implicit none
        !! author: Ian Porter
        !! date: 12/13/2017
        !!
        !! subroutine performs the write for a tensor attribute
        !!
        integer(i4k) :: i, j

        write(unit,100) me%dataname, me%datatype
        if (allocated(me%i_tensor)) then
            do i = 1, size(me%i_tensor,dim=1)
                do j = 1, size(me%i_tensor(i)%val,dim=1)
                    write(unit,101) me%i_tensor(i)%val(j,:)
                end do
                write(unit,105)
            end do
        else if (allocated(me%r_tensor)) then
            do i = 1, size(me%r_tensor,dim=1)
                do j = 1, size(me%r_tensor(i)%val,dim=1)
                    write(unit,102) me%r_tensor(i)%val(j,:)
                end do
                write(unit,105)
            end do
        end if

100     format('tensors ',(a),' ',(a))
101     format(*(i8,' '))
102     format(*(es13.6,' '))
105     format()
    end procedure tensor_write

    module procedure tensor_setup
        implicit none
        !! author: Ian Porter
        !! date: 12/14/2017
        !!
        !! subroutine performs the set-up for a tensor attribute
        !!
        integer(i4k) :: i

        me%dataname = dataname
        if (present(datatype)) then
            me%datatype = datatype
        else if (present(int3d)) then
            me%datatype = 'int'
        else
            me%datatype = 'double'
        end if
        if (present(int3d)) then
            if (size(int3d,dim=2) /= 3 .or. size(int3d,dim=3) /= 3) then
                error stop 'tensors can only be 3x3'
            else
                allocate(me%i_tensor(1:ubound(int3d,dim=1)))
                do i = 1, ubound(int3d,dim=1)
                    me%i_tensor(i)%val(1:3,1:3) = int3d(i,1:3,1:3)
                end do
                me%nvals = size(me%i_tensor,dim=1)
            end if
        else if (present(real3d)) then
            if (size(real3d,dim=2) /= 3 .or. size(real3d,dim=3) /= 3) then
                error stop 'tensors can only be 3x3'
            else
                allocate(me%r_tensor(1:ubound(real3d,dim=1)))
                do i = 1, ubound(real3d,dim=1)
                    me%r_tensor(i)%val(1:3,1:3) = real3d(i,1:3,1:3)
                end do
                me%nvals = size(me%r_tensor,dim=1)
            end if
        else
            error stop 'error: must provide either int3d or real3d in tensor_setup'
        end if

    end procedure tensor_setup

    module procedure tensor_check_for_diffs
        implicit none
        !! author: Ian Porter
        !! date: 12/14/2017
        !!
        !! function checks for differences in a tensor attribute
        !!
        integer(i4k) :: i, j, k

        diffs = .false.
        if (.not. same_type_as(me,you)) then
            diffs = .true.
        else
            select type (you)
            class is (tensor)
                if (me%dataname /= you%dataname)      then
                    diffs = .true.
                else if (me%datatype /= you%datatype) then
                    diffs = .true.
                else if (me%nvals /= you%nvals)       then
                    diffs = .true.
                else if (allocated(me%i_tensor))      then
                    do i = 1, ubound(me%i_tensor,dim=1)
                        do j = 1, ubound(me%i_tensor(i)%val,dim=1)
                            do k = 1, ubound(me%i_tensor(i)%val,dim=2)
                                if (me%i_tensor(i)%val(j,k) /= you%i_tensor(i)%val(j,k)) then
                                    diffs = .true.
                                end if
                            end do
                        end do
                    end do
                else if (allocated(me%r_tensor))      then
                    do i = 1, ubound(me%r_tensor,dim=1)
                        do j = 1, ubound(me%r_tensor(i)%val,dim=1)
                            do k = 1, ubound(me%r_tensor(i)%val,dim=2)
                                if (me%r_tensor(i)%val(j,k) /= you%r_tensor(i)%val(j,k))     then
                                    diffs = .true.
                                end if
                            end do
                        end do
                    end do
                else
                    diffs = .true.
                end if
            end select
        end if

    end procedure tensor_check_for_diffs
    !********
    ! fields
    !********
    module procedure field_read
        use misc, only : interpret_string
        implicit none
        !! author: Ian Porter
        !! date: 12/14/2017
        !!
        !! subroutine performs the read for a field attribute
        !!
        integer(i4k)              :: i, j, iostat, dim
        logical                   :: end_of_file
        character(len=def_len)    :: line
        character(*), parameter   :: real_char = 'r'
        real(r8k),        dimension(:), allocatable :: reals
        integer(i4k),     dimension(:), allocatable :: ints
        type(char_dt),    dimension(:), allocatable :: chars
        character(len=1), dimension(:), allocatable :: datatype

        read(unit,100) line
        call interpret_string (line=line, datatype=[ 'c','i' ], ignore='field ', separator=' ', &
        &                    ints=ints, chars=chars)
        me%dataname = trim(chars(1)%text); dim = ints(1)
        if (allocated(chars)) deallocate(chars)

        allocate(me%array(1:dim)); end_of_file = .false.; i = 0

        get_fields: do
            read(unit,100,iostat=iostat) line
            end_of_file = (iostat < 0)
            if (end_of_file) then
                exit get_fields
            else if (trim(line) == '') then
                cycle      !! skip blank lines
            else
                i = i + 1

                call interpret_string (line=line, datatype=[ 'c','i','i','c' ], separator=' ', chars=chars, ints=ints)
                me%array(i)%name = trim(chars(1)%text); me%array(i)%numcomponents = ints(1)
                me%array(i)%numtuples = ints(2); me%array(i)%datatype = trim(chars(2)%text)
                allocate(datatype(1:me%array(i)%numcomponents),source=real_char)
                allocate(me%array(i)%data(1:me%array(i)%numtuples,1:me%array(i)%numcomponents),source=0.0_r8k)

                do j = 1, me%array(i)%numtuples
                    read(unit,100,iostat=iostat) line
                    call interpret_string (line=line, datatype=datatype, separator=' ', reals=reals)
                    me%array(i)%data(j,:) = reals(:)
                end do
                if (allocated(datatype)) deallocate(datatype)

            end if
        end do get_fields

        if (allocated(me%array)) then
        me%nvals = size(me%array, dim=1)
        end if

100     format((a))
    end procedure field_read

    module procedure field_write
        implicit none
        !! author: Ian Porter
        !! date: 12/13/2017
        !!
        !! subroutine performs the write for a field attribute
        !!
        integer(i4k) :: i, j

        write(unit,100) me%dataname, size(me%array,dim=1)
        do i = 1, size(me%array,dim=1)
            write(unit,101) me%array(i)%name, me%array(i)%numcomponents, me%array(i)%numtuples, me%array(i)%datatype
            do j = 1, me%array(i)%numtuples
                write(unit,102) me%array(i)%data(j,:)
            end do
            write(unit,103)
        end do

100     format('field ',(a),' ',(i0))
101     format((a),' ',(i0),' ',(i0),' ',(a))
102     format(*(es13.6,' '))
103     format()
    end procedure field_write

    module procedure field_setup
        implicit none
        !! author: Ian Porter
        !! date: 12/14/2017
        !!
        !! subroutine performs the set-up for a field attribute
        !!
        me%dataname = dataname
        if (present(datatype)) then
            me%datatype = datatype
        else
            me%datatype = 'double'
        end if
        me%array = field_arrays
        me%nvals = size(me%array,dim=1)

    end procedure field_setup

    module procedure field_check_for_diffs
        implicit none
        !! author: Ian Porter
        !! date: 12/14/2017
        !!
        !! function checks for differences in a field attribute
        !!
        integer(i4k) :: i, j, k

        diffs = .false.
        if (.not. same_type_as(me,you)) then
            diffs = .true.
        else
            select type (you)
            class is (field)
                if      (me%dataname /= you%dataname) then
                    diffs = .true.
                else if (me%nvals /= you%nvals)       then
                    diffs = .true.
                else
                    do i = 1, ubound(me%array,dim=1)
                        if      (me%array(i)%name          /= you%array(i)%name         ) then
                            diffs = .true.
                        else if (me%array(i)%numcomponents /= you%array(i)%numcomponents) then
                            diffs = .true.
                        else if (me%array(i)%numtuples     /= you%array(i)%numtuples    ) then
                            diffs = .true.
                        else if (me%array(i)%datatype      /= you%array(i)%datatype     ) then
                            diffs = .true.
                        else
                            do j = 1, ubound(me%array(i)%data,dim=1)
                                do k = 1, ubound(me%array(i)%data,dim=2)
                                    if (me%array(i)%data(j,k) /= me%array(i)%data(j,k)) then
                                        diffs = .true.
                                    end if
                                end do
                            end do
                        end if
                    end do
                end if
            end select
        end if

    end procedure field_check_for_diffs

end submodule vtk_attributes_procedures
