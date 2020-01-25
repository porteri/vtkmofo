module vtk_attributes
    use precision, only : i4k, r8k
    use vtk_dataarray_element, only : dataarray_dt
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
    private
    public :: attribute, attributes, scalar, vector, normal, texture, tensor, field, field_data_array

    character(len=*), parameter :: default = 'default'     !! default table name

    type, abstract :: attribute
        !! abstract dt of attribute information
        integer(i4k) :: nvals = 0_i4k
        character(len=:), allocatable :: dataname
        character(len=:), allocatable :: datatype
    contains
        procedure(abs_read),  deferred, public :: read
        procedure(abs_write), deferred, public :: write
        procedure, non_overridable,     public :: init => initialize  !! initialize the attribute
        procedure, private :: check_for_diffs
        generic :: operator(.diff.) => check_for_diffs
        procedure :: convert_to_dataarray
        !procedure(abs_get_name), deferred, public :: get_name
    end type attribute

    type, extends(attribute) :: scalar
        !! scalar attribute dt
        integer(i4k) :: numcomp = 0
        character(len=:), allocatable :: tablename
        integer(i4k), dimension(:), allocatable :: ints
        real(r8k),    dimension(:), allocatable :: reals
    contains
        procedure :: read  => scalar_read
        procedure :: write => scalar_write
        procedure :: setup => scalar_setup
        procedure, private :: check_for_diffs => scalar_check_for_diffs
        procedure :: convert_to_dataarray => scalar_convert_to_dataarray
    end type scalar

    type, extends(attribute) :: vector
        !! vector attribute dt
        integer(i4k), dimension(:,:), allocatable :: i_vector
        real(r8k),    dimension(:,:), allocatable :: r_vector
    contains
        procedure :: read  => vector_read
        procedure :: write => vector_write
        procedure :: setup => vector_setup
        procedure, private :: check_for_diffs => vector_check_for_diffs
        procedure :: convert_to_dataarray => vector_convert_to_dataarray
    end type vector

    type, extends(attribute) :: normal
        !! normal attribute dt
        integer(i4k), dimension(:,:), allocatable :: i_normal
        real(r8k),    dimension(:,:), allocatable :: r_normal
    contains
        procedure :: read  => normal_read
        procedure :: write => normal_write
        procedure :: setup => normal_setup
        procedure, private :: check_for_diffs => normal_check_for_diffs
        procedure :: convert_to_dataarray => normal_convert_to_dataarray
    end type normal

    type, extends(attribute) :: texture
        !! texture attribute dt
        integer(i4k), dimension(:,:), allocatable :: i_texture
        real(r8k),    dimension(:,:), allocatable :: r_texture
    contains
        procedure :: read  => texture_read
        procedure :: write => texture_write
        procedure :: setup => texture_setup
        procedure, private :: check_for_diffs => texture_check_for_diffs
    end type texture

    type :: i_tensor_array
        !! tensor integer data dt
        integer(i4k), dimension(3,3) :: val = 0_i4k
    end type i_tensor_array

    type :: r_tensor_array
        !! tensor real data dt
        real(r8k),    dimension(3,3) :: val = 0.0_r8k
    end type r_tensor_array

    type, extends(attribute) :: tensor
        !! tensor attribute dt
        type(i_tensor_array), dimension(:), allocatable :: i_tensor
        type(r_tensor_array), dimension(:), allocatable :: r_tensor
    contains
        procedure :: read  => tensor_read
        procedure :: write => tensor_write
        procedure :: setup => tensor_setup
        procedure, private :: check_for_diffs => tensor_check_for_diffs
        procedure :: convert_to_dataarray => tensor_convert_to_dataarray
    end type tensor

    type :: field_data_array
        !! field data dt
        character(len=:), allocatable :: name
        integer(i4k) :: numcomponents = 0
        integer(i4k) :: numtuples     = 0
        character(len=:), allocatable :: datatype
        real(r8k), dimension(:,:), allocatable :: data
    end type field_data_array

    type, extends(attribute) :: field
        !! field attribute dt
        type(field_data_array), dimension(:), allocatable :: array
    contains
        procedure :: read  => field_read
        procedure :: write => field_write
        procedure :: setup => field_setup
        procedure, private :: check_for_diffs => field_check_for_diffs
    end type field

    type :: attributes
        class(attribute), allocatable :: attribute
    end type attributes

    interface

        module subroutine abs_read (me, unit)
            implicit none
            !! author: Ian Porter
            !! date: 12/13/2017
            !!
            !! abstract for reading an attribute
            !!
            class(attribute), intent(out) :: me
            integer(i4k),     intent(in)  :: unit

        end subroutine abs_read

        module subroutine abs_write (me, unit)
            implicit none
            !! author: Ian Porter
            !! date: 12/13/2017
            !!
            !! abstract for writing an attribute
            !!
            class(attribute), intent(in) :: me
            integer(i4k),     intent(in) :: unit

        end subroutine abs_write

        module subroutine initialize (me, dataname, datatype, numcomp, tablename, int1d, int2d, int3d, &
            &                         real1d, real2d, real3d, field_arrays)
            implicit none
            !! author: Ian Porter
            !! date: 12/13/2017
            !!
            !! abstract for performing the set-up of an attribute
            !!
            class(attribute), intent(out) :: me
            character(len=*), intent(in)  :: dataname
            integer(i4k),     intent(in), optional :: numcomp
            character(len=*), intent(in), optional :: datatype, tablename
            integer(i4k), dimension(:),     intent(in), optional :: int1d
            integer(i4k), dimension(:,:),   intent(in), optional :: int2d
            integer(i4k), dimension(:,:,:), intent(in), optional :: int3d
            real(r8k),    dimension(:),     intent(in), optional :: real1d
            real(r8k),    dimension(:,:),   intent(in), optional :: real2d
            real(r8k),    dimension(:,:,:), intent(in), optional :: real3d
            type(field_data_array), dimension(:), intent(in), optional :: field_arrays

        end subroutine initialize

        module function check_for_diffs (me, you) result (diffs)
            implicit none
            !! author: Ian Porter
            !! date: 12/13/2017
            !!
            !! function checks for differences in an attribute
            !!
            class(attribute), intent(in) :: me, you
            logical                      :: diffs

        end function check_for_diffs

        module function convert_to_dataarray (me) result (array)
            implicit none
            !! author: Ian Porter
            !! date: 07/20/2019
            !!
            !! function converts an attribute to a dataarray
            !!
            class(attribute), intent(in) :: me
            type(dataarray_dt)           :: array

        end function convert_to_dataarray
        !********
        ! scalars
        !********
        module subroutine scalar_read (me, unit)
            implicit none
            !! author: Ian Porter
            !! date: 12/13/2017
            !!
            !! subroutine performs the read for a scalar attribute
            !!
            class(scalar), intent(out) :: me
            integer(i4k),  intent(in)  :: unit

        end subroutine scalar_read

        module subroutine scalar_write (me, unit)
            implicit none
            !! author: Ian Porter
            !! date: 12/13/2017
            !!
            !! subroutine performs the write for a scalar attribute
            !!
            class(scalar), intent(in) :: me
            integer(i4k),  intent(in) :: unit

        end subroutine scalar_write

        module subroutine scalar_setup (me, dataname, datatype, numcomp, tablename, int1d, real1d)
            implicit none
            !! author: Ian Porter
            !! date: 12/13/2017
            !!
            !! subroutine performs the set-up for a scalar attribute
            !!
            class(scalar),    intent(out) :: me
            character(len=*), intent(in)  :: dataname
            integer(i4k),     intent(in), optional :: numcomp
            character(len=*), intent(in), optional :: datatype
            character(len=*), intent(in), optional :: tablename
            integer(i4k), dimension(:), intent(in), optional :: int1d
            real(r8k),    dimension(:), intent(in), optional :: real1d

        end subroutine scalar_setup

        module function scalar_check_for_diffs (me, you) result (diffs)
            implicit none
            !! author: Ian Porter
            !! date: 12/13/2017
            !!
            !! function checks for differences in a scalar attribute
            !!
            class(scalar),    intent(in) :: me
            class(attribute), intent(in) :: you
            logical                      :: diffs

        end function scalar_check_for_diffs

        module function scalar_convert_to_dataarray (me) result (array)
            implicit none
            !! author: Ian Porter
            !! date: 07/20/2019
            !!
            !! function converts a scalar attribute to a dataarray
            !!
            class(scalar), intent(in) :: me
            type(dataarray_dt)        :: array

        end function scalar_convert_to_dataarray
        !********
        ! vectors
        !********
        module subroutine vector_read (me, unit)
            implicit none
            !! author: Ian Porter
            !! date: 12/14/2017
            !!
            !! subroutine performs the read for a vector attribute
            !!
            class(vector), intent(out) :: me
            integer(i4k),  intent(in)  :: unit

        end subroutine vector_read

        module subroutine vector_write (me, unit)
            implicit none
            !! author: Ian Porter
            !! date: 12/13/2017
            !!
            !! subroutine performs the write for a vector attribute
            !!
            class(vector), intent(in) :: me
            integer(i4k),  intent(in) :: unit

        end subroutine vector_write

        module subroutine vector_setup (me, dataname, datatype, int2d, real2d)
            implicit none
            !! author: Ian Porter
            !! date: 12/14/2017
            !!
            !! subroutine performs the set-up for a vector attribute
            !!
            class(vector),    intent(out) :: me
            character(len=*), intent(in)  :: dataname
            character(len=*), intent(in), optional :: datatype
            integer(i4k), dimension(:,:), intent(in), optional :: int2d
            real(r8k),    dimension(:,:), intent(in), optional :: real2d

        end subroutine vector_setup

        module function vector_check_for_diffs (me, you) result (diffs)
            implicit none
            !! author: Ian Porter
            !! date: 12/14/2017
            !!
            !! function checks for differences in a vector attribute
            !!
            class(vector),    intent(in) :: me
            class(attribute), intent(in) :: you
            logical                      :: diffs

        end function vector_check_for_diffs

        module function vector_convert_to_dataarray (me) result (array)
            implicit none
            !! author: Ian Porter
            !! date: 01/05/2020
            !!
            !! function converts a vector attribute to a dataarray
            !!
            class(vector), intent(in) :: me
            type(dataarray_dt)        :: array

        end function vector_convert_to_dataarray
        !********
        ! normals
        !********
        module subroutine normal_read (me, unit)
            implicit none
            !! author: Ian Porter
            !! date: 12/14/2017
            !!
            !! subroutine performs the read for a normal attribute
            !!
            class(normal), intent(out) :: me
            integer(i4k),  intent(in)  :: unit

        end subroutine normal_read

        module subroutine normal_write (me, unit)
            implicit none
            !! author: Ian Porter
            !! date: 12/13/2017
            !!
            !! subroutine performs the write for a normal attribute
            !!
            class(normal), intent(in) :: me
            integer(i4k),  intent(in) :: unit

        end subroutine normal_write

        module subroutine normal_setup (me, dataname, datatype, int2d, real2d)
            implicit none
            !! author: Ian Porter
            !! date: 12/14/2017
            !!
            !! subroutine performs the set-up for a normal attribute
            !!
            class(normal),    intent(out) :: me
            character(len=*), intent(in)  :: dataname
            character(len=*), intent(in), optional :: datatype
            integer(i4k), dimension(:,:), intent(in), optional :: int2d
            real(r8k),    dimension(:,:), intent(in), optional :: real2d

        end subroutine normal_setup

        module function normal_check_for_diffs (me, you) result (diffs)
            implicit none
            !! author: Ian Porter
            !! date: 12/14/2017
            !!
            !! function checks for differences in a normal attribute
            !!
            class(normal),    intent(in) :: me
            class(attribute), intent(in) :: you
            logical                      :: diffs

        end function normal_check_for_diffs

        module function normal_convert_to_dataarray (me) result (array)
            implicit none
            !! author: Ian Porter
            !! date: 01/05/2020
            !!
            !! function converts a normal attribute to a dataarray
            !!
            class(normal), intent(in) :: me
            type(dataarray_dt)        :: array

        end function normal_convert_to_dataarray
        !********
        ! textures
        !********
        module subroutine texture_read (me, unit)
            implicit none
            !! author: Ian Porter
            !! date: 12/14/2017
            !!
            !! subroutine performs the read for a texture attribute
            !!
            class(texture), intent(out) :: me
            integer(i4k),   intent(in)  :: unit

        end subroutine texture_read

        module subroutine texture_write (me, unit)
            implicit none
            !! author: Ian Porter
            !! date: 12/13/2017
            !!
            !! subroutine performs the write for a texture attribute
            !!
            class(texture), intent(in) :: me
            integer(i4k),   intent(in) :: unit

        end subroutine texture_write

        module subroutine texture_setup (me, dataname, datatype, int2d, real2d)
            implicit none
            !! author: Ian Porter
            !! date: 12/14/2017
            !!
            !! subroutine performs the set-up for a texture attribute
            !!
            class(texture),   intent(out) :: me
            character(len=*), intent(in)  :: dataname
            character(len=*), intent(in), optional :: datatype
            integer(i4k), dimension(:,:), intent(in), optional :: int2d
            real(r8k),    dimension(:,:), intent(in), optional :: real2d

        end subroutine texture_setup

        module function texture_check_for_diffs (me, you) result (diffs)
            implicit none
            !! author: Ian Porter
            !! date: 12/14/2017
            !!
            !! function checks for differences in a texture attribute
            !!
            class(texture),   intent(in) :: me
            class(attribute), intent(in) :: you
            logical                      :: diffs

        end function texture_check_for_diffs
        !********
        ! tensors
        !********
        module subroutine tensor_read (me, unit)
            implicit none
            !! author: Ian Porter
            !! date: 12/14/2017
            !!
            !! subroutine performs the read for a tensor attribute
            !!
            class(tensor), intent(out) :: me
            integer(i4k),  intent(in)  :: unit

        end subroutine tensor_read

        module subroutine tensor_write (me, unit)
            implicit none
            !! author: Ian Porter
            !! date: 12/13/2017
            !!
            !! subroutine performs the write for a tensor attribute
            !!
            class(tensor), intent(in) :: me
            integer(i4k),  intent(in) :: unit

        end subroutine tensor_write

        module subroutine tensor_setup (me, dataname, datatype, int3d, real3d)
            implicit none
            !! author: Ian Porter
            !! date: 12/14/2017
            !!
            !! subroutine performs the set-up for a tensor attribute
            !!
            class(tensor),    intent(out) :: me
            character(len=*), intent(in)  :: dataname
            character(len=*), intent(in), optional :: datatype
            integer(i4k), dimension(:,:,:), intent(in), optional :: int3d
            real(r8k),    dimension(:,:,:), intent(in), optional :: real3d

        end subroutine tensor_setup

        module function tensor_check_for_diffs (me, you) result (diffs)
            implicit none
            !! author: Ian Porter
            !! date: 12/14/2017
            !!
            !! function checks for differences in a tensor attribute
            !!
            class(tensor),    intent(in) :: me
            class(attribute), intent(in) :: you
            logical                      :: diffs

        end function tensor_check_for_diffs

        module function tensor_convert_to_dataarray (me) result (array)
            implicit none
            !! author: Ian Porter
            !! date: 01/05/2020
            !!
            !! function converts a tensor attribute to a dataarray
            !!
            class(tensor), intent(in) :: me
            type(dataarray_dt)        :: array

        end function tensor_convert_to_dataarray
        !********
        ! fields
        !********
        module subroutine field_read (me, unit)
            implicit none
            !! author: Ian Porter
            !! date: 12/14/2017
            !!
            !! subroutine performs the read for a field attribute
            !!
            class(field), intent(out) :: me
            integer(i4k), intent(in)  :: unit

        end subroutine field_read

        module subroutine field_write (me, unit)
            implicit none
            !! author: Ian Porter
            !! date: 12/13/2017
            !!
            !! subroutine performs the write for a field attribute
            !!
            class(field), intent(in) :: me
            integer(i4k), intent(in) :: unit

        end subroutine field_write

        module subroutine field_setup (me, dataname, datatype, field_arrays)
            implicit none
            !! author: Ian Porter
            !! date: 12/14/2017
            !!
            !! subroutine performs the set-up for a field attribute
            !!
            class(field),     intent(out) :: me
            character(len=*), intent(in)  :: dataname
            character(len=*), intent(in), optional :: datatype
            type(field_data_array), dimension(:), intent(in) :: field_arrays

        end subroutine field_setup

        module function field_check_for_diffs (me, you) result (diffs)
            implicit none
            !! author: Ian Porter
            !! date: 12/14/2017
            !!
            !! function checks for differences in a field attribute
            !!
            class(field),     intent(in) :: me
            class(attribute), intent(in) :: you
            logical                      :: diffs

        end function field_check_for_diffs

    end interface

end module vtk_attributes
