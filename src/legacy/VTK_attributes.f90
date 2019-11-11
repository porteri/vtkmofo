MODULE vtk_attributes
    USE Precision, ONLY : i4k, r8k
    USE VTK_DataArray_element, ONLY : DataArray_dt
    IMPLICIT NONE
    !! author: Ian Porter
    !! date: 12/13/2017
    !!
    !! This module contains the dataset attributes for vtk format
    !!
    !! The following dataset attributes are available:
    !! 1) scalars
    !! 2) vectors
    !! 3) normals
    !! 4) texture coordinates (1D, 2D & 3D)
    !! 5) 3x3 tensors
    !! 6) field data
    !!
    PRIVATE
    PUBLIC :: attribute, attributes, scalar, vector, normal, texture, tensor, field, field_data_array

    CHARACTER(LEN=*), PARAMETER :: default = 'default'     !! Default table name

    TYPE, ABSTRACT :: attribute
        !! Abstract DT of attribute information
        INTEGER(i4k) :: nvals = 0_i4k
        CHARACTER(LEN=:), ALLOCATABLE :: dataname
        CHARACTER(LEN=:), ALLOCATABLE :: datatype
    CONTAINS
        PROCEDURE(abs_read),  DEFERRED, PUBLIC :: read
        PROCEDURE(abs_write), DEFERRED, PUBLIC :: write
        PROCEDURE, NON_OVERRIDABLE,     PUBLIC :: init => initialize  !! Initialize the attribute
        PROCEDURE, PRIVATE :: check_for_diffs
        GENERIC :: OPERATOR(.diff.) => check_for_diffs
        PROCEDURE :: convert_to_dataarray
        !PROCEDURE(abs_get_name), DEFERRED, PUBLIC :: get_name
    END TYPE attribute

    TYPE, EXTENDS(attribute) :: scalar
        !! Scalar attribute DT
        INTEGER(i4k) :: numcomp = 0
        CHARACTER(LEN=:), ALLOCATABLE :: tablename
        INTEGER(i4k), DIMENSION(:), ALLOCATABLE :: ints
        REAL(r8k),    DIMENSION(:), ALLOCATABLE :: reals
    CONTAINS
        PROCEDURE :: read  => scalar_read
        PROCEDURE :: write => scalar_write
        PROCEDURE :: setup => scalar_setup
        PROCEDURE, PRIVATE :: check_for_diffs => scalar_check_for_diffs
        PROCEDURE :: convert_to_dataarray => scalar_convert_to_dataarray
    END TYPE scalar

    TYPE, EXTENDS(attribute) :: vector
        !! Vector attribute DT
        INTEGER(i4k), DIMENSION(:,:), ALLOCATABLE :: i_vector
        REAL(r8k),    DIMENSION(:,:), ALLOCATABLE :: r_vector
    CONTAINS
        PROCEDURE :: read  => vector_read
        PROCEDURE :: write => vector_write
        PROCEDURE :: setup => vector_setup
        PROCEDURE, PRIVATE :: check_for_diffs => vector_check_for_diffs
    END TYPE vector

    TYPE, EXTENDS(attribute) :: normal
        !! Normal attribute DT
        INTEGER(i4k), DIMENSION(:,:), ALLOCATABLE :: i_normal
        REAL(r8k),    DIMENSION(:,:), ALLOCATABLE :: r_normal
    CONTAINS
        PROCEDURE :: read  => normal_read
        PROCEDURE :: write => normal_write
        PROCEDURE :: setup => normal_setup
        PROCEDURE, PRIVATE :: check_for_diffs => normal_check_for_diffs
    END TYPE normal

    TYPE, EXTENDS(attribute) :: texture
        !! Texture attribute DT
        INTEGER(i4k), DIMENSION(:,:), ALLOCATABLE :: i_texture
        REAL(r8k),    DIMENSION(:,:), ALLOCATABLE :: r_texture
    CONTAINS
        PROCEDURE :: read  => texture_read
        PROCEDURE :: write => texture_write
        PROCEDURE :: setup => texture_setup
        PROCEDURE, PRIVATE :: check_for_diffs => texture_check_for_diffs
    END TYPE texture

    TYPE :: i_tensor_array
        !! Tensor integer data DT
        INTEGER(i4k), DIMENSION(3,3) :: val = 0_i4k
    END TYPE i_tensor_array

    TYPE :: r_tensor_array
        !! Tensor real data DT
        REAL(r8k),    DIMENSION(3,3) :: val = 0.0_r8k
    END TYPE r_tensor_array

    TYPE, EXTENDS(attribute) :: tensor
        !! Tensor attribute DT
        TYPE(i_tensor_array), DIMENSION(:), ALLOCATABLE :: i_tensor
        TYPE(r_tensor_array), DIMENSION(:), ALLOCATABLE :: r_tensor
    CONTAINS
        PROCEDURE :: read  => tensor_read
        PROCEDURE :: write => tensor_write
        PROCEDURE :: setup => tensor_setup
        PROCEDURE, PRIVATE :: check_for_diffs => tensor_check_for_diffs
    END TYPE tensor

    TYPE :: field_data_array
        !! Field data DT
        CHARACTER(LEN=:), ALLOCATABLE :: name
        INTEGER(i4k) :: numComponents = 0
        INTEGER(i4k) :: numTuples     = 0
        CHARACTER(LEN=:), ALLOCATABLE :: datatype
        REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: data
    END TYPE field_data_array

    TYPE, EXTENDS(attribute) :: field
        !! Field attribute DT
        TYPE(field_data_array), DIMENSION(:), ALLOCATABLE :: array
    CONTAINS
        PROCEDURE :: read  => field_read
        PROCEDURE :: write => field_write
        PROCEDURE :: setup => field_setup
        PROCEDURE, PRIVATE :: check_for_diffs => field_check_for_diffs
    END TYPE field

    TYPE :: attributes
        CLASS(attribute), ALLOCATABLE :: attribute
    END TYPE attributes

    INTERFACE

        MODULE SUBROUTINE abs_read (me, unit)
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 12/13/2017
        !!
        !! Abstract for reading an attribute
        !!
        CLASS(attribute), INTENT(OUT) :: me
        INTEGER(i4k),     INTENT(IN)  :: unit

        END SUBROUTINE abs_read

        MODULE SUBROUTINE abs_write (me, unit)
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 12/13/2017
        !!
        !! Abstract for writing an attribute
        !!
        CLASS(attribute), INTENT(IN) :: me
        INTEGER(i4k),     INTENT(IN) :: unit

        END SUBROUTINE abs_write

        MODULE SUBROUTINE initialize (me, dataname, datatype, numcomp, tablename, int1d, int2d, int3d, &
          &                           real1d, real2d, real3d, field_arrays)
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 12/13/2017
        !!
        !! Abstract for performing the set-up of an attribute
        !!
        CLASS(attribute), INTENT(OUT) :: me
        CHARACTER(LEN=*), INTENT(IN)  :: dataname
        INTEGER(i4k),     INTENT(IN), OPTIONAL :: numcomp
        CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: datatype, tablename
        INTEGER(i4k), DIMENSION(:),     INTENT(IN), OPTIONAL :: int1d
        INTEGER(i4k), DIMENSION(:,:),   INTENT(IN), OPTIONAL :: int2d
        INTEGER(i4k), DIMENSION(:,:,:), INTENT(IN), OPTIONAL :: int3d
        REAL(r8k),    DIMENSION(:),     INTENT(IN), OPTIONAL :: real1d
        REAL(r8k),    DIMENSION(:,:),   INTENT(IN), OPTIONAL :: real2d
        REAL(r8k),    DIMENSION(:,:,:), INTENT(IN), OPTIONAL :: real3d
        TYPE(field_data_array), DIMENSION(:), INTENT(IN), OPTIONAL :: field_arrays

        END SUBROUTINE initialize

        MODULE FUNCTION check_for_diffs (me, you) RESULT (diffs)
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 12/13/2017
        !!
        !! Function checks for differences in an attribute
        !!
        CLASS(attribute), INTENT(IN) :: me, you
        LOGICAL                      :: diffs

        END FUNCTION check_for_diffs

        MODULE FUNCTION convert_to_dataarray (me) RESULT (array)
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 07/20/2019
        !!
        !! Function converts an attribute to a dataarray
        !!
        CLASS(attribute), INTENT(IN) :: me
        TYPE(DataArray_dt)           :: array

        END FUNCTION convert_to_dataarray
!********
! Scalars
!********
        MODULE SUBROUTINE scalar_read (me, unit)
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 12/13/2017
        !!
        !! Subroutine performs the read for a scalar attribute
        !!
        CLASS(scalar), INTENT(OUT) :: me
        INTEGER(i4k),  INTENT(IN)  :: unit

        END SUBROUTINE scalar_read

        MODULE SUBROUTINE scalar_write (me, unit)
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 12/13/2017
        !!
        !! Subroutine performs the write for a scalar attribute
        !!
        CLASS(scalar), INTENT(IN) :: me
        INTEGER(i4k),  INTENT(IN) :: unit

        END SUBROUTINE scalar_write

        MODULE SUBROUTINE scalar_setup (me, dataname, datatype, numcomp, tablename, int1d, real1d)
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 12/13/2017
        !!
        !! Subroutine performs the set-up for a scalar attribute
        !!
        CLASS(scalar),    INTENT(OUT) :: me
        CHARACTER(LEN=*), INTENT(IN)  :: dataname
        INTEGER(i4k),     INTENT(IN), OPTIONAL :: numcomp
        CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: datatype
        CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: tablename
        INTEGER(i4k), DIMENSION(:), INTENT(IN), OPTIONAL :: int1d
        REAL(r8k),    DIMENSION(:), INTENT(IN), OPTIONAL :: real1d

        END SUBROUTINE scalar_setup

        MODULE FUNCTION scalar_check_for_diffs (me, you) RESULT (diffs)
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 12/13/2017
        !!
        !! Function checks for differences in a scalar attribute
        !!
        CLASS(scalar),    INTENT(IN) :: me
        CLASS(attribute), INTENT(IN) :: you
        LOGICAL                      :: diffs

        END FUNCTION scalar_check_for_diffs

        MODULE FUNCTION scalar_convert_to_dataarray (me) RESULT (array)
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 07/20/2019
        !!
        !! Function converts an attribute to a dataarray
        !!
        CLASS(scalar), INTENT(IN) :: me
        TYPE(DataArray_dt)        :: array

        END FUNCTION scalar_convert_to_dataarray
!********
! Vectors
!********
        MODULE SUBROUTINE vector_read (me, unit)
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 12/14/2017
        !!
        !! Subroutine performs the read for a vector attribute
        !!
        CLASS(vector), INTENT(OUT) :: me
        INTEGER(i4k),  INTENT(IN)  :: unit

        END SUBROUTINE vector_read

        MODULE SUBROUTINE vector_write (me, unit)
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 12/13/2017
        !!
        !! Subroutine performs the write for a vector attribute
        !!
        CLASS(vector), INTENT(IN) :: me
        INTEGER(i4k),  INTENT(IN) :: unit

        END SUBROUTINE vector_write

        MODULE SUBROUTINE vector_setup (me, dataname, datatype, int2d, real2d)
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 12/14/2017
        !!
        !! Subroutine performs the set-up for a vector attribute
        !!
        CLASS(vector),    INTENT(OUT) :: me
        CHARACTER(LEN=*), INTENT(IN)  :: dataname
        CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: datatype
        INTEGER(i4k), DIMENSION(:,:), INTENT(IN), OPTIONAL :: int2d
        REAL(r8k),    DIMENSION(:,:), INTENT(IN), OPTIONAL :: real2d

        END SUBROUTINE vector_setup

        MODULE FUNCTION vector_check_for_diffs (me, you) RESULT (diffs)
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 12/14/2017
        !!
        !! Function checks for differences in a vector attribute
        !!
        CLASS(vector),    INTENT(IN) :: me
        CLASS(attribute), INTENT(IN) :: you
        LOGICAL                      :: diffs

        END FUNCTION vector_check_for_diffs
!********
! Normals
!********
        MODULE SUBROUTINE normal_read (me, unit)
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 12/14/2017
        !!
        !! Subroutine performs the read for a normal attribute
        !!
        CLASS(normal), INTENT(OUT) :: me
        INTEGER(i4k),  INTENT(IN)  :: unit

        END SUBROUTINE normal_read

        MODULE SUBROUTINE normal_write (me, unit)
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 12/13/2017
        !!
        !! Subroutine performs the write for a normal attribute
        !!
        CLASS(normal), INTENT(IN) :: me
        INTEGER(i4k),  INTENT(IN) :: unit

        END SUBROUTINE normal_write

        MODULE SUBROUTINE normal_setup (me, dataname, datatype, int2d, real2d)
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 12/14/2017
        !!
        !! Subroutine performs the set-up for a normal attribute
        !!
        CLASS(normal),    INTENT(OUT) :: me
        CHARACTER(LEN=*), INTENT(IN)  :: dataname
        CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: datatype
        INTEGER(i4k), DIMENSION(:,:), INTENT(IN), OPTIONAL :: int2d
        REAL(r8k),    DIMENSION(:,:), INTENT(IN), OPTIONAL :: real2d

        END SUBROUTINE normal_setup

        MODULE FUNCTION normal_check_for_diffs (me, you) RESULT (diffs)
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 12/14/2017
        !!
        !! Function checks for differences in a normal attribute
        !!
        CLASS(normal),    INTENT(IN) :: me
        CLASS(attribute), INTENT(IN) :: you
        LOGICAL                      :: diffs

        END FUNCTION normal_check_for_diffs
!********
! Textures
!********
        MODULE SUBROUTINE texture_read (me, unit)
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 12/14/2017
        !!
        !! Subroutine performs the read for a texture attribute
        !!
        CLASS(texture), INTENT(OUT) :: me
        INTEGER(i4k),   INTENT(IN)  :: unit

        END SUBROUTINE texture_read

        MODULE SUBROUTINE texture_write (me, unit)
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 12/13/2017
        !!
        !! Subroutine performs the write for a texture attribute
        !!
        CLASS(texture), INTENT(IN) :: me
        INTEGER(i4k),   INTENT(IN) :: unit

        END SUBROUTINE texture_write

        MODULE SUBROUTINE texture_setup (me, dataname, datatype, int2d, real2d)
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 12/14/2017
        !!
        !! Subroutine performs the set-up for a texture attribute
        !!
        CLASS(texture),   INTENT(OUT) :: me
        CHARACTER(LEN=*), INTENT(IN)  :: dataname
        CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: datatype
        INTEGER(i4k), DIMENSION(:,:), INTENT(IN), OPTIONAL :: int2d
        REAL(r8k),    DIMENSION(:,:), INTENT(IN), OPTIONAL :: real2d

        END SUBROUTINE texture_setup

        MODULE FUNCTION texture_check_for_diffs (me, you) RESULT (diffs)
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 12/14/2017
        !!
        !! Function checks for differences in a texture attribute
        !!
        CLASS(texture),   INTENT(IN) :: me
        CLASS(attribute), INTENT(IN) :: you
        LOGICAL                      :: diffs

        END FUNCTION texture_check_for_diffs
!********
! Tensors
!********
        MODULE SUBROUTINE tensor_read (me, unit)
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 12/14/2017
        !!
        !! Subroutine performs the read for a tensor attribute
        !!
        CLASS(tensor), INTENT(OUT) :: me
        INTEGER(i4k),  INTENT(IN)  :: unit

        END SUBROUTINE tensor_read

        MODULE SUBROUTINE tensor_write (me, unit)
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 12/13/2017
        !!
        !! Subroutine performs the write for a tensor attribute
        !!
        CLASS(tensor), INTENT(IN) :: me
        INTEGER(i4k),  INTENT(IN) :: unit

        END SUBROUTINE tensor_write

        MODULE SUBROUTINE tensor_setup (me, dataname, datatype, int3d, real3d)
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 12/14/2017
        !!
        !! Subroutine performs the set-up for a tensor attribute
        !!
        CLASS(tensor),    INTENT(OUT) :: me
        CHARACTER(LEN=*), INTENT(IN)  :: dataname
        CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: datatype
        INTEGER(i4k), DIMENSION(:,:,:), INTENT(IN), OPTIONAL :: int3d
        REAL(r8k),    DIMENSION(:,:,:), INTENT(IN), OPTIONAL :: real3d

        END SUBROUTINE tensor_setup

        MODULE FUNCTION tensor_check_for_diffs (me, you) RESULT (diffs)
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 12/14/2017
        !!
        !! Function checks for differences in a tensor attribute
        !!
        CLASS(tensor),    INTENT(IN) :: me
        CLASS(attribute), INTENT(IN) :: you
        LOGICAL                      :: diffs

        END FUNCTION tensor_check_for_diffs
!********
! Fields
!********
        MODULE SUBROUTINE field_read (me, unit)
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 12/14/2017
        !!
        !! Subroutine performs the read for a field attribute
        !!
        CLASS(field), INTENT(OUT) :: me
        INTEGER(i4k), INTENT(IN)  :: unit

        END SUBROUTINE field_read

        MODULE SUBROUTINE field_write (me, unit)
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 12/13/2017
        !!
        !! Subroutine performs the write for a field attribute
        !!
        CLASS(field), INTENT(IN) :: me
        INTEGER(i4k), INTENT(IN) :: unit

        END SUBROUTINE field_write

        MODULE SUBROUTINE field_setup (me, dataname, datatype, field_arrays)
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 12/14/2017
        !!
        !! Subroutine performs the set-up for a field attribute
        !!
        CLASS(field),     INTENT(OUT) :: me
        CHARACTER(LEN=*), INTENT(IN)  :: dataname
        CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: datatype
        TYPE(field_data_array), DIMENSION(:), INTENT(IN) :: field_arrays

        END SUBROUTINE field_setup

        MODULE FUNCTION field_check_for_diffs (me, you) RESULT (diffs)
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 12/14/2017
        !!
        !! Function checks for differences in a field attribute
        !!
        CLASS(field),     INTENT(IN) :: me
        CLASS(attribute), INTENT(IN) :: you
        LOGICAL                      :: diffs

        END FUNCTION field_check_for_diffs

    END INTERFACE

END MODULE vtk_attributes
