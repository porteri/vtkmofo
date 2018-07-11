MODULE vtk_attributes
    USE Precision
    USE Misc, ONLY : def_len
    IMPLICIT NONE
    !>@brief
    !> This module contains the dataset attributes for vtk format
    !>@author
    !> Ian Porter
    !>@date
    !> 12/13/2017
    !
    ! The following dataset attributes are available:
    ! 1) scalars
    ! 2) vectors
    ! 3) normals
    ! 4) texture coordinates (1D, 2D & 3D)
    ! 5) 3x3 tensors
    ! 6) field data
    !

    PRIVATE
    PUBLIC :: attribute, attributes, scalar, vector, normal, texture, tensor, field, field_data_array

    CHARACTER(LEN=*), PARAMETER :: default = 'default'     !! Default table name

    TYPE, ABSTRACT :: attribute
        CHARACTER(LEN=:), ALLOCATABLE :: dataname
        CHARACTER(LEN=:), ALLOCATABLE :: datatype
    CONTAINS
        PROCEDURE(abs_read),  DEFERRED, PUBLIC :: read
        PROCEDURE(abs_write), DEFERRED, PUBLIC :: write
        PROCEDURE(abs_setup), DEFERRED, PUBLIC :: setup
        PROCEDURE, PRIVATE :: check_for_diffs
        GENERIC :: OPERATOR(.diff.) => check_for_diffs
    END TYPE attribute

    TYPE, EXTENDS(attribute) :: scalar
        INTEGER(i4k) :: numcomp = 0
        CHARACTER(LEN=:), ALLOCATABLE :: tablename
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: scalars
    CONTAINS
        PROCEDURE :: read  => scalar_read
        PROCEDURE :: write => scalar_write
        PROCEDURE :: setup => scalar_setup
        PROCEDURE, PRIVATE :: check_for_diffs => check_for_diffs_scalar
    END TYPE scalar

    TYPE, EXTENDS(attribute) :: vector
        REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: vectors
    CONTAINS
        PROCEDURE :: read  => vector_read
        PROCEDURE :: write => vector_write
        PROCEDURE :: setup => vector_setup
        PROCEDURE, PRIVATE :: check_for_diffs => check_for_diffs_vector
    END TYPE vector

    TYPE, EXTENDS(attribute) :: normal
        REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: normals
    CONTAINS
        PROCEDURE :: read  => normal_read
        PROCEDURE :: write => normal_write
        PROCEDURE :: setup => normal_setup
        PROCEDURE, PRIVATE :: check_for_diffs => check_for_diffs_normal
    END TYPE normal

    TYPE, EXTENDS(attribute) :: texture
        REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: textures
    CONTAINS
        PROCEDURE :: read  => texture_read
        PROCEDURE :: write => texture_write
        PROCEDURE :: setup => texture_setup
        PROCEDURE, PRIVATE :: check_for_diffs => check_for_diffs_texture
    END TYPE texture

    TYPE :: tensor_array
        REAL(r8k), DIMENSION(3,3) :: val = 0.0_r8k
    END TYPE tensor_array

    TYPE, EXTENDS(attribute) :: tensor
        TYPE(tensor_array), DIMENSION(:), ALLOCATABLE :: tensors
    CONTAINS
        PROCEDURE :: read  => tensor_read
        PROCEDURE :: write => tensor_write
        PROCEDURE :: setup => tensor_setup
        PROCEDURE, PRIVATE :: check_for_diffs => check_for_diffs_tensor
    END TYPE tensor

    TYPE :: field_data_array
        CHARACTER(LEN=:), ALLOCATABLE :: name
        INTEGER(i4k) :: numComponents = 0
        INTEGER(i4k) :: numTuples     = 0
        CHARACTER(LEN=:), ALLOCATABLE :: datatype
        REAL(r8k), DIMENSION(:,:), ALLOCATABLE :: data
    END TYPE field_data_array

    TYPE, EXTENDS(attribute) :: field
        TYPE(field_data_array), DIMENSION(:), ALLOCATABLE :: array
    CONTAINS
        PROCEDURE :: read  => field_read
        PROCEDURE :: write => field_write
        PROCEDURE :: setup => field_setup
        PROCEDURE, PRIVATE :: check_for_diffs => check_for_diffs_field
    END TYPE field

    TYPE :: attributes
        INTEGER(i4k) :: n = 0 !! # of points or cells in the dataset
        CLASS(attribute), ALLOCATABLE :: attribute
    END TYPE attributes

    INTERFACE

        MODULE SUBROUTINE abs_read (me, unit)
        !>@brief
        !> Abstract for reading an attribute
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 12/13/2017
        CLASS(attribute), INTENT(OUT) :: me
        INTEGER(i4k),     INTENT(IN)  :: unit

        END SUBROUTINE abs_read

        MODULE SUBROUTINE abs_write (me, unit)
        !>@brief
        !> Abstract for writing an attribute
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 12/13/2017
        CLASS(attribute), INTENT(IN) :: me
        INTEGER(i4k),     INTENT(IN) :: unit

        END SUBROUTINE abs_write

        MODULE SUBROUTINE abs_setup (me, dataname, datatype, numcomp, tablename, values1d, values2d, values3d, field_arrays)
        !>@brief
        !> Abstract for performing the set-up of an attribute
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 12/13/2017
        CLASS(attribute), INTENT(OUT) :: me
        CHARACTER(LEN=*), INTENT(IN)  :: dataname
        INTEGER(i4k),     INTENT(IN), OPTIONAL :: numcomp
        CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: datatype, tablename
!        REAL(r8k), DIMENSION(..),   INTENT(IN), OPTIONAL :: values
        REAL(r8k), DIMENSION(:),     INTENT(IN), OPTIONAL :: values1d
        REAL(r8k), DIMENSION(:,:),   INTENT(IN), OPTIONAL :: values2d
        REAL(r8k), DIMENSION(:,:,:), INTENT(IN), OPTIONAL :: values3d
        TYPE(field_data_array), DIMENSION(:), INTENT(IN), OPTIONAL :: field_arrays

        END SUBROUTINE abs_setup

        MODULE FUNCTION check_for_diffs (me, you) RESULT (diffs)
        !>@brief
        !> Function checks for differences in an attribute
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 12/13/2017
        CLASS(attribute), INTENT(IN) :: me, you
        LOGICAL                      :: diffs

        END FUNCTION check_for_diffs
!********
! Scalars
!********
        MODULE SUBROUTINE scalar_read (me, unit)
        !>@brief
        !> Subroutine performs the read for a scalar attribute
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 12/13/2017
        CLASS(scalar), INTENT(OUT) :: me
        INTEGER(i4k),  INTENT(IN)  :: unit

        END SUBROUTINE scalar_read

        MODULE SUBROUTINE scalar_write (me, unit)
        !>@brief
        !> Subroutine performs the write for a scalar attribute
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 12/13/2017
        CLASS(scalar), INTENT(IN) :: me
        INTEGER(i4k),  INTENT(IN) :: unit

        END SUBROUTINE scalar_write

        MODULE SUBROUTINE scalar_setup (me, dataname, datatype, numcomp, tablename, values1d, values2d, values3d, field_arrays)
        !>@brief
        !> Subroutine performs the set-up for a scalar attribute
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 12/13/2017
        CLASS(scalar),    INTENT(OUT) :: me
        CHARACTER(LEN=*), INTENT(IN)  :: dataname
        INTEGER(i4k),     INTENT(IN), OPTIONAL :: numcomp
        CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: datatype, tablename
!        REAL(r8k), DIMENSION(..),   INTENT(IN), OPTIONAL :: values
        REAL(r8k), DIMENSION(:),     INTENT(IN), OPTIONAL :: values1d
        REAL(r8k), DIMENSION(:,:),   INTENT(IN), OPTIONAL :: values2d
        REAL(r8k), DIMENSION(:,:,:), INTENT(IN), OPTIONAL :: values3d
        TYPE(field_data_array), DIMENSION(:), INTENT(IN), OPTIONAL :: field_arrays

        END SUBROUTINE scalar_setup

        MODULE FUNCTION check_for_diffs_scalar (me, you) RESULT (diffs)
        !>@brief
        !> Function checks for differences in a scalar attribute
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 12/13/2017
        CLASS(scalar),    INTENT(IN) :: me
        CLASS(attribute), INTENT(IN) :: you
        LOGICAL                      :: diffs

        END FUNCTION check_for_diffs_scalar
!********
! Vectors
!********
        MODULE SUBROUTINE vector_read (me, unit)
        !>@brief
        !> Subroutine performs the read for a vector attribute
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 12/14/2017
        CLASS(vector), INTENT(OUT) :: me
        INTEGER(i4k),  INTENT(IN)  :: unit

        END SUBROUTINE vector_read

        MODULE SUBROUTINE vector_write (me, unit)
        !>@brief
        !> Subroutine performs the write for a vector attribute
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 12/13/2017
        CLASS(vector), INTENT(IN) :: me
        INTEGER(i4k),  INTENT(IN) :: unit

        END SUBROUTINE vector_write

        MODULE SUBROUTINE vector_setup (me, dataname, datatype, numcomp, tablename, values1d, values2d, values3d, field_arrays)
        !>@brief
        !> Subroutine performs the set-up for a vector attribute
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 12/14/2017
        CLASS(vector),    INTENT(OUT) :: me
        CHARACTER(LEN=*), INTENT(IN)  :: dataname
        INTEGER(i4k),     INTENT(IN), OPTIONAL :: numcomp
        CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: datatype, tablename
!        REAL(r8k), DIMENSION(..),   INTENT(IN), OPTIONAL :: values
        REAL(r8k), DIMENSION(:),     INTENT(IN), OPTIONAL :: values1d
        REAL(r8k), DIMENSION(:,:),   INTENT(IN), OPTIONAL :: values2d
        REAL(r8k), DIMENSION(:,:,:), INTENT(IN), OPTIONAL :: values3d
        TYPE(field_data_array), DIMENSION(:), INTENT(IN), OPTIONAL :: field_arrays

        END SUBROUTINE vector_setup

        MODULE FUNCTION check_for_diffs_vector (me, you) RESULT (diffs)
        !>@brief
        !> Function checks for differences in a vector attribute
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 12/14/2017
        CLASS(vector),    INTENT(IN) :: me
        CLASS(attribute), INTENT(IN) :: you
        LOGICAL                      :: diffs

        END FUNCTION check_for_diffs_vector
!********
! Normals
!********
        MODULE SUBROUTINE normal_read (me, unit)
        !>@brief
        !> Subroutine performs the read for a normal attribute
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 12/14/2017
        CLASS(normal), INTENT(OUT) :: me
        INTEGER(i4k),  INTENT(IN)  :: unit

        END SUBROUTINE normal_read

        MODULE SUBROUTINE normal_write (me, unit)
        !>@brief
        !> Subroutine performs the write for a normal attribute
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 12/13/2017
        CLASS(normal), INTENT(IN) :: me
        INTEGER(i4k),  INTENT(IN) :: unit

        END SUBROUTINE normal_write

        MODULE SUBROUTINE normal_setup (me, dataname, datatype, numcomp, tablename, values1d, values2d, values3d, field_arrays)
        !>@brief
        !> Subroutine performs the set-up for a normal attribute
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 12/14/2017
        CLASS(normal),    INTENT(OUT) :: me
        CHARACTER(LEN=*), INTENT(IN)  :: dataname
        INTEGER(i4k),     INTENT(IN), OPTIONAL :: numcomp
        CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: datatype, tablename
!        REAL(r8k), DIMENSION(..),   INTENT(IN), OPTIONAL :: values
        REAL(r8k), DIMENSION(:),     INTENT(IN), OPTIONAL :: values1d
        REAL(r8k), DIMENSION(:,:),   INTENT(IN), OPTIONAL :: values2d
        REAL(r8k), DIMENSION(:,:,:), INTENT(IN), OPTIONAL :: values3d
        TYPE(field_data_array), DIMENSION(:), INTENT(IN), OPTIONAL :: field_arrays

        END SUBROUTINE normal_setup

        MODULE FUNCTION check_for_diffs_normal (me, you) RESULT (diffs)
        !>@brief
        !> Function checks for differences in a normal attribute
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 12/14/2017
        CLASS(normal),    INTENT(IN) :: me
        CLASS(attribute), INTENT(IN) :: you
        LOGICAL                      :: diffs

        END FUNCTION check_for_diffs_normal
!********
! Textures
!********
        MODULE SUBROUTINE texture_read (me, unit)
        !>@brief
        !> Subroutine performs the read for a texture attribute
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 12/14/2017
        CLASS(texture), INTENT(OUT) :: me
        INTEGER(i4k),   INTENT(IN)  :: unit

        END SUBROUTINE texture_read

        MODULE SUBROUTINE texture_write (me, unit)
        !>@brief
        !> Subroutine performs the write for a texture attribute
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 12/13/2017
        CLASS(texture), INTENT(IN) :: me
        INTEGER(i4k),   INTENT(IN) :: unit

        END SUBROUTINE texture_write

        MODULE SUBROUTINE texture_setup (me, dataname, datatype, numcomp, tablename, values1d, values2d, values3d, field_arrays)
        !>@brief
        !> Subroutine performs the set-up for a texture attribute
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 12/14/2017
        CLASS(texture),   INTENT(OUT) :: me
        CHARACTER(LEN=*), INTENT(IN)  :: dataname
        INTEGER(i4k),     INTENT(IN), OPTIONAL :: numcomp
        CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: datatype, tablename
!        REAL(r8k), DIMENSION(..),   INTENT(IN), OPTIONAL :: values
        REAL(r8k), DIMENSION(:),     INTENT(IN), OPTIONAL :: values1d
        REAL(r8k), DIMENSION(:,:),   INTENT(IN), OPTIONAL :: values2d
        REAL(r8k), DIMENSION(:,:,:), INTENT(IN), OPTIONAL :: values3d
        TYPE(field_data_array), DIMENSION(:), INTENT(IN), OPTIONAL :: field_arrays

        END SUBROUTINE texture_setup

        MODULE FUNCTION check_for_diffs_texture (me, you) RESULT (diffs)
        !>@brief
        !> Function checks for differences in a texture attribute
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 12/14/2017
        CLASS(texture),   INTENT(IN) :: me
        CLASS(attribute), INTENT(IN) :: you
        LOGICAL                      :: diffs

        END FUNCTION check_for_diffs_texture
!********
! Tensors
!********
        MODULE SUBROUTINE tensor_read (me, unit)
        !>@brief
        !> Subroutine performs the read for a tensor attribute
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 12/14/2017
        CLASS(tensor), INTENT(OUT) :: me
        INTEGER(i4k),  INTENT(IN)  :: unit

        END SUBROUTINE tensor_read

        MODULE SUBROUTINE tensor_write (me, unit)
        !>@brief
        !> Subroutine performs the write for a tensor attribute
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 12/13/2017
        CLASS(tensor), INTENT(IN) :: me
        INTEGER(i4k),  INTENT(IN) :: unit

        END SUBROUTINE tensor_write

        MODULE SUBROUTINE tensor_setup (me, dataname, datatype, numcomp, tablename, values1d, values2d, values3d, field_arrays)
        !>@brief
        !> Subroutine performs the set-up for a tensor attribute
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 12/14/2017
        CLASS(tensor),    INTENT(OUT) :: me
        CHARACTER(LEN=*), INTENT(IN)  :: dataname
        INTEGER(i4k),     INTENT(IN), OPTIONAL :: numcomp
        CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: datatype, tablename
!        REAL(r8k), DIMENSION(..),   INTENT(IN), OPTIONAL :: values
        REAL(r8k), DIMENSION(:),     INTENT(IN), OPTIONAL :: values1d
        REAL(r8k), DIMENSION(:,:),   INTENT(IN), OPTIONAL :: values2d
        REAL(r8k), DIMENSION(:,:,:), INTENT(IN), OPTIONAL :: values3d
        TYPE(field_data_array), DIMENSION(:), INTENT(IN), OPTIONAL :: field_arrays

        END SUBROUTINE tensor_setup

        MODULE FUNCTION check_for_diffs_tensor (me, you) RESULT (diffs)
        !>@brief
        !> Function checks for differences in a tensor attribute
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 12/14/2017
        CLASS(tensor),    INTENT(IN) :: me
        CLASS(attribute), INTENT(IN) :: you
        LOGICAL                      :: diffs

        END FUNCTION check_for_diffs_tensor
!********
! Fields
!********
        MODULE SUBROUTINE field_read (me, unit)
        !>@brief
        !> Subroutine performs the read for a field attribute
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 12/14/2017
        CLASS(field), INTENT(OUT) :: me
        INTEGER(i4k), INTENT(IN)  :: unit

        END SUBROUTINE field_read

        MODULE SUBROUTINE field_write (me, unit)
        !>@brief
        !> Subroutine performs the write for a field attribute
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 12/13/2017
        CLASS(field), INTENT(IN) :: me
        INTEGER(i4k), INTENT(IN) :: unit

        END SUBROUTINE field_write

        MODULE SUBROUTINE field_setup (me, dataname, datatype, numcomp, tablename, values1d, values2d, values3d, field_arrays)
        !>@brief
        !> Subroutine performs the set-up for a field attribute
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 12/14/2017
        CLASS(field),     INTENT(OUT) :: me
        CHARACTER(LEN=*), INTENT(IN)  :: dataname
        INTEGER(i4k),     INTENT(IN), OPTIONAL :: numcomp
        CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: datatype, tablename
!        REAL(r8k), DIMENSION(..),   INTENT(IN), OPTIONAL :: values
        REAL(r8k), DIMENSION(:),     INTENT(IN), OPTIONAL :: values1d
        REAL(r8k), DIMENSION(:,:),   INTENT(IN), OPTIONAL :: values2d
        REAL(r8k), DIMENSION(:,:,:), INTENT(IN), OPTIONAL :: values3d
        TYPE(field_data_array), DIMENSION(:), INTENT(IN), OPTIONAL :: field_arrays

        END SUBROUTINE field_setup

        MODULE FUNCTION check_for_diffs_field (me, you) RESULT (diffs)
        !>@brief
        !> Function checks for differences in a field attribute
        !>@author
        !> Ian Porter, NRC
        !>@date
        !> 12/14/2017
        CLASS(field),     INTENT(IN) :: me
        CLASS(attribute), INTENT(IN) :: you
        LOGICAL                      :: diffs

        END FUNCTION check_for_diffs_field

    END INTERFACE

END MODULE vtk_attributes
