MODULE vtk_attributes_unit_tests
    CONTAINS
        SUBROUTINE vtk_attributes_scalar (test_pass)
        USE Kinds
        USE vtk_attributes, ONLY : attribute, scalar
        USE PassFail,       ONLY : Analyze
        IMPLICIT NONE
        INTEGER(i4k), PARAMETER :: vtk_unit_1 = 20, vtk_unit_2 = 21
        CHARACTER(LEN=*), PARAMETER :: filename = 'attributes_scalar.vtk'
        REAL(r8k), DIMENSION(*), PARAMETER :: scalar_vals = &
          & [ 0.5_r8k, 1.0_r8k, 2.0_r8k, 4.0_r8k, 2.0_r8k, 1.0_r8k, 0.5_r8k ]
        LOGICAL, INTENT(OUT) :: test_pass

        CLASS(attribute), ALLOCATABLE :: vtk_type_1, vtk_type_2

        ALLOCATE(scalar::vtk_type_1, vtk_type_2)

        OPEN (unit=vtk_unit_1, file=filename, status='old', form = 'formatted')
        CALL vtk_type_1%read(vtk_unit_1)
        CLOSE(unit=vtk_unit_1)

        !! Create a similar data type
        CALL vtk_type_2%setup(dataname='temperature', numcomp=1, values1d=scalar_vals)
        OPEN (unit=vtk_unit_2)
        CALL vtk_type_2%write(vtk_unit_2)
        REWIND(unit=vtk_unit_2)
        CALL vtk_type_2%read(vtk_unit_2)
        CLOSE(unit=vtk_unit_2)

        !! Compare the read file and the written/read file to ensure both types are the same
        test_pass = .NOT. (vtk_type_1 .diff. vtk_type_2)

        END SUBROUTINE vtk_attributes_scalar

        SUBROUTINE vtk_attributes_vector (test_pass)
        USE Kinds
        USE vtk_attributes, ONLY : attribute, vector
        USE PassFail,       ONLY : Analyze
        IMPLICIT NONE
        INTEGER(i4k), PARAMETER :: vtk_unit_1 = 20, vtk_unit_2 = 21
        CHARACTER(LEN=*), PARAMETER :: filename = 'attributes_vector.vtk'
        REAL(r8k), DIMENSION(2,3), PARAMETER :: vector_vals = RESHAPE ( &
          & [ 0.5_r8k, 1.0_r8k, 0.5_r8k, &
          &   4.0_r8k, 2.0_r8k, 1.0_r8k ], [2,3])
        LOGICAL, INTENT(OUT) :: test_pass

        CLASS(attribute), ALLOCATABLE :: vtk_type_1, vtk_type_2

        ALLOCATE(vector::vtk_type_1, vtk_type_2)

        OPEN (unit=vtk_unit_1, file=filename, status='old', form = 'formatted')
        CALL vtk_type_1%read(vtk_unit_1)
        CLOSE(unit=vtk_unit_1)

        !! Create a similar data type
        CALL vtk_type_2%setup(dataname='temperature', numcomp=1, values2d=vector_vals)
        OPEN (unit=vtk_unit_2)
        CALL vtk_type_2%write(vtk_unit_2)
        REWIND(unit=vtk_unit_2)
        CALL vtk_type_2%read(vtk_unit_2)
        CLOSE(unit=vtk_unit_2)

        !! Compare the read file and the written/read file to ensure both types are the same
        test_pass = .NOT. (vtk_type_1 .diff. vtk_type_2)

        END SUBROUTINE vtk_attributes_vector

        SUBROUTINE vtk_attributes_normal (test_pass)
        USE Kinds
        USE vtk_attributes, ONLY : attribute, normal
        USE PassFail,       ONLY : Analyze
        IMPLICIT NONE
        INTEGER(i4k), PARAMETER :: vtk_unit_1 = 20, vtk_unit_2 = 21
        CHARACTER(LEN=*), PARAMETER :: filename = 'attributes_normal.vtk'
        REAL(r8k), DIMENSION(4,3), PARAMETER :: normal_vals = RESHAPE ( &
          & [ 0.5_r8k, 1.0_r8k, 0.5_r8k, &
          &   1.0_r8k, 1.0_r8k, 1.0_r8k, &
          &   1.0_r8k, 0.5_r8k, 1.0_r8k, &
          &   1.0_r8k, 0.9_r8k, 1.0_r8k ], [4,3])
        LOGICAL, INTENT(OUT) :: test_pass

        CLASS(attribute), ALLOCATABLE :: vtk_type_1, vtk_type_2

        ALLOCATE(normal::vtk_type_1, vtk_type_2)

        OPEN (unit=vtk_unit_1, file=filename, status='old', form = 'formatted')
        CALL vtk_type_1%read(vtk_unit_1)
        CLOSE(unit=vtk_unit_1)

        !! Create a similar data type
        CALL vtk_type_2%setup(dataname='normalized_temp', numcomp=1, values2d=normal_vals)
        OPEN (unit=vtk_unit_2)
        CALL vtk_type_2%write(vtk_unit_2)
        REWIND(unit=vtk_unit_2)
        CALL vtk_type_2%read(vtk_unit_2)
        CLOSE(unit=vtk_unit_2)

        !! Compare the read file and the written/read file to ensure both types are the same
        test_pass = .NOT. (vtk_type_1 .diff. vtk_type_2)

        END SUBROUTINE vtk_attributes_normal

        SUBROUTINE vtk_attributes_texture (test_pass)
        USE Kinds
        USE vtk_attributes, ONLY : attribute, texture
        USE PassFail,       ONLY : Analyze
        IMPLICIT NONE
        INTEGER(i4k), PARAMETER :: vtk_unit_1 = 20, vtk_unit_2 = 21
        CHARACTER(LEN=*), PARAMETER :: filename = 'attributes_texture.vtk'
        REAL(r8k), DIMENSION(6,2), PARAMETER :: texture_vals = RESHAPE ( &
          & [ 0.5_r8k, 1.0_r8k, &
          &   1.0_r8k, 1.0_r8k, &
          &   1.0_r8k, 0.5_r8k, &
          &   1.0_r8k, 0.9_r8k, &
          &   1.0_r8k, 0.9_r8k, &
          &   1.0_r8k, 0.9_r8k ], [6,2])
        LOGICAL, INTENT(OUT) :: test_pass

        CLASS(attribute), ALLOCATABLE :: vtk_type_1, vtk_type_2

        ALLOCATE(texture::vtk_type_1, vtk_type_2)

        OPEN (unit=vtk_unit_1, file=filename, status='old', form = 'formatted')
        CALL vtk_type_1%read(vtk_unit_1)
        CLOSE(unit=vtk_unit_1)

        !! Create a similar data type
        CALL vtk_type_2%setup(dataname='textured_temp', numcomp=1, values2d=texture_vals)
        OPEN (unit=vtk_unit_2)
        CALL vtk_type_2%write(vtk_unit_2)
        REWIND(unit=vtk_unit_2)
        CALL vtk_type_2%read(vtk_unit_2)
        CLOSE(unit=vtk_unit_2)

        !! Compare the read file and the written/read file to ensure both types are the same
        test_pass = .NOT. (vtk_type_1 .diff. vtk_type_2)

        END SUBROUTINE vtk_attributes_texture

        SUBROUTINE vtk_attributes_tensor (test_pass)
        USE Kinds
        USE vtk_attributes, ONLY : attribute, tensor
        USE PassFail,       ONLY : Analyze
        IMPLICIT NONE
        INTEGER(i4k), PARAMETER :: vtk_unit_1 = 20, vtk_unit_2 = 21
        CHARACTER(LEN=*), PARAMETER :: filename = 'attributes_tensor.vtk'
        REAL(r8k), DIMENSION(3,3), PARAMETER :: tensor_1 = RESHAPE ( &
          & [ 0.57_r8k, 1.00_r8k, 0.00_r8k, &
          &   1.00_r8k, 0.75_r8k, 0.80_r8k, &
          &   0.00_r8k, 0.80_r8k, 0.57_r8k ], [3,3])
        REAL(r8k), DIMENSION(3,3), PARAMETER :: tensor_2 = RESHAPE ( &
          & [ 1.57_r8k, 2.00_r8k, 1.00_r8k, &
          &   2.00_r8k, 1.75_r8k, 1.80_r8k, &
          &   1.00_r8k, 1.80_r8k, 1.57_r8k], [3,3])
        REAL(r8k), DIMENSION(3,3), PARAMETER :: tensor_3 = RESHAPE ( &
          & [ 2.57_r8k, 3.00_r8k, 2.00_r8k, &
          &   3.00_r8k, 2.75_r8k, 2.80_r8k, &
          &   2.00_r8k, 2.80_r8k, 2.57_r8k ], [3,3])
        REAL(r8k), DIMENSION(3,3), PARAMETER :: tensor_4 = RESHAPE ( &
          & [ 3.57_r8k, 4.00_r8k, 3.00_r8k, &
          &   4.00_r8k, 3.75_r8k, 3.80_r8k, &
          &   3.00_r8k, 3.80_r8k, 3.57_r8k ], [3,3])
        REAL(r8k), DIMENSION(4,3,3) :: tensor_vals
        LOGICAL, INTENT(OUT) :: test_pass

        CLASS(attribute), ALLOCATABLE :: vtk_type_1, vtk_type_2

        tensor_vals(1,:,:) = tensor_1; tensor_vals(2,:,:) = tensor_2; tensor_vals(3,:,:) = tensor_3; tensor_vals(4,:,:) = tensor_4
        ALLOCATE(tensor::vtk_type_1, vtk_type_2)

        OPEN (unit=vtk_unit_1, file=filename, status='old', form = 'formatted')
        CALL vtk_type_1%read(vtk_unit_1)
        CLOSE(unit=vtk_unit_1)

        !! Create a similar data type
        CALL vtk_type_2%setup(dataname='tensor_temp', numcomp=1, values3d=tensor_vals)
        OPEN (unit=vtk_unit_2,FILE=FILENAME)
        CALL vtk_type_2%write(vtk_unit_2)
        REWIND(unit=vtk_unit_2)
        CALL vtk_type_2%read(vtk_unit_2)
        CLOSE(unit=vtk_unit_2)

        !! Compare the read file and the written/read file to ensure both types are the same
        test_pass = .NOT. (vtk_type_1 .diff. vtk_type_2)

        END SUBROUTINE vtk_attributes_tensor

        SUBROUTINE vtk_attributes_field (test_pass)
        USE Kinds
        USE vtk_attributes, ONLY : attribute, field, field_data_array
        USE PassFail,       ONLY : Analyze
        IMPLICIT NONE
        INTEGER(i4k),     PARAMETER :: vtk_unit_1 = 20, vtk_unit_2 = 21
        CHARACTER(LEN=*), PARAMETER :: filename = 'attributes_field.vtk'
        TYPE(field_data_array)      :: array_1, array_2
        TYPE(field_data_array), DIMENSION(2) :: array
        LOGICAL, INTENT(OUT)        :: test_pass
        REAL(r8k), DIMENSION(3,3), PARAMETER :: data_1 = RESHAPE ( &
          & [ 200.0_r8k, 200.0_r8k, 200.0_r8k, &
          &   300.0_r8k, 400.0_r8k, 300.0_r8k, &
          &   500.0_r8k, 450.0_r8k, 500.0_r8k ], [3,3] )
        REAL(r8k), DIMENSION(2,5), PARAMETER :: data_2 = RESHAPE ( &
          & [ 20.0_r8k, 200.0_r8k, &
          &   25.0_r8k, 250.0_r8k, &
          &   30.0_r8k, 300.0_r8k, &
          &   35.0_r8k, 350.0_r8k, &
          &   40.0_r8k, 400.0_r8k ], [2,5] )

        CLASS(attribute), ALLOCATABLE :: vtk_type_1, vtk_type_2

        ALLOCATE(field::vtk_type_1, vtk_type_2)

        array_1%name = 'temps'; array_1%numComponents=3; array_1%numTuples=3; array_1%datatype='double'; array_1%data = data_1
        array_2%name = 'pressures'; array_2%numComponents=5; array_2%numTuples=2; array_2%datatype='double'; array_2%data = data_2
        array(1) = array_1; array(2) = array_2

        OPEN (unit=vtk_unit_1, file=filename, status='old', form = 'formatted')
        CALL vtk_type_1%read(vtk_unit_1)
        CLOSE(unit=vtk_unit_1)

        !! Create a similar data type
        CALL vtk_type_2%setup(dataname='field_temp_press', numcomp=1, field_arrays=array)
        OPEN (unit=vtk_unit_2)
        CALL vtk_type_2%write(vtk_unit_2)
        REWIND(unit=vtk_unit_2)
        CALL vtk_type_2%read(vtk_unit_2)
        CLOSE(unit=vtk_unit_2)

        !! Compare the read file and the written/read file to ensure both types are the same
        test_pass = .NOT. (vtk_type_1 .diff. vtk_type_2)

        END SUBROUTINE vtk_attributes_field
END MODULE vtk_attributes_unit_tests

PROGRAM vtk_attributes
    USE vtk_attributes_unit_tests
    USE PassFail, ONLY : all_tests_pass
    IMPLICIT NONE
    !>@brief
    !> Driver testing subroutine for the attributes information
    !>@author
    !> Ian Porter
    !>@date
    !> 12/14/2017
    LOGICAL, DIMENSION(6) :: test_pass = .FALSE.

    CALL vtk_attributes_scalar  (test_pass(1))
    CALL vtk_attributes_vector  (test_pass(2))
    CALL vtk_attributes_normal  (test_pass(3))
    CALL vtk_attributes_texture (test_pass(4))
    CALL vtk_attributes_tensor  (test_pass(5))
    CALL vtk_attributes_field   (test_pass(6))

    IF (ALL(test_pass)) CALL all_tests_pass()

END PROGRAM vtk_attributes
