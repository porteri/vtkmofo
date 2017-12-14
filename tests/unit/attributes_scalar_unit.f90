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
    LOGICAL, DIMENSION(4) :: test_pass = .FALSE.
    CALL vtk_attributes_scalar  (test_pass(1))
    CALL vtk_attributes_vector  (test_pass(2))
    CALL vtk_attributes_normal  (test_pass(3))
    CALL vtk_attributes_texture (test_pass(4))

    IF (ALL(test_pass)) CALL all_tests_pass()

END PROGRAM vtk_attributes
