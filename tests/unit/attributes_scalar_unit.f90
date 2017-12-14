PROGRAM vtk_attributes_scalar
    USE Kinds
    USE vtk_attributes, ONLY : attribute, scalar!, OPERATOR(.diff.)
    USE PassFail,       ONLY : Analyze, all_tests_pass
    IMPLICIT NONE
    INTEGER(i4k), PARAMETER :: vtk_unit_1 = 20, vtk_unit_2 = 21
    CHARACTER(LEN=*), PARAMETER :: filename = 'attributes_scalar.vtk'
    REAL(r8k), DIMENSION(*), PARAMETER :: scalar_vals = &
      & [ 0.5_r8k, 1.0_r8k, 2.0_r8k, 4.0_r8k, 2.0_r8k, 1.0_r8k, 0.5_r8k ]
    LOGICAL :: test_pass

    CLASS(attribute), ALLOCATABLE :: vtk_type_1, vtk_type_2

    ALLOCATE(scalar::vtk_type_1, vtk_type_2)

    OPEN (unit=vtk_unit_1, file=filename, status='old', form = 'formatted')
    CALL vtk_type_1%read(vtk_unit_1)
    CLOSE(unit=vtk_unit_1)

    !! Create a similar data type
    CALL vtk_type_2%setup(dataname='temperature', numcomp=1, scalars=scalar_vals)
    OPEN (unit=vtk_unit_2)
    CALL vtk_type_2%write(vtk_unit_2)
    REWIND(unit=vtk_unit_2)
    CALL vtk_type_2%read(vtk_unit_2)
    CLOSE(unit=vtk_unit_2)

    !! Compare the read file and the written/read file to ensure both types are the same
    test_pass = .NOT. (vtk_type_1 .diff. vtk_type_2)

    IF (test_pass) CALL all_tests_pass()

END PROGRAM vtk_attributes_scalar
