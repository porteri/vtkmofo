PROGRAM DataArray_test
    USE Precision,         ONLY : i4k
    USE VTKmofoPassFail,   ONLY : all_tests_pass
    USE VTK_piece_element, ONLY : DataArray_dt
    IMPLICIT NONE
    !! author: Ian Porter
    !! date: 06/07/2019
    !!
    !! DataArray derived type unit test
    !!
    !! This test will produce the following output:
    !!
    !!<DataArray type="">
    !!</DataArray>
    !!
    TYPE(DataArray_dt) :: foo
    CHARACTER(LEN=*), PARAMETER :: type = 'Float32'
    CHARACTER(LEN=*), PARAMETER :: name = 'foo_data'
    INTEGER(i4k), PARAMETER :: NumberOfComponents = 1
    CHARACTER(LEN=*), PARAMETER :: format = 'ascii'
    CHARACTER(LEN=*), PARAMETER :: offset = '0'

    CALL foo%initialize(type=type)
    CALL foo%initialize(name=name)
    CALL foo%initialize(NumberOfComponents=NumberOfComponents)
    CALL foo%initialize(format=format)
    CALL foo%initialize(offset=offset)
    CALL foo%initialize(type, name, NumberOfComponents, format, offset)

    CALL all_tests_pass()

END PROGRAM DataArray_test
