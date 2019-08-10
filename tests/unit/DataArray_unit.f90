PROGRAM DataArray_test
    USE Precision,             ONLY : i4k
    USE VTKmofoPassFail,       ONLY : all_tests_pass
    USE VTK_DataArray_element, ONLY : DataArray_dt
    USE XML,                   ONLY : file_format, ascii
    IMPLICIT NONE
    !! author: Ian Porter
    !! date: 06/07/2019
    !!
    !! DataArray derived type unit test
    !!
    !! This test will produce the following output:
    !!
    !!<DataArray  offset="0">
    !!    <DataArray type="Float32" Name="foo_data" NumberOfComponents="1" format="ascii" offset="0">
    !!    </DataArray>
    !!</DataArray>
    !!
    TYPE(DataArray_dt) :: foo, foo2
    INTEGER(i4k) :: unit
    CHARACTER(LEN=*), PARAMETER :: type = 'Float32'
    CHARACTER(LEN=*), PARAMETER :: name = 'foo_data'
    INTEGER(i4k),     PARAMETER :: NumberOfComponents = 1
    CHARACTER(LEN=*), PARAMETER :: format = 'ascii'
    CHARACTER(LEN=*), PARAMETER :: offset = '0'

    file_format = ascii

    CALL foo%initialize(type=type)
    CALL foo%initialize(name=name)
    CALL foo%initialize(NumberOfComponents=NumberOfComponents)
    CALL foo%initialize(format=format)
    CALL foo%initialize(offset=offset)
    CALL foo2%initialize(type, name, NumberOfComponents, format, offset)

    CALL foo%add(foo2)

    OPEN (newunit=unit,file="DataArray_test.xml",status="replace",form="formatted")
    CALL foo%write(unit)

    CALL all_tests_pass()

END PROGRAM DataArray_test
