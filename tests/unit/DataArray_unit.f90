program dataarray_test
    use precision,             only : i4k
    use vtkmofopassfail,       only : all_tests_pass
    use vtk_dataarray_element, only : dataarray_dt
    use xml,                   only : file_format, ascii
    implicit none
    !! author: Ian Porter
    !! date: 06/07/2019
    !!
    !! dataarray derived type unit test
    !!
    !! this test will produce the following output:
    !!
    !!<dataarray  offset="0">
    !!    <dataarray type="float32" name="foo_data" numberofcomponents="1" format="ascii" offset="0">
    !!    </dataarray>
    !!</dataarray>
    !!
    type(dataarray_dt) :: foo, foo2
    integer(i4k) :: unit
    character(len=*), parameter :: type = 'float32'
    character(len=*), parameter :: name = 'foo_data'
    integer(i4k),     parameter :: numberofcomponents = 1
    character(len=*), parameter :: format = 'ascii'
    character(len=*), parameter :: offset = '0'

    file_format = ascii

    call foo%initialize(type=type)
    call foo%initialize(name=name)
    call foo%initialize(numberofcomponents=numberofcomponents)
    call foo%initialize(format=format)
    call foo%initialize(offset=offset)
    call foo2%initialize(type, name, numberofcomponents, format, offset)

    call foo%add(foo2)

    open (newunit=unit,file="dataarray_test.xml",status="replace",form="formatted")
    call foo%write(unit)

    call all_tests_pass()

end program dataarray_test
