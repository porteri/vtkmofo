program xml_test
    use vtkmofopassfail, only : all_tests_pass
    use xml,             only : xml_element_dt, xml_file_dt
    use precision,       only : i4k, i8k, r4k, r8k
    implicit none
    !! author: Ian Porter
    !! date: 05/02/2019
    !!
    !! xml derived type unit test
    !!
    !! this test will produce the following output:
    !!
    !!<?xml version="1.0" encoding="utf-8"?>
    !!<xml_foo>
    !!   <xml_foo1 needed="additional_data">
    !!            "blah"
    !!            "blah"
    !!            "blah"
    !!   </xml_foo1>
    !!   <xml_foo2 needed="nothing new to report here">
    !!       "more blah"
    !!       "more blah"
    !!   </xml_foo2>
    !!   <xml_foo2 needed="still nothing new to report here">
    !!        "more blah"
    !!        "more blah"
    !!        <xml_foo3>
    !!            4
    !!            8 8
    !!            4.0000000000000000
    !!            8.0000000000000000 8.0000000000000000
    !!            true false
    !!        </xml_foo3>
    !!   </xml_foo2>
    !!   <xml_foo3>
    !!       4
    !!       8 8
    !!       4.0000000000000000
    !!       8.0000000000000000 8.0000000000000000
    !!       true false
    !!   </xml_foo3>
    !!</xml_foo>
    !!<xml_foo2 needed="still nothing new to report here">
    !!     "more blah"
    !!     "more blah"
    !!     <xml_foo3>
    !!         4
    !!         8 8
    !!         4.0000000000000000
    !!         8.0000000000000000 8.0000000000000000
    !!         true false
    !!     </xml_foo3>
    !!</xml_foo2>
    !!<xml_foo2 needed="still nothing new to report here">
    !!     "more blah"
    !!     "more blah"
    !!     <xml_foo3>
    !!         4
    !!         8 8
    !!         4.0000000000000000
    !!         8.0000000000000000 8.0000000000000000
    !!         true false
    !!     </xml_foo3>
    !!</xml_foo2>
    !!
    !!
    type(xml_element_dt) :: foo, foo1, foo2, foo3
    type(xml_file_dt) :: xml_file

    call foo%setup('xml_foo','',offset=3)

    call foo1%setup('xml_foo1','needed="additional_data"',9)
    call foo1%add('blah')
    call foo1%add('blah')
    call foo1%add('blah')
    call foo%add(foo1)
    call foo2%setup('xml_foo2','needed="nothing new to report here"')
    call foo2%add('more blah')
    call foo2%add('more blah')
    call foo%add(foo2)

    call foo2%setup('xml_foo2','needed="still nothing new to report here"',5)
    call foo2%add('even more blah')
    call foo2%add('even more blah')

    call foo3%setup('xml_foo3')
    call foo3%add([ 4_i4k ])
    call foo3%add([ 8_i8k, 8_i8k ])
    call foo3%add([ 4.0_r8k ])
    call foo3%add([ 8.0_r8k, 8.0_r8k ])
    call foo3%add([ .true., .false. ])

    call foo2%add(foo3)
    call foo%add(foo2)
    call foo%add(foo3)

    call xml_file%setup(filename='xml_test.xml')
    call xml_file%add(foo)
    call xml_file%add(foo2)
    call xml_file%add(foo2)

    call xml_file%write()

    call all_tests_pass()

end program xml_test
