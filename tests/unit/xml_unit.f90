PROGRAM xml_test
    USE VTKmofoPassFail, ONLY : all_tests_pass
    USE XML,             ONLY : xml_element_dt, xml_file_dt
    USE Precision,       ONLY : i4k, i8k, r4k, r8k
    IMPLICIT NONE
    !! author: Ian Porter
    !! date: 05/02/2019
    !!
    !! XML derived type unit test
    !!
    !! This test will produce the following output:
    !!
    !!<?xml version="1.0" encoding="UTF-8"?>
    !!<xMl_FoO>
    !!   <xMl_FoO1 needed="additional_data">
    !!            "blah"
    !!            "blah"
    !!            "blah"
    !!   </xMl_FoO1>
    !!   <xMl_FoO2 needed="nothing new to report here">
    !!       "more blah"
    !!       "more blah"
    !!   </xMl_FoO2>
    !!   <xMl_FoO2 needed="still nothing new to report here">
    !!        "more blah"
    !!        "more blah"
    !!        <xml_FOO3>
    !!            4
    !!            8 8
    !!            4.0000000000000000
    !!            8.0000000000000000 8.0000000000000000
    !!            True False
    !!        </xml_FOO3>
    !!   </xMl_FoO2>
    !!   <xml_FOO3>
    !!       4
    !!       8 8
    !!       4.0000000000000000
    !!       8.0000000000000000 8.0000000000000000
    !!       True False
    !!   </xml_FOO3>
    !!</xMl_FoO>
    !!<xMl_FoO2 needed="still nothing new to report here">
    !!     "more blah"
    !!     "more blah"
    !!     <xml_FOO3>
    !!         4
    !!         8 8
    !!         4.0000000000000000
    !!         8.0000000000000000 8.0000000000000000
    !!         True False
    !!     </xml_FOO3>
    !!</xMl_FoO2>
    !!<xMl_FoO2 needed="still nothing new to report here">
    !!     "more blah"
    !!     "more blah"
    !!     <xml_FOO3>
    !!         4
    !!         8 8
    !!         4.0000000000000000
    !!         8.0000000000000000 8.0000000000000000
    !!         True False
    !!     </xml_FOO3>
    !!</xMl_FoO2>
    !!
    !!
    TYPE(xml_element_dt) :: foo, foo1, foo2, foo3
    TYPE(xml_file_dt) :: xml_file

    CALL foo%setup('xMl_FoO','',offset=3)

    CALL foo1%setup('xMl_FoO1','needed="additional_data"',9)
    CALL foo1%add('blah')
    CALL foo1%add('blah')
    CALL foo1%add('blah')
    CALL foo%add(foo1)
    CALL foo2%setup('xMl_FoO2','needed="nothing new to report here"')
    CALL foo2%add('more blah')
    CALL foo2%add('more blah')
    CALL foo%add(foo2)

    CALL foo2%setup('xMl_FoO2','needed="still nothing new to report here"',5)
    CALL foo2%add('more blah')
    CALL foo2%add('more blah')

    CALL foo3%setup('xml_FOO3')
    CALL foo3%add([ 4_i4k ])
    CALL foo3%add([ 8_i8k, 8_i8k ])
    CALL foo3%add([ 4.0_r8k ])
    CALL foo3%add([ 8.0_r8k, 8.0_r8k ])
    CALL foo3%add([.true., .false.])

    CALL foo2%add(foo3)
    CALL foo%add(foo2)
    CALL foo%add(foo3)

    CALL xml_file%setup(filename='xml_test.xml')
    CALL xml_file%add(foo)
    CALL xml_file%add(foo2)
    CALL xml_file%add(foo2)

    CALL xml_file%write()

    CALL all_tests_pass()

END PROGRAM xml_test
