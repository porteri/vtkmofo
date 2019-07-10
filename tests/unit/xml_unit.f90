PROGRAM xml_test
    USE VTKmofoPassFail, ONLY : all_tests_pass
    USE XML,             ONLY : xml_element_dt, xml_file_dt
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
    !!        </xml_FOO3>
    !!   </xMl_FoO2>
    !!   <xml_FOO3>
    !!   </xml_FOO3>
    !!</xMl_FoO>
    !!<xMl_FoO2 needed="still nothing new to report here">
    !!     "more blah"
    !!     "more blah"
    !!     <xml_FOO3>
    !!     </xml_FOO3>
    !!</xMl_FoO2>
    !!<xMl_FoO2 needed="still nothing new to report here">
    !!     "more blah"
    !!     "more blah"
    !!     <xml_FOO3>
    !!     </xml_FOO3>
    !!</xMl_FoO2>
    !!
    !!
    TYPE(xml_element_dt) :: foo, foo1, foo2, foo3
    TYPE(xml_file_dt) :: xml_file
write(0,*) 'before foo%setup'
    CALL foo%setup('xMl_FoO','',offset=3)
write(0,*) 'before foo1%setup'
    CALL foo1%setup('xMl_FoO1','needed="additional_data"',9)
    CALL foo1%add('blah')
    CALL foo1%add('blah')
    CALL foo1%add('blah')
write(0,*) 'before foo%add(foo1)'
    CALL foo%add(foo1)
write(0,*) 'before foo2%setup'
    CALL foo2%setup('xMl_FoO2','needed="nothing new to report here"')
    CALL foo2%add('more blah')
    CALL foo2%add('more blah')
    CALL foo%add(foo2)

    CALL foo2%setup('xMl_FoO2','needed="still nothing new to report here"',5)
    CALL foo2%add('more blah')
    CALL foo2%add('more blah')

write(0,*) 'before foo3%setup'
    CALL foo3%setup('xml_FOO3')
    CALL foo2%add(foo3)
write(0,*) 'before foo%add(foo2)'
    CALL foo%add(foo2)
write(0,*) 'before foo%add(foo3)'
    CALL foo%add(foo3)

write(0,*) 'before xml_file%setup'
    CALL xml_file%setup(filename='xml_test.xml')
write(0,*) 'before xml_file%add(foo)'
    CALL xml_file%add(foo)
    CALL xml_file%add(foo2)
write(0,*)
write(0,*) 'before xml_file%add(foo2)'
write(0,*)
    CALL xml_file%add(foo2)
write(0,*)
write(0,*) 'before xml_file%write'
write(0,*)
    CALL xml_file%write()
write(0,*) 'before all_tests_pass'
    CALL all_tests_pass()

END PROGRAM xml_test