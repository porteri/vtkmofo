PROGRAM xml_test
    USE VTKmofoPassFail, ONLY : all_tests_pass
    USE XML,             ONLY : xml_dt
    IMPLICIT NONE
    !! author: Ian Porter
    !! date: 05/02/2019
    !!
    !! XML derived type unit test
    !!
    !! This test will produce the following output:
    !!
    !!<xMl_FoO>
    !!   <xMl_FoO1 additional_data>
    !!            blah
    !!            blah
    !!            blah
    !!   </xMl_FoO1>
    !!   <xMl_FoO2 nothing new to report here>
    !!        more blah
    !!        more blah
    !!   </xMl_FoO2>
    !!   <xMl_FoO2 nothing new to report here>
    !!        more blah
    !!        more blah
    !!   </xMl_FoO2>
    !!</xMl_FoO>
    !!
    INTEGER :: unit
    TYPE(xml_dt) :: foo, foo1, foo2

    OPEN(newUnit=unit,file='xml_test.xml',form='formatted',status='replace',access='stream')
    CALL foo%setup('xMl_FoO',unit,3)
    CALL foo%begin('')
    CALL foo1%setup('xMl_FoO1',unit,9)
    CALL foo1%begin('additional_data')
    CALL foo1%add('blah')
    CALL foo1%add('blah')
    CALL foo1%add('blah')
    CALL foo1%end()
    CALL foo2%setup('xMl_FoO2',unit,5)
    CALL foo2%begin('nothing new to report here')
    CALL foo2%add('more blah')
    CALL foo2%add('more blah')
    CALL foo2%end()
    CALL foo2%begin('nothing new to report here')
    CALL foo2%add('more blah')
    CALL foo2%add('more blah')
    CALL foo2%end()
    CALL foo%end()

    CALL all_tests_pass()

END PROGRAM xml_test
