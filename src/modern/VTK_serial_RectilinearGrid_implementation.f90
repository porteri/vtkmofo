SUBMODULE (VTK_serial_RectilinearGrid) RectilinearGrid_sub
    !! author: Ian Porter
    !! date: 05/06/2019
    !!
    !! This submodule implements the procedures for a serial Rectilinear Grid
    !!

    CHARACTER(LEN=*), PARAMETER :: file_extension = ".vtr"
    CHARACTER(LEN=*), PARAMETER :: grid_type = "RectilinearGrid"

    CONTAINS

        MODULE PROCEDURE set_grid_data
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 05/06/2019
        !!
        !! This sets parameters specific to the DT
        !!

        CALL me%initialize(type=grid_type,file_extension=file_extension)

    END PROCEDURE set_grid_data

    
    MODULE PROCEDURE rectilinear_grid_write

    TYPE(xml_element_dt) :: foo, foo1, foo2
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

    CALL foo%add(foo2)
    
    
    

    CALL me%setup(filename='xml_test.xml')
    CALL me%add(all_data)
    CALL xml_file%write()

    END PROCEDURE rectilinear_grid_write

END SUBMODULE RectilinearGrid_sub
