SUBMODULE (VTK_Serial_file) VTK_Serial_file_implementation
    IMPLICIT NONE
    !! author: Ian Porter
    !! date: 05/06/2019
    !!
    !! This is the basic file for a serial VTK file
    !!

    CONTAINS

        MODULE PROCEDURE finalize
        USE XML, ONLY : xml_element_dt
        IMPLICIT NONE
        !! Writes data inside of itself
        TYPE(VTK_element_dt) :: grid

        IF (ALLOCATED(me%piece)) THEN
            CALL me%piece%finalize()
            CALL grid%setup(name=me%grid_type,string= "WholeExtent=" // '"' // me%WholeExtent // '"')
            CALL grid%add(me%piece)
            CALL me%add(grid)
            CALL grid%me_deallocate()
        END IF

        END PROCEDURE finalize

        MODULE PROCEDURE vtk_dataset_deallocate
        IMPLICIT NONE
        !! gcc Work-around for deallocating a multi-dimension derived type w/ allocatable character strings

        IF (ALLOCATED(foo%piece)) CALL foo%piece%piece_deallocate()

        CALL foo%me_deallocate()

        END PROCEDURE vtk_dataset_deallocate

        MODULE PROCEDURE deallocate_VTK_file_dt
        IMPLICIT NONE
        !! gcc Work-around for deallocating a multi-dimension derived type w/ allocatable character strings

        IF (ALLOCATED(foo%vtk_dataset)) CALL foo%vtk_dataset%vtk_dataset_deallocate()

        CALL foo%deallocate() !! Deallocate the xml data

        END PROCEDURE deallocate_VTK_file_dt

END SUBMODULE VTK_Serial_file_implementation
