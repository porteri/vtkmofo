SUBMODULE (VTK_piece_element) VTK_piece_element_implementation
    USE XML, ONLY : xml_file_dt, xml_element_dt
    IMPLICIT NONE
    !! author: Ian Porter
    !! date: 06/07/2019
    !!
    !! This is the basic file piece elements
    !!
    CONTAINS

        MODULE PROCEDURE vtk_element_setup
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 06/07/2019
        !!
        !! This converts the VTK_element_dt header into XML format
        !!
        CLASS(VTK_element_dt), INTENT(INOUT) :: me

        END PROCEDURE vtk_element_setup

END SUBMODULE VTK_piece_element_implementation
