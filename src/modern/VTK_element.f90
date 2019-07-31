MODULE VTK_element
    USE XML,            ONLY : xml_file_dt, xml_element_dt
    USE vtk_attributes, ONLY : attribute, attributes
    USE vtk_datasets,   ONLY : dataset
    IMPLICIT NONE
    !! author: Ian Porter
    !! date: 05/06/2019
    !!
    !! This is the basic component for a VTK element
    !!

    PRIVATE
    PUBLIC :: VTK_element_dt

    CHARACTER(LEN=*), PARAMETER :: def_version = "0.1"
    CHARACTER(LEN=*), PARAMETER :: def_byte_order = "LittleEndian"

    TYPE, EXTENDS(xml_element_dt) :: VTK_element_dt
        PRIVATE
        CHARACTER(LEN=:), ALLOCATABLE :: type
        CHARACTER(LEN=:), ALLOCATABLE :: version
        CHARACTER(LEN=:), ALLOCATABLE :: byte_order
        CHARACTER(LEN=:), ALLOCATABLE :: compression
        CHARACTER(LEN=:), ALLOCATABLE, PUBLIC :: file_extension
        CHARACTER(LEN=:), ALLOCATABLE, PUBLIC :: filename
        TYPE(VTK_element_dt), ALLOCATABLE, PUBLIC :: vtk_element !! Currently handle only one piece
    CONTAINS
        PROCEDURE, NON_OVERRIDABLE :: vtk_element_setup
        PROCEDURE, NON_OVERRIDABLE, PUBLIC :: initialize
        PROCEDURE, PUBLIC :: finalize
        PROCEDURE :: me_deallocate => gcc_bug_workaround_deallocate_vtk_element_single
!        PROCEDURE :: gcc_bug_workaround_deallocate_single => gcc_bug_workaround_deallocate_vtk_element_single
    END TYPE VTK_element_dt

    INTERFACE

        MODULE SUBROUTINE vtk_element_setup (me)
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 05/07/2019
        !!
        !! This converts the VTK_element_dt header into XML format
        !!
        CLASS(VTK_element_dt), INTENT(INOUT) :: me

        END SUBROUTINE vtk_element_setup

        MODULE SUBROUTINE initialize (me, type, byte_order, compression, file_extension)
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 05/07/2019
        !!
        !! This is an external interface to allow the user or other routines to set the internal variables
        !!
        CLASS(VTK_element_dt), INTENT(INOUT)        :: me              !!
        CHARACTER(LEN=*),      INTENT(IN), OPTIONAL :: type            !! Grid type
        CHARACTER(LEN=*),      INTENT(IN), OPTIONAL :: byte_order      !! Byte order (BigEndian or LittleEndian)
        CHARACTER(LEN=*),      INTENT(IN), OPTIONAL :: compression     !!
        CHARACTER(LEN=*),      INTENT(IN), OPTIONAL :: file_extension  !!

        END SUBROUTINE initialize

        RECURSIVE MODULE SUBROUTINE finalize (me)
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 06/07/2019
        !!
        !! This writes the end of the file
        !!
        CLASS(VTK_element_dt), INTENT(INOUT) :: me              !!

        END SUBROUTINE finalize

        RECURSIVE MODULE SUBROUTINE gcc_bug_workaround_deallocate_vtk_element_single (foo)
        IMPLICIT NONE
        !! gcc Work-around for deallocating a multi-dimension derived type w/ allocatable character strings
        CLASS(VTK_element_dt), INTENT(INOUT) :: foo
        END SUBROUTINE gcc_bug_workaround_deallocate_vtk_element_single

    END INTERFACE

END MODULE VTK_element
