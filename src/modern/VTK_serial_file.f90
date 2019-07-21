MODULE VTK_Serial_file
    USE XML,            ONLY : xml_file_dt, xml_element_dt
    USE vtk_attributes, ONLY : attribute, attributes
    USE vtk_datasets,   ONLY : dataset
    IMPLICIT NONE
    !! author: Ian Porter
    !! date: 05/06/2019
    !!
    !! This is the basic file for a serial VTK file
    !!

    PRIVATE
    PUBLIC :: serial_file, VTK_element_dt

    CHARACTER(LEN=*), PARAMETER :: def_version = "0.1"
    CHARACTER(LEN=*), PARAMETER :: def_byte_order = "LittleEndian"

    TYPE(xml_file_dt), ALLOCATABLE :: serial_file    !! Serial VTK file
!    TYPE(xml_file_dt) :: parallel_file  !! Parallel VTK file
    !! Parallel file is a TODO for future work

    TYPE, EXTENDS(xml_element_dt), ABSTRACT :: VTK_element_dt
        PRIVATE
        CHARACTER(LEN=:), ALLOCATABLE :: type
        CHARACTER(LEN=:), ALLOCATABLE :: version
        CHARACTER(LEN=:), ALLOCATABLE :: byte_order
        CHARACTER(LEN=:), ALLOCATABLE :: compression
        CHARACTER(LEN=:), ALLOCATABLE, PUBLIC :: file_extension
        CHARACTER(LEN=:), ALLOCATABLE, PUBLIC :: filename
    CONTAINS
        PROCEDURE, NON_OVERRIDABLE :: vtk_element_setup
        PROCEDURE, NON_OVERRIDABLE, PUBLIC :: initialize
        PROCEDURE(abs_set_grid_data), DEFERRED :: set_grid_data
        GENERIC, PUBLIC :: set => set_grid_data
        PROCEDURE, NON_OVERRIDABLE, PUBLIC :: finalize
        PROCEDURE :: deallocate_vtk => gcc_bug_workaround_deallocate_vtk_element_single
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

        MODULE SUBROUTINE abs_set_grid_data (me, geometry, celldata, pointdata, celldatasets, pointdatasets)
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 05/07/2019
        !!
        !! This is a deferred routine for each grid type to implement its own routine to set grid dependent data / info
        !!
        CLASS(VTK_element_dt), INTENT(INOUT) :: me
        CLASS(dataset),    INTENT(IN)           :: geometry   !! DT of geometry to be printed
        CLASS(attribute),  INTENT(IN), OPTIONAL :: celldata   !!
        CLASS(attribute),  INTENT(IN), OPTIONAL :: pointdata  !!
        TYPE(attributes), DIMENSION(:), INTENT(IN), OPTIONAL :: celldatasets  !!
        TYPE(attributes), DIMENSION(:), INTENT(IN), OPTIONAL :: pointdatasets !!

        END SUBROUTINE abs_set_grid_data

        MODULE SUBROUTINE finalize (me)
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 06/07/2019
        !!
        !! This writes the end of the file
        !!
        CLASS(VTK_element_dt), INTENT(IN) :: me              !!

        END SUBROUTINE finalize

        RECURSIVE MODULE SUBROUTINE gcc_bug_workaround_deallocate_vtk_element_single (foo)
        IMPLICIT NONE
        !! gcc Work-around for deallocating a multi-dimension derived type w/ allocatable character strings
        CLASS(VTK_element_dt), INTENT(INOUT) :: foo
        END SUBROUTINE gcc_bug_workaround_deallocate_vtk_element_single

    END INTERFACE

END MODULE VTK_Serial_file
