MODULE XML
    USE Precision,    ONLY : i4k
    USE File_utility, ONLY : file_data_structure
    IMPLICIT NONE
    !! author: Ian Porter
    !! date: 04/02/2019
    !!
    !! This is a simple xml format writer
    !!

    PRIVATE
    PUBLIC :: xml_element_dt, xml_file_dt

    TYPE string_dt
        CHARACTER(LEN=:), ALLOCATABLE :: text
    END TYPE string_dt

    TYPE xml_element_dt
        PRIVATE
        !! XML derived type
        CHARACTER(LEN=:), ALLOCATABLE :: name         !! Name of the XML block
        INTEGER(i4k) :: unit                          !! File unit #
        CHARACTER(LEN=:), ALLOCATABLE :: offset       !! Offset for data within XML block
        CHARACTER(LEN=:), ALLOCATABLE :: additional_data !! Additional data to write in header
        TYPE(string_dt),      DIMENSION(:), ALLOCATABLE :: string  !! String data set(s) within element
        TYPE(xml_element_dt), DIMENSION(:), ALLOCATABLE :: element !! Element data set(s) within element
    CONTAINS
        PROCEDURE, PUBLIC  :: setup => element_setup   !! Set up element block
        PROCEDURE, PRIVATE :: begin => element_begin   !! Write open of element block
        PROCEDURE, PRIVATE :: element_add_data         !! Write raw data inside of element block
        PROCEDURE, PRIVATE :: element_add_element      !! Write another element inside element block
        GENERIC, PUBLIC    :: add   => element_add_data, element_add_element
        PROCEDURE, PRIVATE :: end   => element_end     !! Write closure of element block
        PROCEDURE, PUBLIC  :: write => element_write   !! Writes the element block
    END TYPE xml_element_dt

    TYPE, EXTENDS(file_data_structure) :: xml_file_dt
        !! Full XML file DT
        TYPE(xml_element_dt), DIMENSION(:), ALLOCATABLE :: element
    CONTAINS
        PROCEDURE :: setup_file_information => xml_file_setup
        PROCEDURE, PRIVATE :: begin => xml_begin
        PROCEDURE, PUBLIC  :: add   => xml_add
        PROCEDURE, PRIVATE :: end   => xml_end
        PROCEDURE, PUBLIC  :: write => xml_write
    END TYPE xml_file_dt

    INTERFACE

        MODULE SUBROUTINE element_setup (me, name, string, offset)
        IMPLICIT NONE
        !! This sets up the information needed to define the XML element block
        CLASS(xml_element_dt), INTENT(OUT) :: me     !! XML element derived type
        CHARACTER(LEN=*),      INTENT(IN)  :: name   !! Name of the XML block
        CHARACTER(LEN=*),      INTENT(IN)  :: string !! String of additional data to write
        INTEGER(i4k),          INTENT(IN), OPTIONAL :: offset !! # of leading spaces inside XML block
        END SUBROUTINE element_setup

        MODULE SUBROUTINE element_begin (me, unit)
        IMPLICIT NONE
        !! This begins an xml element block
        CLASS(xml_element_dt), INTENT(IN) :: me      !! XML element derived type
        INTEGER(i4k),          INTENT(IN)  :: unit   !! File unit # to write to
        END SUBROUTINE element_begin

        MODULE SUBROUTINE element_add_data (me, string)
        IMPLICIT NONE
        !! This adds data inside of an xml element block
        CLASS(xml_element_dt), INTENT(INOUT) :: me      !! XML element derived type
        CHARACTER(LEN=*),      INTENT(IN)    :: string  !! String of data to write
        END SUBROUTINE element_add_data

        MODULE SUBROUTINE element_add_element (me, element)
        IMPLICIT NONE
        !! This adds an element inside of an xml element block
        CLASS(xml_element_dt), INTENT(INOUT) :: me       !! XML element derived type
        TYPE(xml_element_dt),  INTENT(IN)    :: element  !! Inner XML element
        END SUBROUTINE element_add_element

        MODULE SUBROUTINE element_end (me, unit)
        IMPLICIT NONE
        !! This ends an XML element block
        CLASS(xml_element_dt), INTENT(IN) :: me      !! XML element derived type
        INTEGER(i4k),          INTENT(IN) :: unit    !! File unit # to write to
        END SUBROUTINE element_end

        RECURSIVE MODULE SUBROUTINE element_write (me, unit)
        IMPLICIT NONE
        !! This writes an XML element block
        CLASS(xml_element_dt), INTENT(IN) :: me      !! XML element derived type
        INTEGER(i4k),          INTENT(IN) :: unit    !! File unit # to write to

        END SUBROUTINE element_write

        MODULE SUBROUTINE xml_file_setup (me, filename, open_status, close_status, form, access)
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 05/02/2019
        !!
        !! Establishes the file information
        !!
        CLASS(xml_file_dt), INTENT(INOUT) :: me                  !! XML file DT
        CHARACTER(LEN=*),   INTENT(IN)    :: filename            !! File name
        CHARACTER(LEN=*),   INTENT(IN), OPTIONAL :: open_status  !! File open status
        CHARACTER(LEN=*),   INTENT(IN), OPTIONAL :: close_status !! File close status
        CHARACTER(LEN=*),   INTENT(IN), OPTIONAL :: form         !! File format (formatted or unformatted)
        CHARACTER(LEN=*),   INTENT(IN), OPTIONAL :: access       !! File access type
        END SUBROUTINE xml_file_setup

        MODULE SUBROUTINE xml_begin (me)
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 05/03/2019
        !!
        !! Begins the writing of the XMl file
        !!
        CLASS(xml_file_dt), INTENT(INOUT) :: me                  !! XML file DT
        END SUBROUTINE xml_begin

        MODULE SUBROUTINE xml_add (me, element)
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 05/03/2019
        !!
        !! This adds data inside of the file
        !!
        CLASS(xml_file_dt),    INTENT(INOUT) :: me               !! XML file DT
        CLASS(xml_element_dt), INTENT(IN)    :: element
        END SUBROUTINE xml_add

        MODULE SUBROUTINE xml_end (me)
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 05/03/2019
        !!
        !! Ends the writing of the XMl file
        !!
        CLASS(xml_file_dt), INTENT(IN) :: me                     !! XML file DT
        END SUBROUTINE xml_end

        MODULE SUBROUTINE xml_write (me)
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 05/03/2019
        !!
        !! Writes the XMl file
        !!
        CLASS(xml_file_dt), INTENT(INOUT) :: me                  !! XML file DT

        END SUBROUTINE xml_write

    END INTERFACE

END MODULE XML
