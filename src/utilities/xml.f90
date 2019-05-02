MODULE XML
    USE Precision, ONLY : i4k
    IMPLICIT NONE
    !! author: Ian Porter
    !! date: 04/02/2019
    !!
    !! This is a simple xml format writer
    !!

    PRIVATE
    PUBLIC :: xml_dt

    TYPE xml_dt
        PRIVATE
        !! XML derived type
        CHARACTER(LEN=:), ALLOCATABLE :: name   !! Name of the XML block
        INTEGER(i4k) :: unit                    !! File unit #
        CHARACTER(LEN=:), ALLOCATABLE :: offset !! Offset for data within XML block
    CONTAINS
        PROCEDURE, PUBLIC :: setup              !! Set up XML block
        PROCEDURE, PUBLIC :: begin              !! Write open of XML block
        PROCEDURE, PUBLIC :: add                !! Write data inside of XML block
        PROCEDURE, PUBLIC :: end                !! Write closure of XML block
    END TYPE xml_dt

    INTERFACE

        MODULE SUBROUTINE setup (me, name, unit, offset)
        IMPLICIT NONE
        !! This sets up the information needed to define the XML block
        CLASS(xml_dt),    INTENT(OUT) :: me     !! XML derived type
        CHARACTER(LEN=*), INTENT(IN)  :: name   !! Name of the XML block
        INTEGER(i4k),     INTENT(IN)  :: unit   !! File unit # to write to
        INTEGER(i4k),     INTENT(IN), OPTIONAL :: offset !! # of leading spaces inside XML block
        END SUBROUTINE setup

        MODULE SUBROUTINE begin (me, string)
        IMPLICIT NONE
        !! This begins an xml block
        CLASS(xml_dt),    INTENT(IN) :: me      !! XML derived type
        CHARACTER(LEN=*), INTENT(IN) :: string  !! String of additional data to write
        END SUBROUTINE begin

        MODULE SUBROUTINE add (me, string)
        IMPLICIT NONE
        !! This adds data inside of an xml block
        CLASS(xml_dt),    INTENT(IN) :: me      !! XML derived type
        CHARACTER(LEN=*), INTENT(IN) :: string  !! String of data to write
        END SUBROUTINE add

        MODULE SUBROUTINE end (me)
        IMPLICIT NONE
        !! This ends an XML block
        CLASS(xml_dt),    INTENT(IN) :: me      !! XML derived type
        END SUBROUTINE end

    END INTERFACE

END MODULE XML
