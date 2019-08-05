MODULE XML
    USE Precision,       ONLY : i1k, i4k, i8k, r4k, r8k
    USE File_utility,    ONLY : file_data_structure
    USE ISO_FORTRAN_ENV, ONLY : output_unit
    IMPLICIT NONE
    !! author: Ian Porter
    !! date: 04/02/2019
    !!
    !! This is a simple xml format writer
    !!

    PRIVATE
    PUBLIC :: xml_element_dt, xml_file_dt, gcc_bug_workaround_allocate, gcc_bug_workaround_deallocate
    PUBLIC :: file_format, binary, ascii, convert_format_to_string, convert_string_to_format
    PUBLIC :: format_ascii, format_binary, format_append, file_format_text

    ENUM, BIND(C)
        ENUMERATOR :: ascii, binary, append
    END ENUM

    INTEGER(i4k) :: file_format = ascii

    CHARACTER(LEN=*), PARAMETER :: format_ascii  = 'ascii'
    CHARACTER(LEN=*), PARAMETER :: format_binary = 'binary'
    CHARACTER(LEN=*), PARAMETER :: format_append = 'appended'
    CHARACTER(LEN=:), ALLOCATABLE :: file_format_text

    TYPE string_dt
        CHARACTER(LEN=:), ALLOCATABLE :: text
    CONTAINS
        PROCEDURE, PRIVATE :: gcc_bug_deallocate_string_dt
        GENERIC, PUBLIC    :: deallocate => gcc_bug_deallocate_string_dt
    END TYPE string_dt

    TYPE real32_dt
        REAL(r4k), DIMENSION(:), ALLOCATABLE :: val
    END TYPE real32_dt

    TYPE real64_dt
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: val
    END TYPE real64_dt

    TYPE xml_element_dt
        PRIVATE
        !! XML derived type
        CHARACTER(LEN=:), ALLOCATABLE :: name         !! Name of the XML block
        INTEGER(i4k) :: unit = output_unit            !! File unit #
        CHARACTER(LEN=:), ALLOCATABLE :: offset       !! Offset for data within XML block
        CHARACTER(LEN=:), ALLOCATABLE :: additional_data !! Additional data to write in header
        TYPE(string_dt),      DIMENSION(:), ALLOCATABLE :: string  !! String data set(s) within element
        TYPE(real32_dt),      DIMENSION(:), ALLOCATABLE :: real32  !! String of real64
        TYPE(real64_dt),      DIMENSION(:), ALLOCATABLE :: real64  !! String of real64
        TYPE(xml_element_dt), DIMENSION(:), ALLOCATABLE :: element !! Element data set(s) within element
    CONTAINS
        PROCEDURE, PUBLIC  :: setup => element_setup   !! Set up element block
        PROCEDURE, PRIVATE :: begin => element_begin   !! Write open of element block
        PROCEDURE, PRIVATE :: element_add_data         !! Write raw data inside of element block
        PROCEDURE, PRIVATE :: element_add_element      !! Write another element inside element block
        PROCEDURE, PRIVATE :: element_add_real32       !! Write real32 into a string inside of element block
        PROCEDURE, PRIVATE :: element_add_real64       !! Write real64 into a string inside of element block
        PROCEDURE, PRIVATE :: element_add_int32        !! Write ints32 into a string inside of element block
        PROCEDURE, PRIVATE :: element_add_int64        !! Write ints64 into a string inside of element block
        PROCEDURE, PRIVATE :: element_add_logical      !! Write logical into a string inside of element block
        GENERIC, PUBLIC    :: add   => element_add_data
        GENERIC, PUBLIC    :: add   => element_add_element
        GENERIC, PUBLIC    :: add   => element_add_real64
        GENERIC, PUBLIC    :: add   => element_add_real32
        GENERIC, PUBLIC    :: add   => element_add_int64
        GENERIC, PUBLIC    :: add   => element_add_int32
        GENERIC, PUBLIC    :: add   => element_add_logical
        PROCEDURE, PRIVATE :: end   => element_end     !! Write closure of element block
        PROCEDURE, PUBLIC  :: write => element_write   !! Writes the element block
        PROCEDURE, PUBLIC  :: replace => replace_in_string !! Replaces an identifier in the string
        PROCEDURE, PRIVATE :: gcc_bug_workaround_deallocate_single
        GENERIC, PUBLIC    :: deallocate => gcc_bug_workaround_deallocate_single
    END TYPE xml_element_dt

    TYPE, EXTENDS(file_data_structure) :: xml_file_dt
        PRIVATE
        !! Full XML file DT
        TYPE(xml_element_dt), DIMENSION(:), ALLOCATABLE, PUBLIC :: element
    CONTAINS
        PROCEDURE :: setup_file_information => xml_file_setup
        PROCEDURE, PRIVATE :: begin => xml_begin
        PROCEDURE, PUBLIC  :: add   => xml_add
        PROCEDURE, PRIVATE :: end   => xml_end
        PROCEDURE, PUBLIC  :: write => xml_write
        PROCEDURE, PRIVATE :: gcc_bug_workaround_deallocate_xml_file_dt
        GENERIC,   PUBLIC  :: deallocate => gcc_bug_workaround_deallocate_xml_file_dt
    END TYPE xml_file_dt

    INTERFACE gcc_bug_workaround_deallocate
        PROCEDURE :: gcc_bug_workaround_deallocate_array
        PROCEDURE :: gcc_bug_workaround_deallocate_single
    END INTERFACE gcc_bug_workaround_deallocate

    INTERFACE

        MODULE SUBROUTINE element_setup (me, name, string, offset)
        IMPLICIT NONE
        !! This sets up the information needed to define the XML element block
        CLASS(xml_element_dt), INTENT(INOUT) :: me     !! XML element derived type
        CHARACTER(LEN=*),      INTENT(IN)    :: name   !! Name of the XML block
        CHARACTER(LEN=*),      INTENT(IN), OPTIONAL :: string !! String of additional data to write
        INTEGER(i4k),          INTENT(IN), OPTIONAL :: offset !! # of leading spaces inside XML block
        END SUBROUTINE element_setup

        MODULE SUBROUTINE element_begin (me, unit)
        IMPLICIT NONE
        !! This begins an xml element block
        CLASS(xml_element_dt), INTENT(IN) :: me      !! XML element derived type
        INTEGER(i4k),          INTENT(IN) :: unit    !! File unit # to write to
        END SUBROUTINE element_begin

        RECURSIVE MODULE SUBROUTINE element_add_real32 (me, var)
        IMPLICIT NONE
        !! This adds real double precision data inside of an xml element block
        CLASS(xml_element_dt),   INTENT(INOUT) :: me    !! XML element derived type
        REAL(r4k), DIMENSION(:), INTENT(IN)    :: var   !! Data to write
        END SUBROUTINE element_add_real32

        RECURSIVE MODULE SUBROUTINE element_add_real64 (me, var)
        IMPLICIT NONE
        !! This adds real double precision data inside of an xml element block
        CLASS(xml_element_dt),   INTENT(INOUT) :: me    !! XML element derived type
        REAL(r8k), DIMENSION(:), INTENT(IN)    :: var   !! Data to write
        END SUBROUTINE element_add_real64

        RECURSIVE MODULE SUBROUTINE element_add_int32 (me, var)
        IMPLICIT NONE
        !! This adds real double precision data inside of an xml element block
        CLASS(xml_element_dt),      INTENT(INOUT) :: me    !! XML element derived type
        INTEGER(i4k), DIMENSION(:), INTENT(IN)    :: var   !! Data to write
        END SUBROUTINE element_add_int32

        RECURSIVE MODULE SUBROUTINE element_add_int64 (me, var)
        IMPLICIT NONE
        !! This adds real double precision data inside of an xml element block
        CLASS(xml_element_dt),      INTENT(INOUT) :: me    !! XML element derived type
        INTEGER(i8k), DIMENSION(:), INTENT(IN)    :: var   !! Data to write
        END SUBROUTINE element_add_int64

        RECURSIVE MODULE SUBROUTINE element_add_logical (me, var)
        IMPLICIT NONE
        !! This adds real double precision data inside of an xml element block
        CLASS(xml_element_dt), INTENT(INOUT) :: me    !! XML element derived type
        LOGICAL, DIMENSION(:), INTENT(IN)    :: var   !! Data to write
        END SUBROUTINE element_add_logical

        RECURSIVE MODULE SUBROUTINE element_add_data (me, string)
        IMPLICIT NONE
        !! This adds data inside of an xml element block
        CLASS(xml_element_dt), INTENT(INOUT) :: me      !! XML element derived type
        CHARACTER(LEN=*),      INTENT(IN)    :: string  !! String of data to write
        END SUBROUTINE element_add_data

        RECURSIVE MODULE SUBROUTINE element_add_element (me, element)
        IMPLICIT NONE
        !! This adds an element inside of an xml element block
        CLASS(xml_element_dt), INTENT(INOUT) :: me       !! XML element derived type
        CLASS(xml_element_dt), INTENT(IN)    :: element  !! Inner XML element
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

        MODULE SUBROUTINE replace_in_string (me, tag, value)
        IMPLICIT NONE
        !! Replaces the existing value associated with tag with value
        CLASS(xml_element_dt), INTENT(INOUT) :: me
        CHARACTER(LEN=*),      INTENT(IN)    :: tag
        CHARACTER(LEN=*),      INTENT(IN)    :: value
        END SUBROUTINE replace_in_string

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

        RECURSIVE MODULE SUBROUTINE xml_begin (me)
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 05/03/2019
        !!
        !! Begins the writing of the XMl file
        !!
        CLASS(xml_file_dt), INTENT(INOUT) :: me                  !! XML file DT
        END SUBROUTINE xml_begin

        RECURSIVE MODULE SUBROUTINE xml_add (me, element)
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 05/03/2019
        !!
        !! This adds data inside of the file
        !!
        CLASS(xml_file_dt),    INTENT(INOUT) :: me               !! XML file DT
        CLASS(xml_element_dt), INTENT(IN)    :: element
        END SUBROUTINE xml_add

        RECURSIVE MODULE SUBROUTINE xml_end (me)
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 05/03/2019
        !!
        !! Ends the writing of the XMl file
        !!
        CLASS(xml_file_dt), INTENT(IN) :: me                     !! XML file DT
        END SUBROUTINE xml_end

        RECURSIVE MODULE SUBROUTINE xml_write (me)
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 05/03/2019
        !!
        !! Writes the XMl file
        !!
        CLASS(xml_file_dt), INTENT(INOUT) :: me                  !! XML file DT
        END SUBROUTINE xml_write

        RECURSIVE MODULE SUBROUTINE gcc_bug_workaround_allocate (me, addfoo, oldfoo)
        IMPLICIT NONE
        !! gcc Work-around for allocating a multi-dimension derived type w/ allocatable character strings
        !! when trying to increase the size of the foo array by 1
        TYPE(xml_element_dt), DIMENSION(:), INTENT(INOUT), ALLOCATABLE :: me      !! DT to be resized to [oldfoo, addfoo]
        TYPE(xml_element_dt), DIMENSION(:), INTENT(IN), OPTIONAL       :: oldfoo  !! Old array of DTs
        TYPE(xml_element_dt),               INTENT(IN), OPTIONAL       :: addfoo  !! New DT to add to array
        END SUBROUTINE gcc_bug_workaround_allocate

        RECURSIVE MODULE SUBROUTINE gcc_bug_workaround_deallocate_array (me)
        IMPLICIT NONE
        !! gcc Work-around for deallocating a multi-dimension derived type w/ allocatable character strings
        TYPE(xml_element_dt), DIMENSION(:), INTENT(INOUT), ALLOCATABLE :: me
        END SUBROUTINE gcc_bug_workaround_deallocate_array

        RECURSIVE MODULE SUBROUTINE gcc_bug_workaround_deallocate_single (me)
        IMPLICIT NONE
        !! gcc Work-around for deallocating a multi-dimension derived type w/ allocatable character strings
        CLASS(xml_element_dt), INTENT(INOUT) :: me
        END SUBROUTINE gcc_bug_workaround_deallocate_single

        RECURSIVE MODULE SUBROUTINE gcc_bug_deallocate_string_dt (me)
        IMPLICIT NONE
        !! gcc Work-around
        CLASS(string_dt), INTENT(INOUT) :: me
        END SUBROUTINE gcc_bug_deallocate_string_dt

        RECURSIVE MODULE SUBROUTINE gcc_bug_workaround_deallocate_xml_file_dt (me)
        IMPLICIT NONE
        !! gcc Work-around
        CLASS(xml_file_dt), INTENT(INOUT) :: me
        END SUBROUTINE gcc_bug_workaround_deallocate_xml_file_dt

        MODULE FUNCTION convert_format_to_string (format) RESULT(string)
        IMPLICIT NONE
        !! Converts the format integer to string
        INTEGER(i4k), INTENT(IN) :: format
        CHARACTER(LEN=:), ALLOCATABLE :: string
        END FUNCTION convert_format_to_string

        MODULE FUNCTION convert_string_to_format (string) RESULT(format)
        IMPLICIT NONE
        !! Converts the format integer to string
        CHARACTER(LEN=*), INTENT(IN) :: string
        INTEGER(i4k) :: format
        END FUNCTION convert_string_to_format

    END INTERFACE

END MODULE XML
