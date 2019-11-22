module file_utility
    use precision, only : i4k
    implicit none
    !! author: Ian Porter
    !! date: 04/04/2018
    !!
    !! this module contains a derived type for file information
    !!
    private
    public :: file_data_structure, is_little_endian

    type file_data_structure
        !! file data derived type
        integer(i4k) :: unit
        character(len=:), allocatable :: filename
        character(len=:), allocatable :: open_status
        character(len=:), allocatable :: close_status
        character(len=:), allocatable :: form
        character(len=:), allocatable :: access
        character(len=:), allocatable :: encoding
    contains
        procedure, public  :: setup_file_information
        generic,   public  :: setup => setup_file_information
        procedure, private :: check_if_exists
        generic,   public  :: exists => check_if_exists
        procedure, private :: check_if_open
        procedure, public  :: open_file
        procedure, public  :: close_file
        procedure, public  :: make_file
        procedure, public  :: file_read_error
        procedure, public  :: wait_for_file
        procedure, public  :: get_unit
        procedure, public, nopass :: is_little_endian
    end type file_data_structure

    interface

        module subroutine setup_file_information (me, filename, open_status, close_status, form, access, unit, encoding)
            implicit none
            !! author: Ian Porter
            !! date: 04/16/2018
            !!
            !! establishes the file information
            !!
            class(file_data_structure), intent(inout) :: me                  !! dt
            character(len=*),           intent(in)    :: filename            !! file name
            character(len=*),           intent(in), optional :: open_status  !! file open status
            character(len=*),           intent(in), optional :: close_status !! file close status
            character(len=*),           intent(in), optional :: form         !! file format(formatted or unformatted)
            character(len=*),           intent(in), optional :: access       !! file access type
            integer(i4k),               intent(in), optional :: unit         !! requested file unit #
            character(len=*),           intent(in), optional :: encoding     !! file encoding type

        end subroutine setup_file_information

        module function check_if_exists (me, check) result (file_exists)
            implicit none
            !! author: Ian Porter
            !! date: 04/04/2018
            !!
            !! checks to see if the file exists
            !!
            class(file_data_structure), intent(in)           :: me           !! dt
            character(len=*),           intent(in), optional :: check    !! type of check (filename or unit)
            logical                                          :: file_exists  !! determins if file exists

        end function check_if_exists

        module function check_if_open (me, check) result (is_open)
            implicit none
            !! author: Ian Porter
            !! date: 04/04/2018
            !!
            !! checks to see if the file is open
            !!
            class(file_data_structure), intent(in)           :: me       !! dt
            character(len=*),           intent(in), optional :: check    !! type of check (filename or unit)
            logical                                          :: is_open  !! determins if file is open

        end function check_if_open

        module subroutine open_file (me)
            implicit none
            !! author: Ian Porter
            !! date: 04/04/2018
            !!
            !! opens the file
            !!
            class(file_data_structure), intent(inout) :: me   !! dt

        end subroutine open_file

        module subroutine close_file (me)
            implicit none
            !! author: Ian Porter
            !! date: 10/25/2018
            !!
            !! closes the file
            !!
            class(file_data_structure), intent(in) :: me   !! dt

        end subroutine close_file

        module subroutine make_file (me)
            implicit none
            !! author: Ian Porter
            !! date: 10/26/2018
            !!
            !! makes a new file
            !!
            class(file_data_structure), intent(inout) :: me   !! dt

        end subroutine make_file

        module subroutine file_read_error (me, inputstat)
            implicit none
            !! author: Ian Porter
            !! date: 04/04/2018
            !!
            !! prints that a file i/o error as occurred
            !!
            class(file_data_structure), intent(in) :: me          !! dt
            integer(i4k),               intent(in) :: inputstat   !! file read error #

        end subroutine file_read_error

        module subroutine wait_for_file (me)
            implicit none
            !! author: Ian Porter
            !! date: 04/04/2018
            !!
            !! waits for a file to exist
            !!
            class(file_data_structure), intent(inout) :: me   !! dt

        end subroutine wait_for_file

        module function get_unit (me) result (unit)
            implicit none
            !! author: Ian Porter
            !! date: 04/03/2019
            !!
            !! function returns the unit of a file
            !!
            class(file_data_structure), intent(in) :: me   !! dt
            integer :: unit                                !! file unit #

        end function get_unit

        pure module function is_little_endian() result (is_little)
            implicit none
            !! author: Ian Porter
            !! date: 09/25/2019
            !!
            !! checks the type of bit ordering to determine if the architecture is little endian
            !!
            logical :: is_little !! flag to determine if little endian

        end function is_little_endian

    end interface

end module file_utility
