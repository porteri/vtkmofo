module misc
    use precision, only : i4k, i8k, r4k, r8k
    implicit none
    !! author: Ian Porter
    !! date: 12/13/2017
    !!
    !! this module contains miscellaneous routines used to read/write to the .vtk file
    !!
    private
    public :: interpret_string, def_len, to_uppercase, to_lowercase, char_dt, sleep_for, convert_to_string, trim_from_string

    interface get_string_value
        procedure :: get_string_char
        procedure :: get_string_int
        procedure :: get_string_real
    end interface

    interface convert_to_string
        procedure :: convert_real32_to_string
        procedure :: convert_real64_to_string
        procedure :: convert_real64_array_to_string
        procedure :: convert_int32_to_string
        procedure :: convert_int64_to_string
        procedure :: convert_logical_to_string
    end interface convert_to_string

    integer(i4k), parameter :: def_len = 1024          !! default character length for each line in file

    type char_dt
        !! character string dt
        character(len=:), allocatable :: text
    end type char_dt

    interface

        module subroutine interpret_string (line, datatype, ignore, separator, reals, ints, chars)
            implicit none
            !! interprets a string (typically read from an input file) into a user-defined # of character and/or integer inputs
            character(len=*), intent(inout) :: line
            character(len=*), intent(in), optional :: ignore
            character(len=*), intent(in), optional :: separator
            character(len=1), dimension(:), intent(in) :: datatype
            integer(i4k),     dimension(:), allocatable, optional :: ints
            real(r8k),        dimension(:), allocatable, optional :: reals
            type(char_dt),    dimension(:), allocatable, optional :: chars

        end subroutine interpret_string

        module subroutine reduce_string (string, sep)
            implicit none
            character(len=:), allocatable, intent(inout) :: string
            character(len=*), intent(in)  :: sep

        end subroutine reduce_string

        module subroutine get_string_char (string, sep, name)
            implicit none
            character(len=*), intent(in)  :: string, sep
            character(len=:), allocatable, intent(out) :: name

        end subroutine get_string_char

        module subroutine get_string_int (string, sep, name)
            implicit none
            character(len=*), intent(in)  :: string, sep
            integer(i4k),     intent(out) :: name

        end subroutine get_string_int

        module subroutine get_string_real (string, sep, name)
            implicit none
            character(len=*), intent(in)  :: string, sep
            real(r8k),        intent(out) :: name

        end subroutine get_string_real

        module function convert_real32_to_string (var) result (string)
            implicit none
            !! converts a real32 to a character string
            real(r4k),        intent(in)  :: var      !! real variable
            character(len=:), allocatable :: string   !! character string
        end function convert_real32_to_string

        module function convert_real64_to_string (var) result (string)
            implicit none
            !! converts a real64 to a character string
            real(r8k),        intent(in)  :: var      !! real variable
            character(len=:), allocatable :: string   !! character string
        end function convert_real64_to_string

        module function convert_real64_array_to_string (var) result (string)
            implicit none
            !! converts a real64 to a character string
            real(r8k), dimension(:), intent(in) :: var      !! real array
            character(len=:),       allocatable :: string   !! character string
        end function convert_real64_array_to_string

        module function convert_int32_to_string (var) result (string)
            implicit none
            !! converts an int32 to a character string
            integer(i4k),     intent(in)  :: var      !! integer variable
            character(len=:), allocatable :: string   !! character string
        end function convert_int32_to_string

        module function convert_int64_to_string (var) result (string)
            implicit none
            !! converts an int64 to a character string
            integer(i8k),     intent(in)  :: var      !! integer variable
            character(len=:), allocatable :: string   !! character string
        end function convert_int64_to_string

        module function convert_logical_to_string (var) result (string)
            implicit none
            !! converts a logical to a character string
            logical,          intent(in)  :: var      !! logical variable
            character(len=:), allocatable :: string   !! character string
        end function convert_logical_to_string

        pure module function to_uppercase (string) result (new_string)
            implicit none
            !! author: Ian Porter
            !! date: 01/23/2019
            !!
            !! this function changes lowercase text in a string to uppercase text
            !!
            character(len=*), intent(in)  :: string
            character(len=:), allocatable :: new_string

        end function to_uppercase

        pure module function to_lowercase (string) result (new_string)
            implicit none
            !! author: Ian Porter
            !! date: 01/23/2019
            !!
            !! this function changes uppercase text in a string to lowercase text
            !!
            character(len=*), intent(in)  :: string
            character(len=:), allocatable :: new_string

        end function to_lowercase

        module subroutine sleep_for (msecs)
            implicit none
            !! author: zaak beekman, paratools
            !! date: 8/8/2018
            !!
            !! this performs a 'sleep' for a specified amount of time
            !!
            integer(i4k), intent(in) :: msecs  !! # of milliseconds to sleep for

        end subroutine

        recursive module function trim_from_string (string, item, case_sensitive) result (new_string)
            implicit none
            !! author: Ian Porter, gse
            !! date: 11/06/2019
            !!
            !! this function trims <item> from a string
            !!
            character(len=*), intent(in)  :: string     !! string to be converted
            character(len=*), intent(in)  :: item       !! item to be trimmed from string
            logical,          intent(in), optional :: case_sensitive
            !! flag for whether or not to search using case sensitivity (false by default)
            character(len=:), allocatable :: new_string !! new string

        end function trim_from_string

    end interface

end module misc
