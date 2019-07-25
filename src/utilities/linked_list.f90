MODULE linked_list
    IMPLICIT NONE
    !! author: Taken from examples in Metcalf, Modern Fortran Explained, 2008
    !!         Modified by Ian Porter
    !! date: 11/9/2018
    !!
    !! Module for a list type that can contain items with any value.
    !!
    !! Note: A list item can be in at most one list at a time.
    !!

    PRIVATE
    PUBLIC :: anylist, anyitem, newitem

    TYPE anylist
        !! Header type
        CLASS(anyitem), POINTER, PRIVATE :: firstptr => NULL()
    CONTAINS
        PROCEDURE, NON_OVERRIDABLE :: append
        PROCEDURE, NON_OVERRIDABLE :: count_list
        PROCEDURE, NON_OVERRIDABLE :: delete_list
        PROCEDURE, NON_OVERRIDABLE :: first
        PROCEDURE, NON_OVERRIDABLE :: last
        PROCEDURE, NON_OVERRIDABLE :: prepend
        PROCEDURE, NON_OVERRIDABLE :: print_list
    END TYPE

    TYPE anyitem
        !! List item type
        CLASS(*), ALLOCATABLE            :: value
        CLASS(anyitem), POINTER, PRIVATE :: nextptr => NULL()
        CLASS(anyitem), POINTER, PRIVATE :: prevptr => NULL()
        CLASS(anylist), POINTER, PRIVATE :: upptr   => NULL()
    CONTAINS
        PROCEDURE, NON_OVERRIDABLE :: change
        PROCEDURE, NON_OVERRIDABLE :: delete
        PROCEDURE, NON_OVERRIDABLE :: list
        PROCEDURE, NON_OVERRIDABLE :: next
        PROCEDURE, NON_OVERRIDABLE :: prev
        PROCEDURE                  :: print
        PROCEDURE, NON_OVERRIDABLE :: remove
    END TYPE

    INTERFACE

        MODULE FUNCTION newitem(something)
        !! Create a new (orphaned) list item.
        CLASS(*), INTENT(IN)    :: something !!
        CLASS(anyitem), POINTER :: newitem   !!

        END FUNCTION

        MODULE FUNCTION count_list(list)
        USE ISO_FORTRAN_ENV, ONLY : ipk => int32
        !! Count how many items there are in a list.
        CLASS(anylist), INTENT(IN) :: list        !!
        INTEGER(ipk)               :: count_list  !!

        END FUNCTION

        MODULE SUBROUTINE delete_list(list)
        !! Delete the contents of a list.
        CLASS(anylist), INTENT(INOUT) :: list !!

        END SUBROUTINE

        MODULE FUNCTION first(list)
        !! Return the first element of a list.
        CLASS(anylist), INTENT(IN) :: list  !!
        CLASS(anyitem), POINTER    :: first !!

        END FUNCTION

        MODULE FUNCTION last(list)
        !! Return the last element of a list
        CLASS(anylist), INTENT(IN) :: list  !!
        CLASS(anyitem), POINTER    :: last  !!

        END FUNCTION

        MODULE SUBROUTINE prepend(list, item)
        !! Insert an item at the beginning of a list.
        CLASS(anylist), INTENT(INOUT), TARGET :: list  !!
        CLASS(anyitem), TARGET                :: item  !!

        END SUBROUTINE

        MODULE SUBROUTINE append(list, item)
        !! Append an item to a list.
        CLASS(anylist), INTENT(INOUT), TARGET :: list  !!
        CLASS(anyitem), TARGET                :: item  !!

        END SUBROUTINE

        MODULE SUBROUTINE print_list(list, show_item_numbers, show_empty_list)
        !! Print the items in a list.
        CLASS(anylist), INTENT(IN) :: list  !!
        LOGICAL,        INTENT(IN), OPTIONAL :: show_item_numbers  !!
        LOGICAL,        INTENT(IN), OPTIONAL :: show_empty_list    !!

        END SUBROUTINE

        MODULE SUBROUTINE change(item, newvalue)
        !! Change the value of an item.
        CLASS(anyitem), INTENT(INOUT) :: item     !!
        CLASS(*),       INTENT(IN)    :: newvalue !!

        END SUBROUTINE

        MODULE SUBROUTINE delete(item)
        !! Delete an item: removes it from the list and deallocates it.
        CLASS(anyitem), TARGET  :: item     !!

        END SUBROUTINE

        MODULE FUNCTION list(item)
        !! Return the list that an item is a member of.  Null if an orphan.
        CLASS(anyitem), INTENT(IN) :: item  !!
        CLASS(anylist), POINTER    :: list  !!

        END FUNCTION

        MODULE FUNCTION next(item)
        !! Return the next item in the list.
        CLASS(anyitem), INTENT(IN) :: item  !!
        CLASS(anyitem), POINTER    :: next  !!

        END FUNCTION

        MODULE FUNCTION prev(item)
        !! Return the previous item in the list,
        !! or the last item IF this one is the first.
        CLASS(anyitem), INTENT(IN) :: item  !!
        CLASS(anyitem), POINTER    :: prev  !!

        END FUNCTION

        MODULE SUBROUTINE PRINT(this)
        !! Print an item.  This is overridable.
        CLASS(anyitem), INTENT(IN) :: this  !!

        END SUBROUTINE

        MODULE SUBROUTINE remove(item)
        !! Remove an item from a list (but keep it and its value).
        CLASS(anyitem), INTENT(INOUT), TARGET :: item

        END SUBROUTINE

    END INTERFACE

END MODULE linked_list
