module linked_list
    use precision, only : i4k
    implicit none
    !! author: taken from examples in metcalf, modern fortran explained, 2008
    !!         modified by Ian Porter
    !! date: 11/9/2018
    !!
    !! module for a list type that can contain items with any value.
    !!
    !! note: a list item can be in at most one list at a time.
    !!

    private
    public :: anylist, anyitem, newitem

    type anylist
        !! header type
        class(anyitem), pointer, private :: firstptr => null()
    contains
        procedure, non_overridable :: append
        procedure, non_overridable :: count_list
        procedure, non_overridable :: delete_list
        procedure, non_overridable :: first
        procedure, non_overridable :: last
        procedure, non_overridable :: prepend
        procedure, non_overridable :: print_list
    end type

    type anyitem
        !! list item type
        class(*), allocatable            :: value
        class(anyitem), pointer, private :: nextptr => null()
        class(anyitem), pointer, private :: prevptr => null()
        class(anylist), pointer, private :: upptr   => null()
    contains
        procedure, non_overridable :: change
        procedure, non_overridable :: delete
        procedure, non_overridable :: list
        procedure, non_overridable :: next
        procedure, non_overridable :: prev
        procedure                  :: print
        procedure, non_overridable :: remove
    end type

    interface

        module function newitem(something)
            implicit none
            !! create a new (orphaned) list item.
            class(*), intent(in)    :: something !!
            class(anyitem), pointer :: newitem   !!

        end function

        module function count_list(list)
            implicit none
            !! count how many items there are in a list.
            class(anylist), intent(in) :: list        !!
            integer(i4k)               :: count_list  !!

        end function

        module subroutine delete_list(list)
            implicit none
            !! delete the contents of a list.
            class(anylist), intent(inout) :: list !!

        end subroutine

        module function first(list)
            implicit none
            !! return the first element of a list.
            class(anylist), intent(in) :: list  !!
            class(anyitem), pointer    :: first !!

        end function

        module function last(list)
            implicit none
            !! return the last element of a list
            class(anylist), intent(in) :: list  !!
            class(anyitem), pointer    :: last  !!

        end function

        module subroutine prepend(list, item)
            implicit none
            !! insert an item at the beginning of a list.
            class(anylist), intent(inout), target :: list  !!
            class(anyitem), target                :: item  !!

        end subroutine

        module subroutine append(list, item)
            implicit none
            !! append an item to a list.
            class(anylist), intent(inout), target :: list  !!
            class(anyitem), target                :: item  !!

        end subroutine

        module subroutine print_list(list, show_item_numbers, show_empty_list)
            implicit none
            !! print the items in a list.
            class(anylist), intent(in) :: list  !!
            logical,        intent(in), optional :: show_item_numbers  !!
            logical,        intent(in), optional :: show_empty_list    !!

        end subroutine

        module subroutine change(item, newvalue)
            implicit none
            !! change the value of an item.
            class(anyitem), intent(inout) :: item     !!
            class(*),       intent(in)    :: newvalue !!

        end subroutine

        module subroutine delete(item)
            implicit none
            !! delete an item: removes it from the list and deallocates it.
            class(anyitem), target  :: item     !!

        end subroutine

        module function list(item)
            implicit none
            !! return the list that an item is a member of.  null if an orphan.
            class(anyitem), intent(in) :: item  !!
            class(anylist), pointer    :: list  !!

        end function

        module function next(item)
            implicit none
            !! return the next item in the list.
            class(anyitem), intent(in) :: item  !!
            class(anyitem), pointer    :: next  !!

        end function

        module function prev(item)
            implicit none
            !! return the previous item in the list,
            !! or the last item if this one is the first.
            class(anyitem), intent(in) :: item  !!
            class(anyitem), pointer    :: prev  !!

        end function

        module subroutine print(this)
            implicit none
            !! print an item.  this is overridable.
            class(anyitem), intent(in) :: this  !!

        end subroutine

        module subroutine remove(item)
            implicit none
            !! remove an item from a list (but keep it and its value).
            class(anyitem), intent(inout), target :: item  !!

        end subroutine

    end interface

end module linked_list
