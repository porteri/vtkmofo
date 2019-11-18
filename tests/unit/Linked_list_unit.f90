program linked_list_unit
    use precision,       only : i4k, r8k
    use linked_list,     only : anylist, anyitem, newitem
    use vtkmofopassfail, only : all_tests_pass
    implicit none
    !! author: Ian Porter
    !! date: 11/09/2018
    !!
    !! unit testing for linked lists
    !!   module:    linked_list
    !!   functions:
    !!
    !! ****************************
    !! unit testing for linked list
    !! ****************************
    !!
    type(anylist) :: list
    class(anyitem), pointer :: p

    type :: foo_1
        !! foo_1 DT
        integer(i4k) :: i
    end type foo_1

    type, extends(foo_1) :: foo_2_dt
        !! foo_2 DT
        real(r8k), dimension(:,:,:), allocatable :: x
    end type foo_2_dt

    type(foo_2_dt) :: foo_2

    ! first demonstrate the most basic workings of a list.
    call list%append(newitem(17))
    call list%append(newitem('world'))
    call list%prepend(newitem('hello'))
    call list%append(newitem(2.25))
    print *, 'the list now has', list%count_list(), 'items.'
    write (*, '(1x, a)', advance='no') 'the first element is: '
    p => list%first()
    call p%print()
    write (*, '(1x, a)', advance='no') 'the last element is: '
    p => list%last()
    call p%print()
    print *, 'after deleting the last element, the list contents are:'
    call p%delete()
    call list%print_list()
    !
    ! now delete the old list and make a new one,
    ! with some values from myitem_list_m.
    !
    !    print *, 'deleting list'
    !    call list%delete_list()   !! todo: there is an error here. found with intel 18.0.5
    !    print *, 'list deleted'
    !    call list%append(newitem('test foo_1 w/ coarray'))
    !    call list%append(newitem(foo_1))
    call list%append(newitem('test foo_2'))
    call list%append(newitem(foo_2))
    print *, 'the contents of our new list are:'
    call list%print_list()
    !
    ! now test some of the other procedures, just to prove they work.
    !
    p => list%first()    !!
    p => p%prev()        !! test prev(), this will be the last item.
    call p%remove        !! remove the last item.
    call list%prepend(p) !! put it back, at the beginning of the list.
    p => p%next()        !! test next(), this will be the second item,
    !! the one with the string "...third.".
    call p%change((0,1)) !! replace it with a complex number.
    print *, 'revised list contents:'
    call list%print_list()
    call list%prepend(p) !! move new item to top
    print *, 'afer moving item 2 to top, list contents:'
    call list%print_list()

    call all_tests_pass()

end program linked_list_unit
