PROGRAM linked_list_unit
    USE Precision,       ONLY : i4k, r8k
    USE linked_list,     ONLY : anylist, anyitem, newitem
    USE VTKmofoPassFail, ONLY : all_tests_pass
    IMPLICIT NONE
    !! author: Ian Porter
    !! date: 11/09/2018
    !!
    !! Unit testing for linked lists
    !!   Module:    linked_list
    !!   Functions:
    !!
    !! ****************************
    !! Unit Testing for linked list
    !! ****************************
    !!
    TYPE(anylist) :: list
    CLASS(anyitem), POINTER :: p

    TYPE :: foo_1
        INTEGER(i4k) :: i
    END TYPE foo_1

    TYPE, EXTENDS(foo_1) :: foo_2_dt
        REAL(r8k), DIMENSION(:,:,:), ALLOCATABLE :: x
    END TYPE foo_2_dt

    TYPE(foo_2_dt) :: foo_2

    ! First demonstrate the most basic workings of a list.
    CALL list%append(newitem(17))
    CALL list%append(newitem('world'))
    CALL list%prepend(newitem('hello'))
    CALL list%append(newitem(2.25))
    PRINT *, 'The list now has', list%count_list(), 'items.'
    WRITE (*, '(1x, a)', advance='no') 'The first element is: '
    p => list%first()
    CALL p%print()
    WRITE (*, '(1x, a)', advance='no') 'The last element is: '
    p => list%last()
    CALL p%print()
    PRINT *, 'After deleting the last element, the list contents are:'
    CALL p%delete()
    CALL list%print_list()
    !
    ! Now delete the old list and make a new one,
    ! with some values from myitem_list_m.
    !
!    print *, 'deleting list'
!    CALL list%delete_list()   !! TODO: There is an error here. Found with Intel 18.0.5
!    print *, 'list deleted'
!    CALL list%append(newitem('Test foo_1 w/ coarray'))
!    CALL list%append(newitem(foo_1))
    CALL list%append(newitem('Test foo_2'))
    CALL list%append(newitem(foo_2))
    PRINT *, 'The contents of our new list are:'
    CALL list%print_list()
    !
    ! Now test some of the other procedures, just to prove they work.
    !
    p => list%first()    !!
    p => p%prev()        !! Test prev(), this will be the last item.
    CALL p%remove        !! Remove the last item.
    CALL list%prepend(p) !! Put it back, at the beginning of the list.
    p => p%next()        !! Test next(), this will be the second item,
                         !! the one with the string "...third.".
    CALL p%change((0,1)) !! Replace it with a complex number.
    PRINT *, 'Revised list contents:'
    CALL list%print_list()
    CALL list%prepend(p) !! Move new item to top
    PRINT *, 'Afer moving item 2 to top, list contents:'
    CALL list%print_list()

    CALL all_tests_pass()

END PROGRAM linked_list_unit
