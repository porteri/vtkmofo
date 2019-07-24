SUBMODULE (linked_list) linked_list_implementation
    IMPLICIT NONE
    !! author: Taken from examples in Metcalf, Modern Fortran Explained, 2008
    !!         Modified by Ian Porter
    !! date: 11/9/2018
    !!
    !! Module for a list type that can contain items with any value.
    !!
    !! Note: A list item can be in at most one list at a time.
    !!

    CONTAINS

        MODULE PROCEDURE newitem
        !! Create a new (orphaned) list item.

        ALLOCATE (newitem)
        ALLOCATE (newitem%value, source=something)

        newitem%prevptr => newitem

        END PROCEDURE

        MODULE PROCEDURE count_list
        !! Count how many items there are in a list.
        CLASS(anyitem), POINTER :: p

        count_list = 0
        p => list%firstptr

        DO WHILE (ASSOCIATED(p))
            count_list = count_list + 1
            p => p%nextptr
        END DO

        END PROCEDURE

        MODULE PROCEDURE delete_list
        !! Delete the contents of a list.

        DO WHILE (ASSOCIATED(list%firstptr))
            CALL delete(list%firstptr)
        END DO

        END PROCEDURE

        MODULE PROCEDURE first
        !! Return the first element of a list.

        first => list%firstptr

        END PROCEDURE

        MODULE PROCEDURE last
        !! Return the last element of a list

        last => list%firstptr
        IF (ASSOCIATED(last)) last => last%prevptr

        END PROCEDURE

        MODULE PROCEDURE prepend
        !! Insert an item at the beginning of a list.

        PRINT *, 'in prepend'
        IF (ASSOCIATED(item%upptr)) CALL remove(item)
        PRINT *, 'just passed remove'
        item%upptr => list
        IF (ASSOCIATED(list%firstptr)) THEN
            item%prevptr => list%firstptr%prevptr
            item%nextptr => list%firstptr
            list%firstptr%prevptr => item
        ELSE
            item%prevptr => item
        END IF
        list%firstptr => item

        END PROCEDURE

        MODULE PROCEDURE append
        !! Append an item to a list.
        CLASS(anyitem), POINTER :: last

        IF (ASSOCIATED(item%upptr)) CALL remove (item)
        item%upptr => list
        IF (ASSOCIATED(list%firstptr)) THEN
            last => list%firstptr%prevptr
            last%nextptr => item
            item%prevptr => last
            list%firstptr%prevptr => item
        ELSE
            list%firstptr => item
            item%prevptr => item
        END IF

        END PROCEDURE

        MODULE PROCEDURE print_list
        !! Print the items in a list.
        CLASS(anyitem), POINTER :: p
        INTEGER :: i
        LOGICAL :: show_numbers

        IF (PRESENT(show_item_numbers)) THEN
            show_numbers = show_item_numbers
        ELSE
            show_numbers = .TRUE.
        END IF
        p => list%firstptr
        IF (.NOT. ASSOCIATED(p)) THEN
            IF (PRESENT(show_empty_list)) THEN
                IF (show_empty_list) PRINT *, 'List is empty.'
            ELSE
                PRINT *, 'List is empty.'
            END IF
        ELSE
            DO i = 1, HUGE(i)-1
                IF (show_numbers) WRITE (*, 1, advance='no') i
1               FORMAT(1x, 'Item ', i0, ':')
                CALL p%PRINT
                p => p%nextptr
                IF (.NOT. ASSOCIATED(p)) EXIT
            END DO
        END IF

        END PROCEDURE

        MODULE PROCEDURE change
        !! Change the value of an item.

        IF (ALLOCATED(item%value)) DEALLOCATE (item%value)
        ALLOCATE (item%value, source=newvalue)

        END PROCEDURE

        MODULE PROCEDURE delete
        !! Delete an item: removes it from the list and deallocates it.
        CLASS(anyitem), POINTER :: temp  !!

        temp => item
        CALL remove(item)
        IF (ASSOCIATED(temp)) DEALLOCATE (temp)

        END PROCEDURE

        MODULE PROCEDURE list
        !! Return the list that an item is a member of.  Null if an orphan.

        list => item%upptr

        END PROCEDURE

        MODULE PROCEDURE next
        !! Return the next item in the list.

        next => item%nextptr

        END PROCEDURE

        MODULE PROCEDURE prev
        !! Return the previous item in the list,
        !! or the last item if this one is the first.

        prev => item%prevptr

        END PROCEDURE

        MODULE PROCEDURE PRINT
        !! Print an item.  This is overridable.
        INTEGER :: length

        SELECT TYPE (v => this%value)
        TYPE IS (CHARACTER(LEN=*))
            length = len(v)
            IF (length>40) THEN
                PRINT 1, length, v(:36)
1               FORMAT(1x, 'character(len=', i0, ') = "', a, '"...')
            ELSE
                PRINT *, 'character = "', v, '"'
            END IF
        TYPE IS (COMPLEX)
            PRINT *, 'complex', v
        TYPE IS (COMPLEX(KIND(0d0)))
            PRINT 2, KIND(v), v
2           FORMAT(1x, 'complex(kind=', i0, ') = (', es23.16, ', ', es23.16, ')')
        TYPE IS (REAL(KIND(0d0)))
            PRINT 3, KIND(v), v
3           FORMAT(1x, 'real(kind=', i0, ') = ', es23.16)
        TYPE IS (INTEGER)
            PRINT *, 'INTEGER = ', v
        TYPE IS (REAL)
            PRINT *, 'real = ', v
        TYPE IS (LOGICAL)
            PRINT *, 'LOGICAL = ', v
        CLASS DEFAULT
            PRINT *, 'unrecognised item TYPE - cannot display value'
        END SELECT

        END PROCEDURE

        MODULE PROCEDURE remove
        !! Remove an item from a list (but keep it and its value).
        CLASS(anylist), POINTER :: list

        list => item%upptr
        IF (ASSOCIATED(list)) THEN
            IF (ASSOCIATED(item%prevptr, item)) THEN
                !! Single item in list.
                NULLIFY(list%firstptr)
            ELSE IF (.NOT. ASSOCIATED(item%nextptr)) THEN
                !! Last item in list.
                list%firstptr%prevptr => item%prevptr
                NULLIFY(item%prevptr%nextptr)
            ELSE IF (ASSOCIATED(list%firstptr, item)) THEN
                !! First item in list.
                list%firstptr => item%nextptr         ! first = next.
                item%prevptr%prevptr => item%nextptr  ! last%prev = item%next.
                item%nextptr%prevptr => item%prevptr  ! next%prev = last.
            ELSE
                item%prevptr%nextptr => item%nextptr  ! last%next = item%next.
                item%nextptr%prevptr => item%prevptr  ! next%prev = item%last.
            END IF
            item%prevptr => item
        END IF
        NULLIFY(item%upptr)

        END PROCEDURE

END SUBMODULE linked_list_implementation
