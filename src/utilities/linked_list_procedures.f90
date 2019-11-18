submodule (linked_list) linked_list_procedures
    use precision, only : i8k
    implicit none
    !! author: taken from examples in metcalf, modern fortran explained, 2008
    !!         modified by Ian Porter
    !! date: 11/9/2018
    !!
    !! module for a list type that can contain items with any value.
    !!
    !! note: a list item can be in at most one list at a time.
    !!

contains

    module procedure newitem
        !! create a new (orphaned) list item.
        implicit none

        allocate (newitem)
        allocate (newitem%value, source=something)

        newitem%prevptr => newitem

    end procedure

    module procedure count_list
        implicit none
        !! count how many items there are in a list.
        class(anyitem), pointer :: p

        count_list = 0
        p => list%firstptr

        do while (associated(p))
            count_list = count_list + 1
            p => p%nextptr
        end do

    end procedure

    module procedure delete_list
        implicit none
        !! delete the contents of a list.

        do while (associated(list%firstptr))
            call delete(list%firstptr)
        end do

    end procedure

    module procedure first
        implicit none
        !! return the first element of a list.

        first => list%firstptr

    end procedure

    module procedure last
        implicit none
        !! return the last element of a list

        last => list%firstptr
        if (associated(last)) last => last%prevptr

    end procedure

    module procedure prepend
        implicit none
        !! insert an item at the beginning of a list.

        print *, 'in prepend'
        if (associated(item%upptr)) call remove(item)
        print *, 'just passed remove'
        item%upptr => list
        if (associated(list%firstptr)) then
            item%prevptr => list%firstptr%prevptr
            item%nextptr => list%firstptr
            list%firstptr%prevptr => item
        else
            item%prevptr => item
        end if
        list%firstptr => item

    end procedure

    module procedure append
        implicit none
        !! append an item to a list.
        class(anyitem), pointer :: last

        if (associated(item%upptr)) call remove (item)
        item%upptr => list
        if (associated(list%firstptr)) then
            last => list%firstptr%prevptr
            last%nextptr => item
            item%prevptr => last
            list%firstptr%prevptr => item
        else
            list%firstptr => item
            item%prevptr => item
        end if

    end procedure

    module procedure print_list
        implicit none
        !! print the items in a list.
        class(anyitem), pointer :: p
        integer :: i
        logical :: show_numbers

        if (present(show_item_numbers)) then
            show_numbers = show_item_numbers
        else
            show_numbers = .true.
        end if
        p => list%firstptr
        if (.not. associated(p)) then
            if (present(show_empty_list)) then
                if (show_empty_list) print *, 'list is empty.'
            else
                print *, 'list is empty.'
            end if
        else
            do i = 1, huge(i)-1
                if (show_numbers) write (*, 1, advance='no') i
1               format(1x, 'item ', i0, ':')
                call p%print
                p => p%nextptr
                if (.not. associated(p)) exit
            end do
        end if

    end procedure

    module procedure change
        implicit none
        !! change the value of an item.

        if (allocated(item%value)) deallocate (item%value)
        allocate (item%value, source=newvalue)

    end procedure

    module procedure delete
        implicit none
        !! delete an item: removes it from the list and deallocates it.
        class(anyitem), pointer :: temp  !!

        temp => item
        call remove(item)
        if (associated(temp)) deallocate (temp)

    end procedure

    module procedure list
        implicit none
        !! return the list that an item is a member of.  null if an orphan.

        list => item%upptr

    end procedure

    module procedure next
        implicit none
        !! return the next item in the list.

        next => item%nextptr

    end procedure

    module procedure prev
        implicit none
        !! return the previous item in the list,
        !! or the last item if this one is the first.

        prev => item%prevptr

    end procedure

    module procedure print
        implicit none
        !! print an item.  this is overridable.
        integer(i8k) :: length

        select type (v => this%value)
        type is (character(len=*))
            length = len(v)
            if (length>40) then
                print 1, length, v(:36)
1               format(1x, 'character(len=', i0, ') = "', a, '"...')
            else
                print *, 'character = "', v, '"'
            end if
        type is (complex)
            print *, 'complex', v
        type is (complex(kind(0d0)))
            print 2, kind(v), v
2           format(1x, 'complex(kind=', i0, ') = (', es23.16, ', ', es23.16, ')')
        type is (real(kind(0d0)))
            print 3, kind(v), v
3           format(1x, 'real(kind=', i0, ') = ', es23.16)
        type is (integer)
            print *, 'integer = ', v
        type is (real)
            print *, 'real = ', v
        type is (logical)
            print *, 'logical = ', v
        class default
            print *, 'unrecognised item type - cannot display value'
        end select

    end procedure

    module procedure remove
        implicit none
        !! remove an item from a list (but keep it and its value).
        class(anylist), pointer :: list

        list => item%upptr
        if (associated(list)) then
            if (associated(item%prevptr, item)) then
                !! single item in list.
                nullify(list%firstptr)
            else if (.not. associated(item%nextptr)) then
                !! last item in list.
                list%firstptr%prevptr => item%prevptr
                nullify(item%prevptr%nextptr)
            else if (associated(list%firstptr, item)) then
                !! first item in list.
                list%firstptr => item%nextptr         ! first = next.
                item%prevptr%prevptr => item%nextptr  ! last%prev = item%next.
                item%nextptr%prevptr => item%prevptr  ! next%prev = last.
            else
                item%prevptr%nextptr => item%nextptr  ! last%next = item%next.
                item%nextptr%prevptr => item%prevptr  ! next%prev = item%last.
            end if
            item%prevptr => item
        end if
        nullify(item%upptr)

    end procedure

end submodule linked_list_procedures
