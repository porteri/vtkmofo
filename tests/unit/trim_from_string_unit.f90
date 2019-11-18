program trim_from_string_unit
    use precision,       only : i4k
    use vtkmofopassfail, only : testid, all_tests_pass
    use misc,            only : trim_from_string
    implicit none
    !! author: Ian Porter
    !! date: 11/06/2019
    !!
    !! unit testing for function trim_from_string
    !!     module:    misc
    !!     functions: trim_from_string
    integer(i4k) :: cnt
    integer(i4k), parameter :: num_tests = 6_i4k
    logical, dimension(num_tests) :: test_passed = .false.
    character(len=:), allocatable :: string_1, string_2, string_3, string_4, string_5, string_6
    character(len=*), parameter :: input_1 = 'string.txt'
    character(len=*), parameter :: item_1  = 'string.txt'
    character(len=*), parameter :: known_1 = ''
    character(len=*), parameter :: input_2 = 'testing.out'
    character(len=*), parameter :: item_2  = '.out'
    character(len=*), parameter :: known_2 = 'testing'
    character(len=*), parameter :: input_3 = 'string i am testing for'
    character(len=*), parameter :: item_3  = 'string '
    character(len=*), parameter :: known_3 = 'i am testing for'
    character(len=*), parameter :: input_4 = 'i am long'
    character(len=*), parameter :: item_4  = 'i am too long'
    character(len=*), parameter :: known_4 = 'i am long'
    character(len=*), parameter :: input_5 = 'i am long'
    character(len=*), parameter :: item_5  = 'i am'
    character(len=*), parameter :: known_5 = ' long'
    character(len=*), parameter :: input_6 = 'i am long'
    character(len=*), parameter :: item_6  = 'long'
    character(len=*), parameter :: known_6 = 'i am '

    testid = 'trim_from_string_test'

    cnt = 0

    !! test # 1 (should return an empty string)
    cnt = cnt + 1
    string_1 = trim_from_string(input_1,item_1)
    test_passed(1) = (string_1 == known_1)

    !! test # 2 (should remove item_2 from input_2)
    cnt = cnt + 1
    string_2 = trim_from_string(input_2,item_2)
    test_passed(2) = (string_2 == known_2)

    !! test # 3 (should remove item_3 at the start of the string)
    cnt = cnt + 1
    string_3 = trim_from_string(input_3,item_3)
    test_passed(3) = (string_3 == known_3)

    !! test # 4 (should not remove item_4 b/c it's larger than input_4)
    cnt = cnt + 1
    string_4 = trim_from_string(input_4,item_4)
    test_passed(4) = (string_4 == known_4)

    !! test # 5 (should not remove item_5 b/c it's using a case sensitive search)
    cnt = cnt + 1
    string_5 = trim_from_string(input_5,item_5,.true.)
    test_passed(5) = (string_5 == known_5)

    !! test # 6 (should remove item_6 b/c it's using a case insensitive search)
    cnt = cnt + 1
    string_6 = trim_from_string(input_6,item_6,.false.)
    test_passed(6) = (string_6 == known_6)

    if (all(test_passed)) call all_tests_pass()

end program trim_from_string_unit
