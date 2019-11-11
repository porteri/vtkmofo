PROGRAM trim_from_string_unit
    !! summary: unit testing for Function trim_from_string
    !!     Module:    Misc
    !!     Functions: trim_from_string
    !! author: Ian Porter
    !! date: 11/06/2019
    USE Precision,       ONLY : i4k
    USE VTKmofoPassFail, ONLY : TestID, all_tests_pass
    USE Misc,            ONLY : TRIM_FROM_STRING
    IMPLICIT NONE
    ! ******************************************
    ! Unit Testing for Function trim_from_string
    ! ******************************************
    ! Inputs
    ! string
    ! Output
    ! string
    INTEGER(i4k) :: cnt
    INTEGER(i4k), PARAMETER :: num_tests = 6_i4k
    LOGICAL, DIMENSION(num_tests) :: test_passed = .FALSE.
    CHARACTER(LEN=:), ALLOCATABLE :: string_1, string_2, string_3, string_4, string_5, string_6
    CHARACTER(LEN=*), PARAMETER :: input_1 = 'string.txt'
    CHARACTER(LEN=*), PARAMETER :: item_1  = 'string.txt'
    CHARACTER(LEN=*), PARAMETER :: known_1 = ''
    CHARACTER(LEN=*), PARAMETER :: input_2 = 'testing.out'
    CHARACTER(LEN=*), PARAMETER :: item_2  = '.out'
    CHARACTER(LEN=*), PARAMETER :: known_2 = 'testing'
    CHARACTER(LEN=*), PARAMETER :: input_3 = 'string I am testing for'
    CHARACTER(LEN=*), PARAMETER :: item_3  = 'string '
    CHARACTER(LEN=*), PARAMETER :: known_3 = 'I am testing for'
    CHARACTER(LEN=*), PARAMETER :: input_4 = 'i am long'
    CHARACTER(LEN=*), PARAMETER :: item_4  = 'i am too long'
    CHARACTER(LEN=*), PARAMETER :: known_4 = 'i am long'
    CHARACTER(LEN=*), PARAMETER :: input_5 = 'i am long'
    CHARACTER(LEN=*), PARAMETER :: item_5  = 'LoNG'
    CHARACTER(LEN=*), PARAMETER :: known_5 = 'i am long'
    CHARACTER(LEN=*), PARAMETER :: input_6 = 'i am long'
    CHARACTER(LEN=*), PARAMETER :: item_6  = 'LoNG'
    CHARACTER(LEN=*), PARAMETER :: known_6 = 'i am '

    TestID = 'trim_from_string_test'

    cnt = 0

    !! Test # 1 (Should return an empty string)
    cnt = cnt + 1
    string_1 = TRIM_FROM_STRING(input_1,item_1)
    test_passed(1) = (string_1 == known_1)

    !! Test # 2 (Should remove item_2 from input_2)
    cnt = cnt + 1
    string_2 = TRIM_FROM_STRING(input_2,item_2)
    test_passed(2) = (string_2 == known_2)

    !! Test # 3 (Should remove item_3 at the start of the string)
    cnt = cnt + 1
    string_3 = TRIM_FROM_STRING(input_3,item_3)
    test_passed(3) = (string_3 == known_3)

    !! Test # 4 (Should not remove item_4 b/c it's larger than input_4)
    cnt = cnt + 1
    string_4 = TRIM_FROM_STRING(input_4,item_4)
    test_passed(4) = (string_4 == known_4)

    !! Test # 5 (Should not remove item_5 b/c it's using a case sensitive search)
    cnt = cnt + 1
    string_5 = TRIM_FROM_STRING(input_5,item_5,.true.)
    test_passed(5) = (string_5 == known_5)

    !! Test # 6 (Should remove item_6 b/c it's using a case insensitive search)
    cnt = cnt + 1
    string_6 = TRIM_FROM_STRING(input_6,item_6,.false.)
    test_passed(6) = (string_6 == known_6)

    IF (ALL(test_passed)) CALL all_tests_pass()

END PROGRAM trim_from_string_unit
