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
    INTEGER(i4k), PARAMETER :: num_tests = 4_i4k
    LOGICAL, DIMENSION(num_tests) :: test_passed = .FALSE.
    CHARACTER(LEN=:), ALLOCATABLE :: string_1, string_2, string_3, string_4
    CHARACTER(LEN=*), PARAMETER :: input_1 = 'string.txt'
    CHARACTER(LEN=*), PARAMETER :: item_1  = 'string.txt'
    CHARACTER(LEN=*), PARAMETER :: known_1 = ''
    CHARACTER(LEN=*), PARAMETER :: input_2 = 'testing.out'
    CHARACTER(LEN=*), PARAMETER :: item_2  = '.out'
    CHARACTER(LEN=*), PARAMETER :: known_2 = 'testing'
    CHARACTER(LEN=*), PARAMETER :: input_3 = 'string I am testing for'
    CHARACTER(LEN=*), PARAMETER :: item_3  = 'string'
    CHARACTER(LEN=*), PARAMETER :: known_3 = 'string I am testing for'
    CHARACTER(LEN=*), PARAMETER :: input_4 = 'i am long'
    CHARACTER(LEN=*), PARAMETER :: item_4  = 'i am too long'
    CHARACTER(LEN=*), PARAMETER :: known_4 = 'i am long'

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

    !! Test # 3 (Should result in no removal of item_2)
    cnt = cnt + 1
    string_3 = TRIM_FROM_STRING(input_3,item_3)
    test_passed(3) = (string_3 == known_3)

    !! Test # 4 (Should not remove item_3 b/c it's larger than input_3)
    cnt = cnt + 1
    string_4 = TRIM_FROM_STRING(input_4,item_4)
    test_passed(4) = (string_4 == known_4)

    IF (ALL(test_passed)) CALL all_tests_pass()

END PROGRAM trim_from_string_unit
