MODULE PassFail
    USE ISO_FORTRAN_ENV, ONLY : i4k => INT32, r8k => REAL64
    IMPLICIT NONE
    !>@breif
    !> This module is used to identity whether the unit test has passed or failed
    !>@author
    !> Ian Porter
    !>@date
    !> 3/21/2015
    PRIVATE
    PUBLIC :: Analyze, testid, all_tests_pass
    !
    INTEGER(i4k) :: testcnt   = 0                   !! Counter for the number of unit tests performed
    INTEGER(i4k) :: testnum   = 0                   !! Test number for unit test being performed (number resets for 
                                                    !! unit test on new subroutine/function)
    INTEGER(i4k) :: utunit    = 40                  !! File unit # for writing output file for Unit Testing
    INTEGER(i4k) :: nfailures = 0                   !! Counter of # of failed unit tests
    REAL(r8k), PARAMETER :: delta_min = 1.0e-20_r8k !! Minimum value for dividing by if known = 0.0
    LOGICAL :: TestingPassed = .TRUE.               !! Flag to indicate whether all unit tests passed or not
    CHARACTER(LEN=20) :: testid                     !! Name of subroutine/function being tested
                                                    !! (Defined by user as start of each set of unit tests)
    CHARACTER(LEN=20) :: testid_prev                !! Name of previous subroutine/function tested. Used as a tracking tool only
    !
    CONTAINS
        ELEMENTAL IMPURE FUNCTION Analyze (known, calc, criteria) RESULT(test_passed)
        IMPLICIT NONE
        !>@brief
        !> This subroutine analyzes the results of the unit test
        !>@author
        !> Ian Porter
        !>@date
        !> 3/21/2015
        ! Inputs
        ! known    - Expected value
        ! calc     - Subroutine/function calculated value
        ! criteria - Acceptance criteria (Fractional difference between known and calc values, relative to known)
        !
        REAL(r8k), INTENT(IN) :: known, calc, criteria
        LOGICAL :: test_passed

        ! Count the # of unit tests performed for each subroutine/function
        IF (testid /= testid_prev) THEN
            !! A new subroutine/function is being tested
            testnum = 1                 !! First unit test for this subroutine/function
            testid_prev = testid
        ELSE
            !! The subroutine/function was tested in the previous iteration
            testnum = testnum + 1
        END IF
        testcnt = testcnt + 1           !! Count the total number of unit tests that have been performed
        ! Check to see if the results of the unit test fall within the specified criteria
        IF (known == 0.0_r8k) THEN
            !! Criteria is defined as the fractional difference (i.e. criteria = 0.01 specifies 1% difference)
            IF (((calc - known) / delta_min) <= criteria) THEN
                !! The difference falls within the acceptance criteria
                CALL TestPass (known, calc)
                test_passed = .TRUE.
            ELSE
                !! The differences is greater than the acceptance criteria
                CALL TestFail (known, calc)
                test_passed = .FALSE.
            END IF
        ELSE
            !! Criteria is defined as the fractional difference (i.e. criteria = 0.01 specifies 1% difference)
            IF ((ABS(calc - known) / known) <= criteria) THEN
                !! The difference falls within the acceptance criteria
                CALL TestPass (known, calc)
                test_passed = .TRUE.
            ELSE
                !! The differences is greater than the acceptance criteria
                CALL TestFail (known, calc)
                test_passed = .FALSE.
            END IF
        END IF

        END FUNCTION Analyze

        SUBROUTINE TestPass (known, calc)
        IMPLICIT NONE
        !>@brief
        !> This subroutine indicates that a unit test has passed.
        !>@author
        !> Ian Porter
        !>@date
        !> 3/21/2015
        !
        ! Input
        !
        ! known - known value that subroutine/function tested should calulate
        ! calc  - calculated value from subroutine/function tested
        !
        REAL(r8k), INTENT(IN) :: known, calc

        ! Write to the command window and unit testing output file
        WRITE (*,100)      testnum, TestID, known, calc
        WRITE (utunit,100) testnum, TestID, known, calc
100     FORMAT (/,'Unit Test # ',i4,' PASSED for Subroutine/Function ',a20, &
          &     /,'Expected = ',e14.7,' Calculated = ',e14.7)

        END SUBROUTINE TestPass

        SUBROUTINE TestFail (known, calc)
        IMPLICIT NONE
        !>@brief
        !> This subroutine indicates that a unit test has failed.
        !>@author
        !> Ian Porter
        !>@date
        !> 3/21/2015
        !
        ! Input
        !
        ! known    - known value that subroutine/function tested should calulate
        ! calc     - calculated value from subroutine/function tested
        !
        ! Internal
        !
        ! nfailures - Counter for # of failed tests
        !
        REAL(r8k), INTENT(IN) :: known, calc

        ! Write to the command window and unit testing output file
        WRITE (*,100)      testnum, TestID, known, calc
        WRITE (utunit,100) testnum, TestID, known, calc
100     FORMAT (/,'Unit Test # ',i4,' FAILED on Subroutine/Function ',a20, &
          &     /,'Expected = ',e14.7,' Calculated = ',e14.7)
        
        nfailures = nfailures + 1    !! Keep track of the number of cases that have failed
        TestingPassed = .FALSE.      !! Indicate that a unit test has failed

        END SUBROUTINE TestFail

        SUBROUTINE all_tests_pass ()
        !>@brief
        !> This subroutine indicates that all unit tests have passed
        !> Using ctest, the value "Test passed" is searched for to indicate passing.
        !>@author
        !> Ian Porter
        !>@date
        !> 12/13/2017

        WRITE(*,*) 'Test passed'

        END SUBROUTINE all_tests_pass
END MODULE PassFail
