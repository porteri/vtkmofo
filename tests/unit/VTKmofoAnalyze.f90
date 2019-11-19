module vtkmofopassfail
    use iso_fortran_env, only : i4k => int32, r8k => real64
    implicit none
    !! author: Ian Porter
    !! date: 3/21/2015
    !!
    !! this module is used to identity whether the unit test has passed or failed
    !!
    private
    public :: analyze, testid, all_tests_pass
    !
    integer(i4k) :: testcnt   = 0                   !! counter for the number of unit tests performed
    integer(i4k) :: testnum   = 0                   !! test number for unit test being performed (number resets for
    !! unit test on new subroutine/function)
    integer(i4k) :: utunit    = 40                  !! file unit # for writing output file for unit testing
    integer(i4k) :: nfailures = 0                   !! counter of # of failed unit tests
    real(r8k), parameter :: delta_min = 1.0e-20_r8k !! minimum value for dividing by if known = 0.0
    logical :: testingpassed = .true.               !! flag to indicate whether all unit tests passed or not
    character(len=20) :: testid                     !! name of subroutine/function being tested
    !! (defined by user as start of each set of unit tests)
    character(len=20) :: testid_prev                !! name of previous subroutine/function tested. used as a tracking tool only

contains

    elemental impure function analyze (known, calc, criteria) result(test_passed)
        implicit none
        !! author: Ian Porter
        !! date: 3/21/2015
        !!
        !! this subroutine analyzes the results of the unit test
        !!
        real(r8k), intent(in) :: known      !! expected value
        real(r8k), intent(in) :: calc       !! subroutine/function calculated value
        real(r8k), intent(in) :: criteria   !! acceptance criteria (fractional difference between known and calc values,
                                            !! relative to known)
        logical :: test_passed

        ! count the # of unit tests performed for each subroutine/function
        if (testid /= testid_prev) then
            !! a new subroutine/function is being tested
            testnum = 1                 !! first unit test for this subroutine/function
            testid_prev = testid
        else
            !! the subroutine/function was tested in the previous iteration
            testnum = testnum + 1
        end if
        testcnt = testcnt + 1           !! count the total number of unit tests that have been performed
        ! check to see if the results of the unit test fall within the specified criteria
        if (known == 0.0_r8k) then
            !! criteria is defined as the fractional difference (i.e. criteria = 0.01 specifies 1% difference)
            if (((calc - known) / delta_min) <= criteria) then
                !! the difference falls within the acceptance criteria
                call testpass (known, calc)
                test_passed = .true.
            else
                !! the differences is greater than the acceptance criteria
                call testfail (known, calc)
                test_passed = .false.
            end if
        else
            !! criteria is defined as the fractional difference (i.e. criteria = 0.01 specifies 1% difference)
            if ((abs(calc - known) / known) <= criteria) then
                !! the difference falls within the acceptance criteria
                call testpass (known, calc)
                test_passed = .true.
            else
                !! the differences is greater than the acceptance criteria
                call testfail (known, calc)
                test_passed = .false.
            end if
        end if

    end function analyze

    subroutine testpass (known, calc)
        implicit none
        !! author: Ian Porter
        !! date: 3/21/2015
        !!
        !! this subroutine indicates that a unit test has passed.
        !!
        real(r8k), intent(in) :: known  !! known value that subroutine/function tested should calulate
        real(r8k), intent(in) :: calc   !! calculated value from subroutine/function tested

        !! write to the command window and unit testing output file
        write(*,100)      testnum, testid, known, calc
        write(utunit,100) testnum, testid, known, calc
100     format(/,'unit test # ',i4,' passed for subroutine/function ',a20, &
            &  /,'expected = ',e14.7,' calculated = ',e14.7)

    end subroutine testpass

    subroutine testfail (known, calc)
        implicit none
        !! author: Ian Porter
        !! date: 3/21/2015
        !!
        !! this subroutine indicates that a unit test has failed.
        !!
        real(r8k), intent(in) :: known  !! known value that subroutine/function tested should calulate
        real(r8k), intent(in) :: calc   !! calculated value from subroutine/function tested

        ! write to the command window and unit testing output file
        write(*,100)      testnum, testid, known, calc
        write(utunit,100) testnum, testid, known, calc
100     format(/,'unit test # ',i4,' failed on subroutine/function ',a20, &
            &  /,'expected = ',e14.7,' calculated = ',e14.7)

        nfailures = nfailures + 1    !! keep track of the number of cases that have failed
        testingpassed = .false.      !! indicate that a unit test has failed

    end subroutine testfail

    subroutine all_tests_pass ()
        implicit none
        !! author: Ian Porter
        !! date: 12/13/2017
        !!
        !! this subroutine indicates that all unit tests have passed
        !! using ctest, the value "test passed" is searched for to indicate passing.
        !!
        character(len=*), parameter :: test_passed_message = 'Test Passed'

        write(*,*) test_passed_message

    end subroutine all_tests_pass

end module vtkmofopassfail
