MODULE Precision
    USE ISO_FORTRAN_ENV, ONLY : i4k => INT32, i8k => INT64, r4k => REAL32, r8k =>REAL64
    IMPLICIT NONE
    !! author: Ian Porter
    !! date: 12/9/2017
    !!
    !! This module contains the Precision used for specifying the precision of variables
    !!
    PRIVATE
    PUBLIC :: i4k, r8k

END MODULE Precision
