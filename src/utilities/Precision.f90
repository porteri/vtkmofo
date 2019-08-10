MODULE Precision
    USE ISO_FORTRAN_ENV, ONLY : i1k => INT8, i2k => INT16, i4k => INT32, i8k => INT64, &
        &                       r4k => REAL32, r8k => REAL64, r16k => REAL128
    IMPLICIT NONE
    !! author: Ian Porter
    !! date: 12/9/2017
    !!
    !! This module contains the Precision used for specifying the precision of variables
    !!
    PRIVATE
    PUBLIC :: i1k, i2k, i4k, i8k, r4k, r8k, r16k

END MODULE Precision
