MODULE Kinds
    USE ISO_FORTRAN_ENV, ONLY : i4k => INT32, i8k => INT64, r4k => REAL32, r8k =>REAL64
    IMPLICIT NONE
    !>@brief
    !> This module contains the kinds used for specifying the precision of variables
    !>@author
    !> Ian Porter
    !>@date
    !> 12/9/2017

    PRIVATE
    PUBLIC :: i4k, r8k

END MODULE Kinds
