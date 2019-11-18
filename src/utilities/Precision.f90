module precision
    use iso_fortran_env, only : i1k => int8, i2k => int16, i4k => int32, i8k => int64, &
        &                       r4k => real32, r8k => real64, r16k => real128
    implicit none
    !! author: Ian Porter
    !! date: 12/9/2017
    !!
    !! this module contains the precision used for specifying the precision of variables
    !!
    private
    public :: i1k, i2k, i4k, i8k, r4k, r8k, r16k

end module precision
