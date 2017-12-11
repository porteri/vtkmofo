MODULE Misc
    USE Kinds
    
    PRIVATE
    PUBLIC :: get_len
    
    INTERFACE get_len
        PROCEDURE :: get_len_int!, get_len_real
    END INTERFACE
    
    CONTAINS
        SUBROUTINE get_len_int (len_size, intval)
        INTEGER(i4k), INTENT(IN)  :: intval
        INTEGER(i4k), INTENT(OUT) :: len_size

        IF      (intval < 1e1) THEN
            len_size = 1
        ELSE IF (intval < 1e2) THEN
            len_size = 2
        ELSE IF (intval < 1e3) THEN
            len_size = 3
        ELSE IF (intval < 1e4) THEN
            len_size = 4
        ELSE IF (intval < 1e5) THEN
            len_size = 5
        ELSE IF (intval < 1e6) THEN
            len_size = 6
        ELSE IF (intval < 1e7) THEN
            len_size = 7
        ELSE
            len_size = 8
        END IF

        END SUBROUTINE get_len_int
    
    END MODULE Misc