SUBMODULE (VTK_serial_RectilinearGrid) RectilinearGrid_sub
    USE Precision, ONLY : i4k
    !! author: Ian Porter
    !! date: 05/06/2019
    !!
    !! This submodule implements the procedures for a serial Rectilinear Grid
    !!

    CHARACTER(LEN=*), PARAMETER :: file_extension = ".vtr"
    CHARACTER(LEN=*), PARAMETER :: grid_type = "RectilinearGrid"

    CONTAINS

        MODULE PROCEDURE set_grid
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 05/06/2019
        !!
        !! This sets parameters specific to the DT
        !!
        CHARACTER(LEN=10) :: tmp_string = '          '
        CHARACTER(LEN=:), ALLOCATABLE :: range_string
        INTEGER(i4k) :: i, j
        INTEGER(i4k), DIMENSION(2,3)  :: range

        CALL me%initialize(type=grid_type,file_extension=file_extension)
        range = geometry%get_range_cnt()

        DO i = 1, 3
            DO j = 1, 2
                WRITE(tmp_string,'(i10)') range(j,i)
                IF (.NOT. ALLOCATED(range_string)) THEN
                    ALLOCATE(range_string,source=TRIM(ADJUSTL(tmp_string)))
                ELSE
                    range_string = range_string // ' ' // TRIM(ADJUSTL(tmp_string))
                END IF
            END DO
        END DO

        ALLOCATE(me%WholeExtent, source=range_string)
        ALLOCATE(me%grid_type, source=grid_type)

        !! For now, don't allow "pieces" but instead force the piece to be the whole extent
        IF (.NOT. ALLOCATED(me%piece)) ALLOCATE(me%piece)
        CALL me%piece%set(geometry)

        END PROCEDURE set_grid

END SUBMODULE RectilinearGrid_sub
