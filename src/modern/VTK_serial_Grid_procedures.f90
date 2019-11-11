SUBMODULE (VTK_serial_Grid) VTK_serial_Grid_procedures
    USE Precision, ONLY : i4k
    !! author: Ian Porter
    !! date: 05/06/2019
    !!
    !! This submodule implements the procedures for a serial Rectilinear Grid
    !!

    CONTAINS

        MODULE PROCEDURE finalize
        USE XML, ONLY : xml_element_dt
        IMPLICIT NONE
        !! Writes data inside of itself
        TYPE(VTK_element_dt) :: grid

        IF (ALLOCATED(me%piece)) THEN
            CALL me%piece%finalize()
            IF (ALLOCATED(me%WholeExtent)) THEN
                CALL grid%setup(name=me%grid_type,string= "WholeExtent=" // '"' // me%WholeExtent // '"')
            ELSE
                CALL grid%setup(name=me%grid_type)
            END IF
            IF (ALLOCATED(me%extra_string)) CALL grid%add(me%extra_string, quotes=.FALSE.)
            CALL grid%add(me%piece)
            CALL me%add(grid)
            CALL grid%me_deallocate()
        END IF

        END PROCEDURE finalize

        MODULE PROCEDURE vtk_dataset_deallocate
        IMPLICIT NONE
        !! gcc Work-around for deallocating a multi-dimension derived type w/ allocatable character strings

        IF (ALLOCATED(foo%piece)) CALL foo%piece%piece_deallocate()

        CALL foo%me_deallocate()

        END PROCEDURE vtk_dataset_deallocate

        MODULE PROCEDURE ImageData_set_grid
        USE VTK_datasets, ONLY : struct_pts
        USE Misc,         ONLY : convert_to_string
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 08/08/2019
        !!
        !! This writes the grid information for an image data grid
        !!
        CHARACTER(LEN=10) :: tmp_string = '          '
        CHARACTER(LEN=:), ALLOCATABLE :: range_string, origin_string, spacing_string
        INTEGER(i4k) :: i, j
        INTEGER(i4k), DIMENSION(2,3)  :: range
        CHARACTER(LEN=*), PARAMETER :: file_extension = ".vti"
        CHARACTER(LEN=*), PARAMETER :: grid_type = "ImageData"

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

        !! Still need to set the following line of information:
        !! Origin=”x0 y0 z0” Spacing=”dx dy dz”>
        SELECT TYPE(geometry)
        CLASS IS (struct_pts)
            ALLOCATE(origin_string, source=convert_to_string(geometry%get_origin()))
            ALLOCATE(spacing_string, source=convert_to_string(geometry%get_spacing()))
            ALLOCATE(me%extra_string, source='Origin="' // origin_string // '", Spacing="' // spacing_string // '"')
        CLASS DEFAULT
            ERROR STOP 'Bad geometry type for ImageData. Terminated in ImageData_set_grid'
        END SELECT

!        ERROR STOP 'ImageData_set_grid is not yet implemented. Need to set origin, spacing'

        !! For now, don't allow "pieces" but instead force the piece to be the whole extent
        IF (.NOT. ALLOCATED(me%piece)) ALLOCATE(me%piece)
        CALL me%piece%set(geometry)

        END PROCEDURE ImageData_set_grid

        MODULE PROCEDURE Rectilineargrid_set_grid
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
        CHARACTER(LEN=*), PARAMETER :: file_extension = ".vtr"
        CHARACTER(LEN=*), PARAMETER :: grid_type = "RectilinearGrid"

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

        END PROCEDURE Rectilineargrid_set_grid

        MODULE PROCEDURE Structuredgrid_set_grid
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
        CHARACTER(LEN=*), PARAMETER :: file_extension = ".vts"
        CHARACTER(LEN=*), PARAMETER :: grid_type = "StructuredGrid"

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

        END PROCEDURE Structuredgrid_set_grid

        MODULE PROCEDURE Unstructuredgrid_set_grid
        IMPLICIT NONE
        !! author: Ian Porter
        !! date: 05/06/2019
        !!
        !! This sets parameters specific to the DT
        !!
        CHARACTER(LEN=*), PARAMETER :: file_extension = ".vtu"
        CHARACTER(LEN=*), PARAMETER :: grid_type = "UnstructuredGrid"

        CALL me%initialize(type=grid_type,file_extension=file_extension)

        ALLOCATE(me%grid_type, source=grid_type)

        !! For now, don't allow "pieces" but instead force the piece to be the whole extent
        IF (.NOT. ALLOCATED(me%piece)) ALLOCATE(me%piece)
        CALL me%piece%set(geometry)

        END PROCEDURE Unstructuredgrid_set_grid


END SUBMODULE VTK_serial_Grid_procedures
