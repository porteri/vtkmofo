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
        USE XML, ONLY : xml_element_dt, gcc_bug_workaround_deallocate
        USE VTK_piece_element
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
        TYPE(xml_element_dt) :: grid
        TYPE(Piece_dt) :: piece

write(0,*) 'start of set_grid'
        CALL me%initialize(type=grid_type,file_extension=file_extension)
write(0,*) 'before me%get_range_cnt()'
        range = geometry%get_range_cnt()
write(0,*) 'after me%get_range_cnt()'
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
write(0,*) 'before call grid%setup'
        CALL grid%setup(name=grid_type,string= "WholeExtent=" // '"' // range_string // '"')
write(0,*) 'before piece%initialize(geometry)'
        !! For now, don't allow "pieces" but instead force the piece to be the whole extent
        CALL piece%initialize(geometry)
write(0,*) 'before call grid%add(piece)'
        CALL grid%add(piece)
write(0,*) 'before call me%add(grid)'
        CALL me%add(grid)
write(0,*) 'before call piece%deallocate()'
        CALL piece%deallocate()
write(0,*) 'before grid%deallocate'
        CALL grid%deallocate()
write(0,*) 'after grid%deallocate'

write(0,*) 'end of set_grid'
        END PROCEDURE set_grid

!          <?xml version="1.0"?>
!              <VTKFile type="RectilinearGrid" version="0.1" byte_order="LittleEndian">
!                <RectilinearGrid WholeExtent="0 2 1 2 4 6">
!                  <Piece Extent="0 2 1 2 4 6">
!                    <PointData Scalars="Pressure_(Pa)">
!                      <DataArray type="Float32" Name="Pressure_(Pa)" format="ascii">
!                        1.0 1.0 1.0
!                        2.0 2.0 2.0
!                        3.0 3.0 3.0
!                        4.0 4.0 4.0
!                        5.0 5.0 5.0
!                        1.0 10.0 1.0
!                      </DataArray>
!                    </PointData>
!                    <Coordinates>
!                      <DataArray type="Float32" format="ascii" RangeMin = "5.00000E-01" RangeMax="1.000000E+00">
!                        5.000000E-01 7.500000E-01 1.000000E+00
!                      </DataArray>
!                      <DataArray type="Float32" format="ascii" RangeMin = "5.00000E-01" RangeMax="1.000000E+00">
!                        5.000000E-01 1.000000E+00
!                      </DataArray>
!                      <DataArray type="Float32" format="ascii" RangeMin = "5.00000E-01" RangeMax="1.000000E+00">
!                        5.000000E-01 7.500000E-01 1.000000E+00
!                      </DataArray>
!                    </Coordinates>
!                  </Piece>
!                </RectilinearGrid>
!              </VTKFile>
END SUBMODULE RectilinearGrid_sub
