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

        MODULE PROCEDURE set_grid_data
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
!        TYPE(Coordinates_dt) :: Coordinates
        TYPE(CellData_dt) :: CellData_xml
        TYPE(PointData_dt) :: PointData_xml

write(0,*) 'start of set_grid_data'
        CALL me%initialize(type=grid_type,file_extension=file_extension)
write(0,*) 'before me%get_range()'
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

        CALL grid%setup(name=grid_type,string= "WholeExtent=" // '"' // range_string // '"')
        !! For now, don't allow "pieces" but instead force the piece to be the whole extent
        CALL piece%initialize(geometry)

        CALL PointData_xml%initialize()
        CALL CellData_xml%initialize()

!! Something to initialize a DataArray with one of the following:

        IF (PRESENT(celldatasets)) THEN
            CALL CellData_xml%add_cell(celldatasets)
        ELSE IF (PRESENT(celldata)) THEN
            CALL CellData_xml%add_cell(celldata)
        END IF
        IF (PRESENT(pointdatasets)) THEN
            CALL PointData_xml%add_cell(pointdatasets)
        ELSE IF (PRESENT(pointdata)) THEN
            CALL PointData_xml%add_cell(pointdata)
        END IF


        !! Add any data
        !CALL PointData_xml%add()
        !CALL Coordinates%setup(name="Coordinates")
        !! Add coordinate information
        !CALL Coordinates%add()
write(0,*) 'before call piece%add(pointdata)'
            CALL piece%add(pointdata_xml)
write(0,*) 'before call piece%add(coordinates)'
!            CALL piece%add(coordinates)
write(0,*) 'before call grid%add(piece)'
            CALL grid%add(piece)
write(0,*) 'before call me%add(grid)'
            CALL me%add(grid)
write(0,*) 'before call PointData_xml%deallocate()'
            CALL PointData_xml%deallocate()
write(0,*) 'before call Coordinates%deallocate()'
!            CALL Coordinates%deallocate()
write(0,*) 'before call piece%deallocate()'
            CALL piece%deallocate()
write(0,*) 'before grid%deallocate'
            CALL grid%deallocate()
write(0,*) 'after grid%deallocate'

write(0,*) 'end of set_grid_data'
        END PROCEDURE set_grid_data

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
