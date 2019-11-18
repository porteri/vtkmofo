module vtk_io
    use precision,      only : i4k
    use vtk_attributes, only : attribute, attributes
    use vtk_datasets,   only : dataset
    use vtk_vars,       only : vtkfilename, default_fn, fcnt
    implicit none
    !! author: Ian Porter
    !! date: 12/1/2017
    !!
    !! this module contains the output file to write to vtk format
    !!
    private
    public :: vtk_legacy_write, vtk_serial_write

    interface vtk_legacy_write
        procedure :: vtk_legacy_full_write
        procedure :: vtk_legacy_append
        procedure :: vtk_legacy_finalize
    end interface vtk_legacy_write

    interface vtk_serial_write
        procedure :: vtk_serial_full_write
        procedure :: vtk_serial_append
        procedure :: vtk_serial_finalize
    end interface vtk_serial_write

    interface

        module subroutine vtk_legacy_full_write (geometry, celldata, pointdata, celldatasets, pointdatasets, &
            &                                    unit, filename, multiple_io, format, title)
            implicit none
            !! author: Ian Porter
            !! date: 12/1/2017
            !!
            !! this subroutine writes the legacy vtk output file
            !!
            class(dataset),   intent(in)           :: geometry   !! dt of geometry to be printed
            class(attribute), intent(in), optional :: celldata   !!
            class(attribute), intent(in), optional :: pointdata  !!
            type(attributes), dimension(:), intent(in), optional :: celldatasets  !!
            type(attributes), dimension(:), intent(in), optional :: pointdatasets !!
            integer(i4k),     intent(in), optional :: unit        !! vtk file unit
            integer(i4k),     intent(in), optional :: format      !! identifier to write in ascii or binary
            logical,          intent(in), optional :: multiple_io !! identifier as to whether there will be multiple files written
            !! (i.e., time-dependent output)
            character(len=*), intent(in), optional :: filename    !! vtk filename
            character(len=*), intent(in), optional :: title       !! title to be written on title line (#2) in output file

        end subroutine vtk_legacy_full_write

        module subroutine vtk_legacy_append (celldata, pointdata, celldatasets, pointdatasets)
            implicit none
            !! author: Ian Porter
            !! date: 12/1/2017
            !!
            !! this subroutine appends data to the legacy vtk output file
            !!
            class(attribute), intent(in), optional :: celldata   !!
            class(attribute), intent(in), optional :: pointdata  !!
            type(attributes), dimension(:), intent(in), optional :: celldatasets  !!
            type(attributes), dimension(:), intent(in), optional :: pointdatasets !!

        end subroutine vtk_legacy_append

        module subroutine vtk_legacy_finalize (finished)
            implicit none
            !! author: Ian Porter
            !! date: 06/03/2019
            !!
            !! this subroutine is a finalizer for the legacy vtk file write
            !!
            logical, intent(in) :: finished  !! finished flag

        end subroutine vtk_legacy_finalize

        module subroutine vtk_legacy_read (unit, geometry, celldata, pointdata, celldatasets, pointdatasets, &
            &                              filename, format, title)
            implicit none
            !! author: Ian Porter
            !! date: 12/20/2017
            !!
            !! this subroutine reads the legacy vtk output file
            !!
            class(dataset),   intent(inout)           :: geometry   !! dt of geometry to be printed
            class(attribute), intent(inout), optional :: celldata   !!
            class(attribute), intent(inout), optional :: pointdata  !!
            type(attributes), dimension(:), intent(inout), optional :: celldatasets  !!
            type(attributes), dimension(:), intent(inout), optional :: pointdatasets !!
            integer(i4k),     intent(in)            :: unit         !! vtk file unit
            integer(i4k),     intent(out), optional :: format       !! identifier as to whether vtk file is ascii or binary
            character(len=*), intent(in),  optional :: filename     !! vtk filename
            character(len=*), intent(out), optional :: title        !! title to be written on title line (#2) in output file

        end subroutine vtk_legacy_read

        module subroutine vtk_serial_full_write (geometry, celldata, pointdata, celldatasets, pointdatasets, &
            &                                    unit, filename, multiple_io, format, title)
            implicit none
            !! author: Ian Porter
            !! date: 5/08/2019
            !!
            !! this subroutine writes the modern serial vtk output file
            !!
            class(dataset),   intent(in)           :: geometry   !! dt of geometry to be printed
            class(attribute), intent(in), optional :: celldata   !!
            class(attribute), intent(in), optional :: pointdata  !!
            type(attributes), dimension(:), intent(in), optional :: celldatasets  !!
            type(attributes), dimension(:), intent(in), optional :: pointdatasets !!
            integer(i4k),     intent(in), optional :: unit        !! vtk file unit
            integer(i4k),     intent(in), optional :: format      !! identifier to write in ascii or binary
            logical,          intent(in), optional :: multiple_io !! identifier as to whether there will be multiple files written
            !! (i.e., time-dependent output)
            character(len=*), intent(in), optional :: filename    !! vtk filename
            character(len=*), intent(in), optional :: title       !! title to be written on title line (#2) in output file

        end subroutine vtk_serial_full_write

        module subroutine vtk_serial_append (celldata, pointdata, celldatasets, pointdatasets)
            implicit none
            !! author: Ian Porter
            !! date: 06/24/2019
            !!
            !! this subroutine appends data to the legacy vtk output file
            !!
            class(attribute), intent(in), optional :: celldata   !!
            class(attribute), intent(in), optional :: pointdata  !!
            type(attributes), dimension(:), intent(in), optional :: celldatasets  !!
            type(attributes), dimension(:), intent(in), optional :: pointdatasets !!

        end subroutine vtk_serial_append

        module subroutine vtk_serial_finalize (finished)
            implicit none
            !! author: Ian Porter
            !! date: 06/24/2019
            !!
            !! this subroutine is a finalizer for the legacy vtk file write
            !!
            logical, intent(in) :: finished  !! finished flag

        end subroutine vtk_serial_finalize

    end interface

end module vtk_io
