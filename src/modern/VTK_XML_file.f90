module vtk_serial_file
    use xml,          only : xml_file_dt
    use vtk_XML_grid, only : vtk_dataset_dt
    implicit none
    !! author: Ian Porter
    !! date: 05/06/2019
    !!
    !! this is the basic file for a serial vtk file
    !!

    private
    public :: serial_file, parallel_file, vtk_dataset_dt

    type, extends(xml_file_dt) :: vtk_file_dt
        !! vtk file type derived type
        class(vtk_dataset_dt), allocatable :: vtk_dataset
    contains
        procedure, private :: deallocate_vtk_file_dt
        generic, public :: me_deallocate => deallocate_vtk_file_dt
    end type vtk_file_dt

    type(vtk_file_dt), allocatable :: serial_file    !! serial vtk file
    type(vtk_file_dt), allocatable :: parallel_file  !! parallel vtk file

    interface

        module subroutine deallocate_vtk_file_dt (foo)
            implicit none
            !! author: Ian Porter
            !! date: 05/06/2019
            !!
            !! gcc work-around for deallocating a multi-dimension derived type w/ allocatable character strings
            !!
            class(vtk_file_dt), intent(inout) :: foo

        end subroutine deallocate_vtk_file_dt

    end interface

end module vtk_serial_file
