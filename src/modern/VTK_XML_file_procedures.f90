submodule (vtk_serial_file) vtk_serial_file_procedures
    implicit none
    !! author: Ian Porter
    !! date: 05/06/2019
    !!
    !! this is the basic file for a serial vtk file
    !!

contains

    module procedure deallocate_vtk_file_dt
        use xml, only : gcc_bug_workaround_deallocate
        implicit none
        !! author: Ian Porter
        !! date: 05/06/2019
        !!
        !! gcc work-around for deallocating a multi-dimension derived type w/ allocatable character strings
        !!

        if (allocated(foo%vtk_dataset)) call foo%vtk_dataset%vtk_dataset_deallocate()

!        call gcc_bug_workaround_deallocate(foo)
        call foo%deallocate() !! deallocate the xml data

    end procedure deallocate_vtk_file_dt

end submodule vtk_serial_file_procedures
