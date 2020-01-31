submodule (vtk_serial_file) vtk_serial_file_procedures
    implicit none
    !! author: Ian Porter
    !! date: 05/06/2019
    !!
    !! this is the basic file for a serial vtk file
    !!

contains

    module procedure deallocate_vtk_file_dt
        use xml, only : gcc_bug_workaround_deallocate_xml_file_dt
        implicit none
        !! author: Ian Porter
        !! date: 05/06/2019
        !!
        !! gcc work-around for deallocating a multi-dimension derived type w/ allocatable character strings
        !!
        !type(vtk_file_dt) :: new_file

        !select type (foo)
        !class is (vtk_file_dt)
        !    foo = new_file
        !end select
        !foo = serial_file_clean
        !return

        if (allocated(foo%vtk_dataset)) call foo%vtk_dataset%vtk_dataset_deallocate()
        deallocate(foo%vtk_dataset)

!        call gcc_bug_workaround_deallocate(foo)
        call gcc_bug_workaround_deallocate_xml_file_dt(foo)
!        call foo%deallocate() !! deallocate the xml data

    end procedure deallocate_vtk_file_dt

end submodule vtk_serial_file_procedures
