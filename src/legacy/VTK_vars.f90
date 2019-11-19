module vtk_vars
    use precision, only : i4k
    use xml,       only : ascii, binary, file_format
    implicit none
    !! author: Ian Porter
    !! date: 12/20/2017
    !!
    !! this module contains basic information needed for reading/writing to the vtk file
    !!
    private
    public :: vtkunit, version, default_title, default_fn, vtkfilename, vtktitle, fcnt, vtk_extension
    public :: ascii, binary, file_format
    !! selected file type
    integer(i4k) :: vtkunit  = 20_i4k                                               !! default vtk unit #
    integer(i4k) :: fcnt     = 0_i4k                                                !! file counter for time-dependent output files
    integer(i4k), parameter :: bit=0_i4k, unsigned_char=1_i4k, char=2_i4k, unsigned_short=3_i4k, short=4_i4k, &
        &                      unsigned_int=5_i4k, int=6_i4k, unsigned_long=7_i4k, long=8_i4k, float=9_i4k,   &
        &                      double=10_i4k                                        !! types of data
    character(len=*), parameter   :: version       = '# vtk datafile version 3.0'   !! vtk datafile version
    character(len=*), parameter   :: default_title = 'version 3.0 vtk file'         !! title card
    character(len=*), parameter   :: vtk_extension = '.vtk'                         !! file extension
    character(len=*), parameter   :: default_fn    = 'out'                          !! default filename
    character(len=:), allocatable :: vtkfilename                                    !! supplied filename
    character(len=:), allocatable :: vtktitle                                       !! supplied title

end module vtk_vars
