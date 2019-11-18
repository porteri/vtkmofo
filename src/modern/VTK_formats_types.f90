module vtk_formats_types
    implicit none
    !! author: Ian Porter
    !! date: 06/07/2019
    !!
    !! this is the basic file piece elements
    !!
    !! data storage formats

    public

    !! data types
    character(len=*), parameter :: type_int8    = 'int8'
    character(len=*), parameter :: type_uint8   = 'uint8'
    character(len=*), parameter :: type_int16   = 'int16'
    character(len=*), parameter :: type_uint16  = 'uint16'
    character(len=*), parameter :: type_int32   = 'int32'
    character(len=*), parameter :: type_uint32  = 'uint32'
    character(len=*), parameter :: type_int64   = 'int64'
    character(len=*), parameter :: type_uint64  = 'uint64'
    character(len=*), parameter :: type_float32 = 'float32'
    character(len=*), parameter :: type_float64 = 'float64'

end module vtk_formats_types
