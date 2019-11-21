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
    character(len=*), parameter :: type_int8    = 'Int8'
    character(len=*), parameter :: type_uint8   = 'UInt8'
    character(len=*), parameter :: type_int16   = 'Int16'
    character(len=*), parameter :: type_uint16  = 'UInt16'
    character(len=*), parameter :: type_int32   = 'Int32'
    character(len=*), parameter :: type_uint32  = 'UInt32'
    character(len=*), parameter :: type_int64   = 'Int64'
    character(len=*), parameter :: type_uint64  = 'UInt64'
    character(len=*), parameter :: type_float32 = 'Float32'
    character(len=*), parameter :: type_float64 = 'Float64'

end module vtk_formats_types
