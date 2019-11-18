module vtk_attributes_unit_tests
    use precision,      only : i4k, r8k
    use vtk_attributes, only : field_data_array
    implicit none
    !! author: Ian Porter
    !! date: 12/14/2017
    !!
    !! unit testing for attributes
    !!
    private
    public :: vtk_attributes_unit
    ! generic information
    integer(i4k), parameter :: n_types  = 8
    integer(i4k), parameter :: vtk_unit = 20
    character(len=15), dimension(n_types), parameter :: filename = &
        & [ 'scalar.vtk     ', &
        &   'vector.vtk     ', &
        &   'normal.vtk     ', &
        &   'texture.vtk    ', &
        &   'tensor.vtk     ', &
        &   'field.vtk      ', &
        &   'scalar_int.vtk ', &
        &   'tensor_int.vtk ' ]
    ! scalar information
    real(r8k), dimension(*),   parameter :: scalar_vals = &
        & [ 0.5_r8k, 1.0_r8k, 2.0_r8k, 4.0_r8k, 2.0_r8k, 1.0_r8k, 0.5_r8k ]
    integer(i4k), dimension(*),   parameter :: int_vals    = &
        & [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ]
    ! vector information
    real(r8k), dimension(2,3), parameter :: vector_r_vals  = reshape ( &
        & [ 0.5_r8k, 1.0_r8k, 0.5_r8k, &
        &   4.0_r8k, 2.0_r8k, 1.0_r8k ], [2,3])
    integer(i4k), dimension(3,3), parameter :: vector_i_vals  = reshape ( &
        & [ 10_i4k, 10_i4k, 10_i4k, &
        &   20_i4k, 20_i4k, 20_i4k, &
        &   30_i4k, 30_i4k, 30_i4k ], [3,3])
    ! normal information
    real(r8k), dimension(4,3), parameter :: normal_r_vals  = reshape ( &
        & [ 0.5_r8k, 1.0_r8k, 0.5_r8k, &
        &   1.0_r8k, 1.0_r8k, 1.0_r8k, &
        &   1.0_r8k, 0.5_r8k, 1.0_r8k, &
        &   1.0_r8k, 0.9_r8k, 1.0_r8k ], [4,3])
    integer(i4k), dimension(4,3), parameter :: normal_i_vals  = reshape ( &
        & [  1_i4k, 0_i4k, -1_i4k, &
        &   -1_i4k, 0_i4k,  1_i4k, &
        &    1_i4k, 0_i4k, -1_i4k, &
        &   -1_i4k, 0_i4k,  1_i4k ], [4,3])
    ! texture information
    real(r8k), dimension(6,2), parameter :: texture_r_vals = reshape ( &
        & [ 0.5_r8k, 1.0_r8k, &
        &   1.0_r8k, 1.0_r8k, &
        &   1.0_r8k, 0.5_r8k, &
        &   1.0_r8k, 0.9_r8k, &
        &   1.0_r8k, 0.9_r8k, &
        &   1.0_r8k, 0.9_r8k ], [6,2])
    integer(i4k), dimension(6,1), parameter :: texture_i_vals = reshape ( &
        & [ 0_i4k, &
        &   1_i4k, &
        &   2_i4k, &
        &   3_i4k, &
        &   4_i4k, &
        &   5_i4k ], [6,1])
    ! tensor information
    real(r8k), dimension(3,3), parameter :: tensor_1     = reshape ( &
        & [ 0.57_r8k, 1.00_r8k, 0.00_r8k, &
        &   1.00_r8k, 0.75_r8k, 0.80_r8k, &
        &   0.00_r8k, 0.80_r8k, 0.57_r8k ], [3,3])
    real(r8k), dimension(3,3), parameter :: tensor_2     = reshape ( &
        & [ 1.57_r8k, 2.00_r8k, 1.00_r8k, &
        &   2.00_r8k, 1.75_r8k, 1.80_r8k, &
        &   1.00_r8k, 1.80_r8k, 1.57_r8k], [3,3])
    real(r8k), dimension(3,3), parameter :: tensor_3     = reshape ( &
        & [ 2.57_r8k, 3.00_r8k, 2.00_r8k, &
        &   3.00_r8k, 2.75_r8k, 2.80_r8k, &
        &   2.00_r8k, 2.80_r8k, 2.57_r8k ], [3,3])
    real(r8k), dimension(3,3), parameter :: tensor_4     = reshape ( &
        & [ 3.57_r8k, 4.00_r8k, 3.00_r8k, &
        &   4.00_r8k, 3.75_r8k, 3.80_r8k, &
        &   3.00_r8k, 3.80_r8k, 3.57_r8k ], [3,3])
    integer(i4k), dimension(3,3), parameter :: tensor_5     = reshape ( &
        & [ -1_i4k,  0_i4k,  1_i4k, &
        &   -2_i4k, -1_i4k,  0_i4k, &
        &   -3_i4k, -2_i4k, -1_i4k ], [3,3])
    integer(i4k), dimension(3,3), parameter :: tensor_6     = reshape ( &
        & [ 1_i4k, 0_i4k,  1_i4k, &
        &   2_i4k, 1_i4k,  0_i4k, &
        &   3_i4k, 2_i4k,  1_i4k ], [3,3])
    real(r8k),    dimension(4,3,3) :: tensor_r_vals
    integer(i4k), dimension(2,3,3) :: tensor_i_vals
    ! fields information
    type(field_data_array)               :: array_1, array_2
    type(field_data_array), dimension(2) :: array
    real(r8k), dimension(3,3), parameter :: data_1 = reshape ( &
        & [ 200.0_r8k, 200.0_r8k, 200.0_r8k, &
        &   300.0_r8k, 400.0_r8k, 300.0_r8k, &
        &   500.0_r8k, 450.0_r8k, 500.0_r8k ], [3,3] )
    real(r8k), dimension(2,5), parameter :: data_2 = reshape ( &
        & [ 20.0_r8k, 200.0_r8k, &
        &   25.0_r8k, 250.0_r8k, &
        &   30.0_r8k, 300.0_r8k, &
        &   35.0_r8k, 350.0_r8k, &
        &   40.0_r8k, 400.0_r8k ], [2,5] )

contains

    subroutine vtk_attributes_unit (test_pass)
        use vtk_attributes, only : attribute, scalar, vector, normal, texture, tensor, field
        implicit none
        !!
        !! loops over each attribute type, performs a write, then performs a read on a different attribute
        !! and compares the two to make sure they are identical
        class(attribute), allocatable :: vtk_type_1, vtk_type_2
        integer(i4k)                  :: i
        logical, intent(out)          :: test_pass
        logical, dimension(n_types)   :: individual_tests_pass

        do i = 1, n_types
            if (allocated(vtk_type_1)) deallocate(vtk_type_1)
            if (allocated(vtk_type_2)) deallocate(vtk_type_2)
            select case (i)
            case (1, 7)
                !! scalar attribute
                allocate(scalar :: vtk_type_1, vtk_type_2)

                !! data type is generated from the defined values above
                if (i == 1) then
                    !! test for reals
                    call vtk_type_1%init(dataname='temperature', numcomp=1, real1d=scalar_vals)
                else if (i == 7) then
                    !! test for integers
                    call vtk_type_1%init(dataname='temperature', numcomp=1, int1d=int_vals)
                end if
            case (2, 8)
                !! vector attribute
                allocate(vector :: vtk_type_1, vtk_type_2)

                if (i == 2) then
                    !! test for reals
                    call vtk_type_1%init(dataname='temperature', numcomp=1, real2d=vector_r_vals)
                else if (i == 8) then
                    !! test for integers
                    call vtk_type_1%init(dataname='temperature', numcomp=1, int2d=vector_i_vals)
                end if
            case (3, 9)
                !! normal attribute
                allocate(normal :: vtk_type_1, vtk_type_2)

                if (i == 3) then
                    !! test for reals
                    call vtk_type_1%init(dataname='normalized_temp', numcomp=1, real2d=normal_r_vals)
                else if (i == 9) then
                    !! test for integers
                    call vtk_type_1%init(dataname='normalized_temp', numcomp=1, int2d=normal_i_vals)
                end if
            case (4, 10)
                !! texture attribute
                allocate(texture :: vtk_type_1, vtk_type_2)

                if (i == 4) then
                    !! test for reals
                    call vtk_type_1%init(dataname='textured_temp', numcomp=1, real2d=texture_r_vals)
                else if (i == 10) then
                    !! test for integers
                    call vtk_type_1%init(dataname='textured_temp', numcomp=1, int2d=texture_i_vals)
                end if
            case (5, 11)
                !! tensor attribute
                allocate(tensor :: vtk_type_1, vtk_type_2)
                if (i == 5) then
                    tensor_r_vals(1,:,:) = tensor_1; tensor_r_vals(2,:,:) = tensor_2
                    tensor_r_vals(3,:,:) = tensor_3; tensor_r_vals(4,:,:) = tensor_4
                    !! data type is generated from the defined values above
                    call vtk_type_1%init(dataname='tensor_temp', numcomp=1, real3d=tensor_r_vals)
                else if (i == 11) then
                    tensor_i_vals(1,:,:) = tensor_5; tensor_i_vals(2,:,:) = tensor_6
                    !! data type is generated from the defined values above
                    call vtk_type_1%init(dataname='tensor_temp', numcomp=1, int3d=tensor_i_vals)
                end if
            case (6)
                !! field attribute
                allocate(field :: vtk_type_1, vtk_type_2)
                array_1%name = 'temps';     array_1%numcomponents=3; array_1%numtuples=3
                array_1%datatype='double';  array_1%data = data_1
                array_2%name = 'pressures'; array_2%numcomponents=5; array_2%numtuples=2
                array_2%datatype='double';  array_2%data = data_2
                array(1) = array_1; array(2) = array_2

                !! data type is generated from the defined values above
                call vtk_type_1%init(dataname='field_temp_press', numcomp=1, field_arrays=array)
            end select

            open (unit=vtk_unit, file=filename(i), form='formatted')
            call vtk_type_1%write(vtk_unit)
            close(unit=vtk_unit)

            !! data type is generated from the read
            open (unit=vtk_unit, file=filename(i), form='formatted')
            call vtk_type_2%read(vtk_unit)
            close(unit=vtk_unit)

            !! compare the read file and the written/read file to ensure both types are the same
            individual_tests_pass(i) = .not. (vtk_type_1 .diff. vtk_type_2)
        end do

        !! compare the read file and the written/read file to ensure both types are the same
        test_pass = all(individual_tests_pass)

    end subroutine vtk_attributes_unit

end module vtk_attributes_unit_tests

program vtk_attributes_test
    use vtk_attributes_unit_tests, only : vtk_attributes_unit
    use vtkmofopassfail,           only : all_tests_pass
    implicit none
    !! author: Ian Porter
    !! date: 12/14/2017
    !!
    !! driver testing subroutine for the attributes information
    !!
    logical :: test_passes = .false.

    call vtk_attributes_unit (test_passes)

    if (test_passes) call all_tests_pass()

end program vtk_attributes_test
