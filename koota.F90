subroutine koota_sum_real4(invec, inoutvec, len) &
    bind(C,name="koota_sum_real4")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(inout) :: invec
    type(c_ptr), intent(inout) :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    real(4), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    write(*,'(i8,1x,z20,1x,z20)') len, loc(invec), loc(inoutvec)

    do i=1,len
        print*,i,fp_invec(i),fp_inoutvec(i)
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_sum_real4

subroutine koota_sum_complex4(invec, inoutvec, len) &
    bind(C,name="koota_sum_complex4")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in) :: invec
    type(c_ptr), intent(inout) :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    complex(4), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_sum_complex4

subroutine koota_max_real4(invec, inoutvec, len) &
    bind(C,name="koota_max_real4")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in) :: invec
    type(c_ptr), intent(inout) :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    real(4), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = MAX(fp_inoutvec(i),fp_invec(i))
    end do
end subroutine koota_max_real4

subroutine koota_band_integer4(invec, inoutvec, len) &
    bind(C,name="koota_band_integer4")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in) :: invec
    type(c_ptr), intent(inout) :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    integer(4), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = IAND(fp_inoutvec(i),fp_invec(i))
    end do
end subroutine koota_band_integer4

subroutine koota_land_logical4(invec, inoutvec, len) &
    bind(C,name="koota_and_logical4")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in) :: invec
    type(c_ptr), intent(inout) :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    logical(4), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) .AND. fp_invec(i)
    end do
end subroutine koota_land_logical4
