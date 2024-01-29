subroutine koota_sum_complex(invec, inoutvec, len) &
    bind(C,name="koota_sum_complex")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_f_pointer
    implicit none
    type(c_ptr), intent(in) :: invec
    type(c_ptr), intent(inout) :: inoutvec
    integer, intent(in), value :: len

    integer :: i
    complex, pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_sum_complex

subroutine koota_max_real(invec, inoutvec, len) &
    bind(C,name="koota_max_real")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_f_pointer
    implicit none
    type(c_ptr), intent(in) :: invec
    type(c_ptr), intent(inout) :: inoutvec
    integer, intent(in), value :: len

    integer :: i
    real, pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = MAX(fp_inoutvec(i),fp_invec(i))
    end do
end subroutine koota_max_real

subroutine koota_band_integer(invec, inoutvec, len) &
    bind(C,name="koota_band_integer")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_f_pointer
    implicit none
    type(c_ptr), intent(in) :: invec
    type(c_ptr), intent(inout) :: inoutvec
    integer, intent(in), value :: len

    integer :: i
    integer, pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = IAND(fp_inoutvec(i),fp_invec(i))
    end do
end subroutine koota_band_integer

subroutine koota_land_logical(invec, inoutvec, len) &
    bind(C,name="koota_and_logical")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_f_pointer
    implicit none
    type(c_ptr), intent(in) :: invec
    type(c_ptr), intent(inout) :: inoutvec
    integer, intent(in), value :: len

    integer :: i
    logical, pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) .AND. fp_invec(i)
    end do
end subroutine koota_land_logical
