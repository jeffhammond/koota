
subroutine koota_min_real4(invec, inoutvec, len) &
    bind(C,name="koota_min_real4")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    real(4), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_min_real4

subroutine koota_max_real4(invec, inoutvec, len) &
    bind(C,name="koota_max_real4")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    real(4), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_max_real4

subroutine koota_sum_real4(invec, inoutvec, len) &
    bind(C,name="koota_sum_real4")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    real(4), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_sum_real4

subroutine koota_prod_real4(invec, inoutvec, len) &
    bind(C,name="koota_prod_real4")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    real(4), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_prod_real4

subroutine koota_band_real4(invec, inoutvec, len) &
    bind(C,name="koota_band_real4")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    real(4), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_band_real4

subroutine koota_bor_real4(invec, inoutvec, len) &
    bind(C,name="koota_bor_real4")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    real(4), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_bor_real4

subroutine koota_bxor_real4(invec, inoutvec, len) &
    bind(C,name="koota_bxor_real4")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    real(4), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_bxor_real4

subroutine koota_land_real4(invec, inoutvec, len) &
    bind(C,name="koota_land_real4")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    real(4), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_land_real4

subroutine koota_lor_real4(invec, inoutvec, len) &
    bind(C,name="koota_lor_real4")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    real(4), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_lor_real4

subroutine koota_lxor_real4(invec, inoutvec, len) &
    bind(C,name="koota_lxor_real4")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    real(4), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_lxor_real4

subroutine koota_min_real8(invec, inoutvec, len) &
    bind(C,name="koota_min_real8")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    real(8), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_min_real8

subroutine koota_max_real8(invec, inoutvec, len) &
    bind(C,name="koota_max_real8")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    real(8), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_max_real8

subroutine koota_sum_real8(invec, inoutvec, len) &
    bind(C,name="koota_sum_real8")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    real(8), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_sum_real8

subroutine koota_prod_real8(invec, inoutvec, len) &
    bind(C,name="koota_prod_real8")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    real(8), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_prod_real8

subroutine koota_band_real8(invec, inoutvec, len) &
    bind(C,name="koota_band_real8")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    real(8), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_band_real8

subroutine koota_bor_real8(invec, inoutvec, len) &
    bind(C,name="koota_bor_real8")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    real(8), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_bor_real8

subroutine koota_bxor_real8(invec, inoutvec, len) &
    bind(C,name="koota_bxor_real8")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    real(8), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_bxor_real8

subroutine koota_land_real8(invec, inoutvec, len) &
    bind(C,name="koota_land_real8")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    real(8), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_land_real8

subroutine koota_lor_real8(invec, inoutvec, len) &
    bind(C,name="koota_lor_real8")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    real(8), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_lor_real8

subroutine koota_lxor_real8(invec, inoutvec, len) &
    bind(C,name="koota_lxor_real8")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    real(8), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_lxor_real8

subroutine koota_min_real16(invec, inoutvec, len) &
    bind(C,name="koota_min_real16")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    real(16), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_min_real16

subroutine koota_max_real16(invec, inoutvec, len) &
    bind(C,name="koota_max_real16")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    real(16), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_max_real16

subroutine koota_sum_real16(invec, inoutvec, len) &
    bind(C,name="koota_sum_real16")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    real(16), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_sum_real16

subroutine koota_prod_real16(invec, inoutvec, len) &
    bind(C,name="koota_prod_real16")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    real(16), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_prod_real16

subroutine koota_band_real16(invec, inoutvec, len) &
    bind(C,name="koota_band_real16")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    real(16), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_band_real16

subroutine koota_bor_real16(invec, inoutvec, len) &
    bind(C,name="koota_bor_real16")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    real(16), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_bor_real16

subroutine koota_bxor_real16(invec, inoutvec, len) &
    bind(C,name="koota_bxor_real16")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    real(16), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_bxor_real16

subroutine koota_land_real16(invec, inoutvec, len) &
    bind(C,name="koota_land_real16")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    real(16), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_land_real16

subroutine koota_lor_real16(invec, inoutvec, len) &
    bind(C,name="koota_lor_real16")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    real(16), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_lor_real16

subroutine koota_lxor_real16(invec, inoutvec, len) &
    bind(C,name="koota_lxor_real16")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    real(16), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_lxor_real16

subroutine koota_min_complex4(invec, inoutvec, len) &
    bind(C,name="koota_min_complex4")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    complex(4), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_min_complex4

subroutine koota_max_complex4(invec, inoutvec, len) &
    bind(C,name="koota_max_complex4")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    complex(4), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_max_complex4

subroutine koota_sum_complex4(invec, inoutvec, len) &
    bind(C,name="koota_sum_complex4")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    complex(4), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_sum_complex4

subroutine koota_prod_complex4(invec, inoutvec, len) &
    bind(C,name="koota_prod_complex4")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    complex(4), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_prod_complex4

subroutine koota_band_complex4(invec, inoutvec, len) &
    bind(C,name="koota_band_complex4")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    complex(4), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_band_complex4

subroutine koota_bor_complex4(invec, inoutvec, len) &
    bind(C,name="koota_bor_complex4")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    complex(4), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_bor_complex4

subroutine koota_bxor_complex4(invec, inoutvec, len) &
    bind(C,name="koota_bxor_complex4")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    complex(4), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_bxor_complex4

subroutine koota_land_complex4(invec, inoutvec, len) &
    bind(C,name="koota_land_complex4")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    complex(4), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_land_complex4

subroutine koota_lor_complex4(invec, inoutvec, len) &
    bind(C,name="koota_lor_complex4")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    complex(4), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_lor_complex4

subroutine koota_lxor_complex4(invec, inoutvec, len) &
    bind(C,name="koota_lxor_complex4")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    complex(4), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_lxor_complex4

subroutine koota_min_complex8(invec, inoutvec, len) &
    bind(C,name="koota_min_complex8")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    complex(8), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_min_complex8

subroutine koota_max_complex8(invec, inoutvec, len) &
    bind(C,name="koota_max_complex8")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    complex(8), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_max_complex8

subroutine koota_sum_complex8(invec, inoutvec, len) &
    bind(C,name="koota_sum_complex8")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    complex(8), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_sum_complex8

subroutine koota_prod_complex8(invec, inoutvec, len) &
    bind(C,name="koota_prod_complex8")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    complex(8), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_prod_complex8

subroutine koota_band_complex8(invec, inoutvec, len) &
    bind(C,name="koota_band_complex8")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    complex(8), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_band_complex8

subroutine koota_bor_complex8(invec, inoutvec, len) &
    bind(C,name="koota_bor_complex8")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    complex(8), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_bor_complex8

subroutine koota_bxor_complex8(invec, inoutvec, len) &
    bind(C,name="koota_bxor_complex8")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    complex(8), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_bxor_complex8

subroutine koota_land_complex8(invec, inoutvec, len) &
    bind(C,name="koota_land_complex8")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    complex(8), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_land_complex8

subroutine koota_lor_complex8(invec, inoutvec, len) &
    bind(C,name="koota_lor_complex8")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    complex(8), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_lor_complex8

subroutine koota_lxor_complex8(invec, inoutvec, len) &
    bind(C,name="koota_lxor_complex8")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    complex(8), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_lxor_complex8

subroutine koota_min_complex16(invec, inoutvec, len) &
    bind(C,name="koota_min_complex16")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    complex(16), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_min_complex16

subroutine koota_max_complex16(invec, inoutvec, len) &
    bind(C,name="koota_max_complex16")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    complex(16), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_max_complex16

subroutine koota_sum_complex16(invec, inoutvec, len) &
    bind(C,name="koota_sum_complex16")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    complex(16), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_sum_complex16

subroutine koota_prod_complex16(invec, inoutvec, len) &
    bind(C,name="koota_prod_complex16")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    complex(16), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_prod_complex16

subroutine koota_band_complex16(invec, inoutvec, len) &
    bind(C,name="koota_band_complex16")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    complex(16), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_band_complex16

subroutine koota_bor_complex16(invec, inoutvec, len) &
    bind(C,name="koota_bor_complex16")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    complex(16), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_bor_complex16

subroutine koota_bxor_complex16(invec, inoutvec, len) &
    bind(C,name="koota_bxor_complex16")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    complex(16), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_bxor_complex16

subroutine koota_land_complex16(invec, inoutvec, len) &
    bind(C,name="koota_land_complex16")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    complex(16), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_land_complex16

subroutine koota_lor_complex16(invec, inoutvec, len) &
    bind(C,name="koota_lor_complex16")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    complex(16), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_lor_complex16

subroutine koota_lxor_complex16(invec, inoutvec, len) &
    bind(C,name="koota_lxor_complex16")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    complex(16), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_lxor_complex16

subroutine koota_min_integer1(invec, inoutvec, len) &
    bind(C,name="koota_min_integer1")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    integer(1), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_min_integer1

subroutine koota_max_integer1(invec, inoutvec, len) &
    bind(C,name="koota_max_integer1")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    integer(1), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_max_integer1

subroutine koota_sum_integer1(invec, inoutvec, len) &
    bind(C,name="koota_sum_integer1")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    integer(1), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_sum_integer1

subroutine koota_prod_integer1(invec, inoutvec, len) &
    bind(C,name="koota_prod_integer1")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    integer(1), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_prod_integer1

subroutine koota_band_integer1(invec, inoutvec, len) &
    bind(C,name="koota_band_integer1")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    integer(1), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_band_integer1

subroutine koota_bor_integer1(invec, inoutvec, len) &
    bind(C,name="koota_bor_integer1")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    integer(1), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_bor_integer1

subroutine koota_bxor_integer1(invec, inoutvec, len) &
    bind(C,name="koota_bxor_integer1")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    integer(1), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_bxor_integer1

subroutine koota_land_integer1(invec, inoutvec, len) &
    bind(C,name="koota_land_integer1")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    integer(1), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_land_integer1

subroutine koota_lor_integer1(invec, inoutvec, len) &
    bind(C,name="koota_lor_integer1")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    integer(1), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_lor_integer1

subroutine koota_lxor_integer1(invec, inoutvec, len) &
    bind(C,name="koota_lxor_integer1")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    integer(1), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_lxor_integer1

subroutine koota_min_integer2(invec, inoutvec, len) &
    bind(C,name="koota_min_integer2")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    integer(2), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_min_integer2

subroutine koota_max_integer2(invec, inoutvec, len) &
    bind(C,name="koota_max_integer2")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    integer(2), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_max_integer2

subroutine koota_sum_integer2(invec, inoutvec, len) &
    bind(C,name="koota_sum_integer2")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    integer(2), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_sum_integer2

subroutine koota_prod_integer2(invec, inoutvec, len) &
    bind(C,name="koota_prod_integer2")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    integer(2), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_prod_integer2

subroutine koota_band_integer2(invec, inoutvec, len) &
    bind(C,name="koota_band_integer2")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    integer(2), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_band_integer2

subroutine koota_bor_integer2(invec, inoutvec, len) &
    bind(C,name="koota_bor_integer2")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    integer(2), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_bor_integer2

subroutine koota_bxor_integer2(invec, inoutvec, len) &
    bind(C,name="koota_bxor_integer2")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    integer(2), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_bxor_integer2

subroutine koota_land_integer2(invec, inoutvec, len) &
    bind(C,name="koota_land_integer2")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    integer(2), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_land_integer2

subroutine koota_lor_integer2(invec, inoutvec, len) &
    bind(C,name="koota_lor_integer2")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    integer(2), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_lor_integer2

subroutine koota_lxor_integer2(invec, inoutvec, len) &
    bind(C,name="koota_lxor_integer2")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    integer(2), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_lxor_integer2

subroutine koota_min_integer4(invec, inoutvec, len) &
    bind(C,name="koota_min_integer4")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    integer(4), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_min_integer4

subroutine koota_max_integer4(invec, inoutvec, len) &
    bind(C,name="koota_max_integer4")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    integer(4), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_max_integer4

subroutine koota_sum_integer4(invec, inoutvec, len) &
    bind(C,name="koota_sum_integer4")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    integer(4), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_sum_integer4

subroutine koota_prod_integer4(invec, inoutvec, len) &
    bind(C,name="koota_prod_integer4")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    integer(4), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_prod_integer4

subroutine koota_band_integer4(invec, inoutvec, len) &
    bind(C,name="koota_band_integer4")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    integer(4), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_band_integer4

subroutine koota_bor_integer4(invec, inoutvec, len) &
    bind(C,name="koota_bor_integer4")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    integer(4), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_bor_integer4

subroutine koota_bxor_integer4(invec, inoutvec, len) &
    bind(C,name="koota_bxor_integer4")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    integer(4), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_bxor_integer4

subroutine koota_land_integer4(invec, inoutvec, len) &
    bind(C,name="koota_land_integer4")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    integer(4), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_land_integer4

subroutine koota_lor_integer4(invec, inoutvec, len) &
    bind(C,name="koota_lor_integer4")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    integer(4), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_lor_integer4

subroutine koota_lxor_integer4(invec, inoutvec, len) &
    bind(C,name="koota_lxor_integer4")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    integer(4), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_lxor_integer4

subroutine koota_min_integer8(invec, inoutvec, len) &
    bind(C,name="koota_min_integer8")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    integer(8), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_min_integer8

subroutine koota_max_integer8(invec, inoutvec, len) &
    bind(C,name="koota_max_integer8")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    integer(8), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_max_integer8

subroutine koota_sum_integer8(invec, inoutvec, len) &
    bind(C,name="koota_sum_integer8")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    integer(8), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_sum_integer8

subroutine koota_prod_integer8(invec, inoutvec, len) &
    bind(C,name="koota_prod_integer8")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    integer(8), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_prod_integer8

subroutine koota_band_integer8(invec, inoutvec, len) &
    bind(C,name="koota_band_integer8")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    integer(8), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_band_integer8

subroutine koota_bor_integer8(invec, inoutvec, len) &
    bind(C,name="koota_bor_integer8")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    integer(8), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_bor_integer8

subroutine koota_bxor_integer8(invec, inoutvec, len) &
    bind(C,name="koota_bxor_integer8")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    integer(8), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_bxor_integer8

subroutine koota_land_integer8(invec, inoutvec, len) &
    bind(C,name="koota_land_integer8")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    integer(8), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_land_integer8

subroutine koota_lor_integer8(invec, inoutvec, len) &
    bind(C,name="koota_lor_integer8")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    integer(8), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_lor_integer8

subroutine koota_lxor_integer8(invec, inoutvec, len) &
    bind(C,name="koota_lxor_integer8")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    integer(8), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_lxor_integer8

subroutine koota_min_integer16(invec, inoutvec, len) &
    bind(C,name="koota_min_integer16")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    integer(16), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_min_integer16

subroutine koota_max_integer16(invec, inoutvec, len) &
    bind(C,name="koota_max_integer16")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    integer(16), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_max_integer16

subroutine koota_sum_integer16(invec, inoutvec, len) &
    bind(C,name="koota_sum_integer16")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    integer(16), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_sum_integer16

subroutine koota_prod_integer16(invec, inoutvec, len) &
    bind(C,name="koota_prod_integer16")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    integer(16), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_prod_integer16

subroutine koota_band_integer16(invec, inoutvec, len) &
    bind(C,name="koota_band_integer16")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    integer(16), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_band_integer16

subroutine koota_bor_integer16(invec, inoutvec, len) &
    bind(C,name="koota_bor_integer16")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    integer(16), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_bor_integer16

subroutine koota_bxor_integer16(invec, inoutvec, len) &
    bind(C,name="koota_bxor_integer16")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    integer(16), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_bxor_integer16

subroutine koota_land_integer16(invec, inoutvec, len) &
    bind(C,name="koota_land_integer16")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    integer(16), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_land_integer16

subroutine koota_lor_integer16(invec, inoutvec, len) &
    bind(C,name="koota_lor_integer16")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    integer(16), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_lor_integer16

subroutine koota_lxor_integer16(invec, inoutvec, len) &
    bind(C,name="koota_lxor_integer16")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    integer(16), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_lxor_integer16
