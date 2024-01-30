
subroutine koota_min_real4(invec, inoutvec, len) &
    bind(C,name="koota_min_real4")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    real(kind=4), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
       fp_inoutvec(i) = MIN(fp_inoutvec(i),fp_invec(i))
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
    real(kind=4), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
       fp_inoutvec(i) = MAX(fp_inoutvec(i),fp_invec(i))
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
    real(kind=4), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
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
    real(kind=4), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
       fp_inoutvec(i) = fp_inoutvec(i) * fp_invec(i)
    end do
end subroutine koota_prod_real4

subroutine koota_min_real8(invec, inoutvec, len) &
    bind(C,name="koota_min_real8")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    real(kind=8), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
       fp_inoutvec(i) = MIN(fp_inoutvec(i),fp_invec(i))
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
    real(kind=8), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
       fp_inoutvec(i) = MAX(fp_inoutvec(i),fp_invec(i))
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
    real(kind=8), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
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
    real(kind=8), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
       fp_inoutvec(i) = fp_inoutvec(i) * fp_invec(i)
    end do
end subroutine koota_prod_real8

subroutine koota_min_real10(invec, inoutvec, len) &
    bind(C,name="koota_min_real10")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    real(kind=10), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
       fp_inoutvec(i) = MIN(fp_inoutvec(i),fp_invec(i))
    end do
end subroutine koota_min_real10

subroutine koota_max_real10(invec, inoutvec, len) &
    bind(C,name="koota_max_real10")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    real(kind=10), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
       fp_inoutvec(i) = MAX(fp_inoutvec(i),fp_invec(i))
    end do
end subroutine koota_max_real10

subroutine koota_sum_real10(invec, inoutvec, len) &
    bind(C,name="koota_sum_real10")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    real(kind=10), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
       fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_sum_real10

subroutine koota_prod_real10(invec, inoutvec, len) &
    bind(C,name="koota_prod_real10")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    real(kind=10), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
       fp_inoutvec(i) = fp_inoutvec(i) * fp_invec(i)
    end do
end subroutine koota_prod_real10

subroutine koota_min_real16(invec, inoutvec, len) &
    bind(C,name="koota_min_real16")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    real(kind=16), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
       fp_inoutvec(i) = MIN(fp_inoutvec(i),fp_invec(i))
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
    real(kind=16), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
       fp_inoutvec(i) = MAX(fp_inoutvec(i),fp_invec(i))
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
    real(kind=16), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
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
    real(kind=16), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
       fp_inoutvec(i) = fp_inoutvec(i) * fp_invec(i)
    end do
end subroutine koota_prod_real16

subroutine koota_sum_complex4(invec, inoutvec, len) &
    bind(C,name="koota_sum_complex4")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    complex(kind=4), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
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
    complex(kind=4), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
       fp_inoutvec(i) = fp_inoutvec(i) * fp_invec(i)
    end do
end subroutine koota_prod_complex4

subroutine koota_sum_complex8(invec, inoutvec, len) &
    bind(C,name="koota_sum_complex8")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    complex(kind=8), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
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
    complex(kind=8), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
       fp_inoutvec(i) = fp_inoutvec(i) * fp_invec(i)
    end do
end subroutine koota_prod_complex8

subroutine koota_sum_complex10(invec, inoutvec, len) &
    bind(C,name="koota_sum_complex10")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    complex(kind=10), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
       fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_sum_complex10

subroutine koota_prod_complex10(invec, inoutvec, len) &
    bind(C,name="koota_prod_complex10")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    complex(kind=10), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
       fp_inoutvec(i) = fp_inoutvec(i) * fp_invec(i)
    end do
end subroutine koota_prod_complex10

subroutine koota_sum_complex16(invec, inoutvec, len) &
    bind(C,name="koota_sum_complex16")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    complex(kind=16), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
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
    complex(kind=16), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
       fp_inoutvec(i) = fp_inoutvec(i) * fp_invec(i)
    end do
end subroutine koota_prod_complex16

subroutine koota_min_integer1(invec, inoutvec, len) &
    bind(C,name="koota_min_integer1")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    integer(kind=1), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
       fp_inoutvec(i) = MIN(fp_inoutvec(i),fp_invec(i))
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
    integer(kind=1), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
       fp_inoutvec(i) = MAX(fp_inoutvec(i),fp_invec(i))
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
    integer(kind=1), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
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
    integer(kind=1), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
       fp_inoutvec(i) = fp_inoutvec(i) * fp_invec(i)
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
    integer(kind=1), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
       fp_inoutvec(i) = IAND(fp_inoutvec(i),fp_invec(i))
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
    integer(kind=1), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
       fp_inoutvec(i) = IOR(fp_inoutvec(i),fp_invec(i))
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
    integer(kind=1), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
       fp_inoutvec(i) = IEOR(fp_inoutvec(i),fp_invec(i))
    end do
end subroutine koota_bxor_integer1

subroutine koota_min_integer2(invec, inoutvec, len) &
    bind(C,name="koota_min_integer2")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    integer(kind=2), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
       fp_inoutvec(i) = MIN(fp_inoutvec(i),fp_invec(i))
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
    integer(kind=2), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
       fp_inoutvec(i) = MAX(fp_inoutvec(i),fp_invec(i))
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
    integer(kind=2), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
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
    integer(kind=2), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
       fp_inoutvec(i) = fp_inoutvec(i) * fp_invec(i)
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
    integer(kind=2), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
       fp_inoutvec(i) = IAND(fp_inoutvec(i),fp_invec(i))
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
    integer(kind=2), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
       fp_inoutvec(i) = IOR(fp_inoutvec(i),fp_invec(i))
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
    integer(kind=2), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
       fp_inoutvec(i) = IEOR(fp_inoutvec(i),fp_invec(i))
    end do
end subroutine koota_bxor_integer2

subroutine koota_min_integer4(invec, inoutvec, len) &
    bind(C,name="koota_min_integer4")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    integer(kind=4), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
       fp_inoutvec(i) = MIN(fp_inoutvec(i),fp_invec(i))
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
    integer(kind=4), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
       fp_inoutvec(i) = MAX(fp_inoutvec(i),fp_invec(i))
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
    integer(kind=4), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
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
    integer(kind=4), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
       fp_inoutvec(i) = fp_inoutvec(i) * fp_invec(i)
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
    integer(kind=4), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
       fp_inoutvec(i) = IAND(fp_inoutvec(i),fp_invec(i))
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
    integer(kind=4), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
       fp_inoutvec(i) = IOR(fp_inoutvec(i),fp_invec(i))
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
    integer(kind=4), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
       fp_inoutvec(i) = IEOR(fp_inoutvec(i),fp_invec(i))
    end do
end subroutine koota_bxor_integer4

subroutine koota_min_integer8(invec, inoutvec, len) &
    bind(C,name="koota_min_integer8")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    integer(kind=8), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
       fp_inoutvec(i) = MIN(fp_inoutvec(i),fp_invec(i))
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
    integer(kind=8), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
       fp_inoutvec(i) = MAX(fp_inoutvec(i),fp_invec(i))
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
    integer(kind=8), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
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
    integer(kind=8), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
       fp_inoutvec(i) = fp_inoutvec(i) * fp_invec(i)
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
    integer(kind=8), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
       fp_inoutvec(i) = IAND(fp_inoutvec(i),fp_invec(i))
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
    integer(kind=8), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
       fp_inoutvec(i) = IOR(fp_inoutvec(i),fp_invec(i))
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
    integer(kind=8), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
       fp_inoutvec(i) = IEOR(fp_inoutvec(i),fp_invec(i))
    end do
end subroutine koota_bxor_integer8

subroutine koota_min_integer16(invec, inoutvec, len) &
    bind(C,name="koota_min_integer16")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    integer(kind=16), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
       fp_inoutvec(i) = MIN(fp_inoutvec(i),fp_invec(i))
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
    integer(kind=16), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
       fp_inoutvec(i) = MAX(fp_inoutvec(i),fp_invec(i))
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
    integer(kind=16), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
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
    integer(kind=16), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
       fp_inoutvec(i) = fp_inoutvec(i) * fp_invec(i)
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
    integer(kind=16), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
       fp_inoutvec(i) = IAND(fp_inoutvec(i),fp_invec(i))
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
    integer(kind=16), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
       fp_inoutvec(i) = IOR(fp_inoutvec(i),fp_invec(i))
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
    integer(kind=16), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
       fp_inoutvec(i) = IEOR(fp_inoutvec(i),fp_invec(i))
    end do
end subroutine koota_bxor_integer16

subroutine koota_land_logical1(invec, inoutvec, len) &
    bind(C,name="koota_land_logical1")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    logical(kind=1), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
       fp_inoutvec(i) = fp_inoutvec(i) .AND. fp_invec(i)
    end do
end subroutine koota_land_logical1

subroutine koota_lor_logical1(invec, inoutvec, len) &
    bind(C,name="koota_lor_logical1")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    logical(kind=1), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
       fp_inoutvec(i) = fp_inoutvec(i) .OR. fp_invec(i)
    end do
end subroutine koota_lor_logical1

subroutine koota_lxor_logical1(invec, inoutvec, len) &
    bind(C,name="koota_lxor_logical1")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    logical(kind=1), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
       fp_inoutvec(i) = fp_inoutvec(i) .NEQV. fp_invec(i)
    end do
end subroutine koota_lxor_logical1

subroutine koota_land_logical2(invec, inoutvec, len) &
    bind(C,name="koota_land_logical2")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    logical(kind=2), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
       fp_inoutvec(i) = fp_inoutvec(i) .AND. fp_invec(i)
    end do
end subroutine koota_land_logical2

subroutine koota_lor_logical2(invec, inoutvec, len) &
    bind(C,name="koota_lor_logical2")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    logical(kind=2), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
       fp_inoutvec(i) = fp_inoutvec(i) .OR. fp_invec(i)
    end do
end subroutine koota_lor_logical2

subroutine koota_lxor_logical2(invec, inoutvec, len) &
    bind(C,name="koota_lxor_logical2")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    logical(kind=2), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
       fp_inoutvec(i) = fp_inoutvec(i) .NEQV. fp_invec(i)
    end do
end subroutine koota_lxor_logical2

subroutine koota_land_logical4(invec, inoutvec, len) &
    bind(C,name="koota_land_logical4")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    logical(kind=4), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
       fp_inoutvec(i) = fp_inoutvec(i) .AND. fp_invec(i)
    end do
end subroutine koota_land_logical4

subroutine koota_lor_logical4(invec, inoutvec, len) &
    bind(C,name="koota_lor_logical4")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    logical(kind=4), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
       fp_inoutvec(i) = fp_inoutvec(i) .OR. fp_invec(i)
    end do
end subroutine koota_lor_logical4

subroutine koota_lxor_logical4(invec, inoutvec, len) &
    bind(C,name="koota_lxor_logical4")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    logical(kind=4), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
       fp_inoutvec(i) = fp_inoutvec(i) .NEQV. fp_invec(i)
    end do
end subroutine koota_lxor_logical4

subroutine koota_land_logical8(invec, inoutvec, len) &
    bind(C,name="koota_land_logical8")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    logical(kind=8), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
       fp_inoutvec(i) = fp_inoutvec(i) .AND. fp_invec(i)
    end do
end subroutine koota_land_logical8

subroutine koota_lor_logical8(invec, inoutvec, len) &
    bind(C,name="koota_lor_logical8")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    logical(kind=8), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
       fp_inoutvec(i) = fp_inoutvec(i) .OR. fp_invec(i)
    end do
end subroutine koota_lor_logical8

subroutine koota_lxor_logical8(invec, inoutvec, len) &
    bind(C,name="koota_lxor_logical8")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    logical(kind=8), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
       fp_inoutvec(i) = fp_inoutvec(i) .NEQV. fp_invec(i)
    end do
end subroutine koota_lxor_logical8

subroutine koota_land_logical16(invec, inoutvec, len) &
    bind(C,name="koota_land_logical16")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    logical(kind=16), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
       fp_inoutvec(i) = fp_inoutvec(i) .AND. fp_invec(i)
    end do
end subroutine koota_land_logical16

subroutine koota_lor_logical16(invec, inoutvec, len) &
    bind(C,name="koota_lor_logical16")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    logical(kind=16), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
       fp_inoutvec(i) = fp_inoutvec(i) .OR. fp_invec(i)
    end do
end subroutine koota_lor_logical16

subroutine koota_lxor_logical16(invec, inoutvec, len) &
    bind(C,name="koota_lxor_logical16")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    logical(kind=16), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
       fp_inoutvec(i) = fp_inoutvec(i) .NEQV. fp_invec(i)
    end do
end subroutine koota_lxor_logical16
