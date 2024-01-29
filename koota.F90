
subroutine koota_sum_complex2(invec, inoutvec, len) &
    bind(C,name="koota_sum_complex2")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    complex(2), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_sum_complex2

subroutine koota_prod_complex2(invec, inoutvec, len) &
    bind(C,name="koota_prod_complex2")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    complex(2), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_prod_complex2

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

subroutine koota_sum_complex32(invec, inoutvec, len) &
    bind(C,name="koota_sum_complex32")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    complex(32), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_sum_complex32

subroutine koota_prod_complex32(invec, inoutvec, len) &
    bind(C,name="koota_prod_complex32")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    complex(32), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_prod_complex32

subroutine koota_sum_real2(invec, inoutvec, len) &
    bind(C,name="koota_sum_real2")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    real(2), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_sum_real2

subroutine koota_prod_real2(invec, inoutvec, len) &
    bind(C,name="koota_prod_real2")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    real(2), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_prod_real2

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

subroutine koota_sum_real32(invec, inoutvec, len) &
    bind(C,name="koota_sum_real32")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    real(32), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_sum_real32

subroutine koota_prod_real32(invec, inoutvec, len) &
    bind(C,name="koota_prod_real32")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    real(32), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_prod_real32

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

subroutine koota_sum_integer32(invec, inoutvec, len) &
    bind(C,name="koota_sum_integer32")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    integer(32), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_sum_integer32

subroutine koota_prod_integer32(invec, inoutvec, len) &
    bind(C,name="koota_prod_integer32")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    integer(32), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len
        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)
    end do
end subroutine koota_prod_integer32
