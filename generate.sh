#!/bin/bash

t=$1
s=$2
op=$3
symbol=koota_${op}_${t}${s}
echo "
subroutine ${symbol}(invec, inoutvec, len) &
    bind(C,name=\"${symbol}\")
    use, intrinsic :: iso_c_binding, only : c_ptr, c_size_t, c_f_pointer
    implicit none
    type(c_ptr), intent(in), value :: invec
    type(c_ptr), intent(in), value :: inoutvec
    integer(kind=c_size_t), intent(in), value :: len

    integer(kind=c_size_t) :: i
    $t(kind=$s), pointer, dimension(:), contiguous :: fp_invec, fp_inoutvec
    call C_F_POINTER(invec,fp_invec,[len])
    call C_F_POINTER(inoutvec,fp_inoutvec,[len])

    do i=1,len"
                if [ $op=="sum" ] ; then
                    echo "        fp_inoutvec(i) = fp_inoutvec(i) + fp_invec(i)"
                elif [ $op=="prod" ] ; then
                    echo "        fp_inoutvec(i) = fp_inoutvec(i) * fp_invec(i)"
                elif [ $op=="land" ] ; then
                    echo "        fp_inoutvec(i) = fp_inoutvec(i) .AND. fp_invec(i)"
                elif [ $op=="lor" ] ; then
                    echo "        fp_inoutvec(i) = fp_inoutvec(i) .OR. fp_invec(i)"
                elif [ $op=="lxor" ] ; then
                    echo "        fp_inoutvec(i) = fp_inoutvec(i) .NEQV. fp_invec(i)"
                elif [ $op=="band" ] ; then
                    echo "        fp_inoutvec(i) = IAND(fp_inoutvec(i),fp_invec(i))"
                elif [ $op=="bor" ] ; then
                    echo "        fp_inoutvec(i) = IOR(fp_inoutvec(i),fp_invec(i))"
                elif [ $op=="bxor" ] ; then
                    echo "        fp_inoutvec(i) = IEOR(fp_inoutvec(i),fp_invec(i))"
                elif [ $op=="max" ] ; then
                    echo "        fp_inoutvec(i) = MAX(fp_inoutvec(i),fp_invec(i))"
                elif [ $op=="min" ] ; then
                    echo "        fp_inoutvec(i) = MIN(fp_inoutvec(i),fp_invec(i))"
                fi
echo "    end do
end subroutine ${symbol}"
