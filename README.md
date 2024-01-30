# Koota

Fortran implementations of MPI Reductions.

This is a Finnish word that is roughly equivalent to what "reduction" means in MPI.

The intuitive English pronunciation is wrong.  Listen to https://forvo.com/word/koota/.

# Design

We implement type-specific reductions with the length passed by value as `size_t`
because these will be called internally after the MPI implementation has 
determined the type.  We do not need two versions for large-count.

This is necessary because CFI types aren't what MPI operates on,
and Fortran types aren't necessarily C interoperable, so we cannot
use `bind(C)` if we declare the arrays with Fortran types.
If we do not comply with `bind(C)`, then we can't specify the C linkage symbol name.
```fortran
! call C_F_POINTER(cptr,fptr[,shape])
<type>, pointer, dimension(:) :: fiptr, fioptr
call C_F_POINTER(invec,fiptr,[len])
call C_F_POINTER(inoutvec,fioptr,[len])
```

This is the list of reductions required by MPI:

Op | Allowed Types
-- | -------------
`MPI_MAX`, `MPI_MIN`              |  integer, floating point
`MPI_SUM`, `MPI_PROD`             |  integer, floating point, complex,
`MPI_LAND`, `MPI_LOR`, `MPI_LXOR` |  logical
`MPI_BAND`, `MPI_BOR`, `MPI_BXOR` |  integer, byte




