# Koota

Fortran implementations of MPI Reductions

# Design

```fortran
ABSTRACT INTERFACE
  SUBROUTINE MPI_User_function(invec, inoutvec, len, datatype)
    USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_PTR
    TYPE(C_PTR), VALUE :: invec, inoutvec
    INTEGER :: len
    TYPE(MPI_Datatype) :: datatype
ABSTRACT INTERFACE
  SUBROUTINE MPI_User_function_c(invec, inoutvec, len, datatype) !(_c)
    USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_PTR
    TYPE(C_PTR), VALUE :: invec, inoutvec
    INTEGER(KIND=MPI_COUNT_KIND) :: len
    TYPE(MPI_Datatype) :: datatype
```

Op | Allowed Types
-- | -------------
`MPI_MAX`, `MPI_MIN`              |  integer, floating point
`MPI_SUM`, `MPI_PROD`             |  integer, floating point, complex,
`MPI_LAND`, `MPI_LOR`, `MPI_LXOR` |  logical
`MPI_BAND`, `MPI_BOR`, `MPI_BXOR` |  integer, byte




