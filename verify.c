#include <stdint.h>
#include <stddef.h>
#include <math.h>
#include <complex.h>

#define MAX(a,b) (((a)>(b))?(a):(b))

void verify_sum_real4(float * invec, float * inoutvec, size_t len)
{
    for (size_t i=0; i<len; i++) {
        inoutvec[i] = inoutvec[i] + invec[i];
    }
}

void verify_sum_complex4(_Complex float * invec, _Complex float * inoutvec, size_t len)
{
    for (size_t i=0; i<len; i++) {
        inoutvec[i] = inoutvec[i] + invec[i];
    }
}

void verify_max_real4(float * invec,float * inoutvec, size_t len)
{
    for (size_t i=0; i<len; i++) {
        inoutvec[i] = MAX(inoutvec[i],invec[i]);
    }
}

void verify_band_integer4(int32_t * invec, int32_t * inoutvec, size_t len)
{
    for (size_t i=0; i<len; i++) {
        inoutvec[i] = inoutvec[i] & invec[i];
    }
}

void verify_land_logical4(int32_t * invec, int32_t * inoutvec, size_t len)
{
    for (size_t i=0; i<len; i++) {
        inoutvec[i] = inoutvec[i] && invec[i];
    }
}

