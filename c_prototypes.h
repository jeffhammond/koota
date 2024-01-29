#ifndef C_PROTOTYPES_H
#define C_PROTOTYPES_H

#include <math.h>
#include <complex.h>

// koota_<op>_<type>
// koota_<op>_<type>_c

void koota_sum_real4(float * invec, float * inoutvec, size_t len);
void koota_sum_complex4(_Complex float * invec, _Complex float * inoutvec, size_t len);
void koota_max_real4(float * invec,float * inoutvec, size_t len);
void koota_band_integer4(int32_t * invec, int32_t * inoutvec, size_t len);
void koota_land_logical4(int32_t * invec, int32_t * inoutvec, size_t len);

#endif // C_PROTOTYPES_H
