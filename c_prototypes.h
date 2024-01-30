#ifndef C_PROTOTYPES_H
#define C_PROTOTYPES_H

#include <stdint.h>
#include <stddef.h>
#include <math.h>
#include <complex.h>

// koota_<op>_<type>
// koota_<op>_<type>_c

void koota_min_real4(void * invec, void * inoutvec, size_t len);
void koota_max_real4(void * invec, void * inoutvec, size_t len);
void koota_sum_real4(void * invec, void * inoutvec, size_t len);
void koota_prod_real4(void * invec, void * inoutvec, size_t len);
void koota_band_real4(void * invec, void * inoutvec, size_t len);
void koota_bor_real4(void * invec, void * inoutvec, size_t len);
void koota_bxor_real4(void * invec, void * inoutvec, size_t len);
void koota_land_real4(void * invec, void * inoutvec, size_t len);
void koota_lor_real4(void * invec, void * inoutvec, size_t len);
void koota_lxor_real4(void * invec, void * inoutvec, size_t len);
void koota_min_real8(void * invec, void * inoutvec, size_t len);
void koota_max_real8(void * invec, void * inoutvec, size_t len);
void koota_sum_real8(void * invec, void * inoutvec, size_t len);
void koota_prod_real8(void * invec, void * inoutvec, size_t len);
void koota_band_real8(void * invec, void * inoutvec, size_t len);
void koota_bor_real8(void * invec, void * inoutvec, size_t len);
void koota_bxor_real8(void * invec, void * inoutvec, size_t len);
void koota_land_real8(void * invec, void * inoutvec, size_t len);
void koota_lor_real8(void * invec, void * inoutvec, size_t len);
void koota_lxor_real8(void * invec, void * inoutvec, size_t len);
void koota_min_real16(void * invec, void * inoutvec, size_t len);
void koota_max_real16(void * invec, void * inoutvec, size_t len);
void koota_sum_real16(void * invec, void * inoutvec, size_t len);
void koota_prod_real16(void * invec, void * inoutvec, size_t len);
void koota_band_real16(void * invec, void * inoutvec, size_t len);
void koota_bor_real16(void * invec, void * inoutvec, size_t len);
void koota_bxor_real16(void * invec, void * inoutvec, size_t len);
void koota_land_real16(void * invec, void * inoutvec, size_t len);
void koota_lor_real16(void * invec, void * inoutvec, size_t len);
void koota_lxor_real16(void * invec, void * inoutvec, size_t len);
void koota_min_complex4(void * invec, void * inoutvec, size_t len);
void koota_max_complex4(void * invec, void * inoutvec, size_t len);
void koota_sum_complex4(void * invec, void * inoutvec, size_t len);
void koota_prod_complex4(void * invec, void * inoutvec, size_t len);
void koota_band_complex4(void * invec, void * inoutvec, size_t len);
void koota_bor_complex4(void * invec, void * inoutvec, size_t len);
void koota_bxor_complex4(void * invec, void * inoutvec, size_t len);
void koota_land_complex4(void * invec, void * inoutvec, size_t len);
void koota_lor_complex4(void * invec, void * inoutvec, size_t len);
void koota_lxor_complex4(void * invec, void * inoutvec, size_t len);
void koota_min_complex8(void * invec, void * inoutvec, size_t len);
void koota_max_complex8(void * invec, void * inoutvec, size_t len);
void koota_sum_complex8(void * invec, void * inoutvec, size_t len);
void koota_prod_complex8(void * invec, void * inoutvec, size_t len);
void koota_band_complex8(void * invec, void * inoutvec, size_t len);
void koota_bor_complex8(void * invec, void * inoutvec, size_t len);
void koota_bxor_complex8(void * invec, void * inoutvec, size_t len);
void koota_land_complex8(void * invec, void * inoutvec, size_t len);
void koota_lor_complex8(void * invec, void * inoutvec, size_t len);
void koota_lxor_complex8(void * invec, void * inoutvec, size_t len);
void koota_min_complex16(void * invec, void * inoutvec, size_t len);
void koota_max_complex16(void * invec, void * inoutvec, size_t len);
void koota_sum_complex16(void * invec, void * inoutvec, size_t len);
void koota_prod_complex16(void * invec, void * inoutvec, size_t len);
void koota_band_complex16(void * invec, void * inoutvec, size_t len);
void koota_bor_complex16(void * invec, void * inoutvec, size_t len);
void koota_bxor_complex16(void * invec, void * inoutvec, size_t len);
void koota_land_complex16(void * invec, void * inoutvec, size_t len);
void koota_lor_complex16(void * invec, void * inoutvec, size_t len);
void koota_lxor_complex16(void * invec, void * inoutvec, size_t len);
void koota_min_integer1(void * invec, void * inoutvec, size_t len);
void koota_max_integer1(void * invec, void * inoutvec, size_t len);
void koota_sum_integer1(void * invec, void * inoutvec, size_t len);
void koota_prod_integer1(void * invec, void * inoutvec, size_t len);
void koota_band_integer1(void * invec, void * inoutvec, size_t len);
void koota_bor_integer1(void * invec, void * inoutvec, size_t len);
void koota_bxor_integer1(void * invec, void * inoutvec, size_t len);
void koota_land_integer1(void * invec, void * inoutvec, size_t len);
void koota_lor_integer1(void * invec, void * inoutvec, size_t len);
void koota_lxor_integer1(void * invec, void * inoutvec, size_t len);
void koota_min_integer2(void * invec, void * inoutvec, size_t len);
void koota_max_integer2(void * invec, void * inoutvec, size_t len);
void koota_sum_integer2(void * invec, void * inoutvec, size_t len);
void koota_prod_integer2(void * invec, void * inoutvec, size_t len);
void koota_band_integer2(void * invec, void * inoutvec, size_t len);
void koota_bor_integer2(void * invec, void * inoutvec, size_t len);
void koota_bxor_integer2(void * invec, void * inoutvec, size_t len);
void koota_land_integer2(void * invec, void * inoutvec, size_t len);
void koota_lor_integer2(void * invec, void * inoutvec, size_t len);
void koota_lxor_integer2(void * invec, void * inoutvec, size_t len);
void koota_min_integer4(void * invec, void * inoutvec, size_t len);
void koota_max_integer4(void * invec, void * inoutvec, size_t len);
void koota_sum_integer4(void * invec, void * inoutvec, size_t len);
void koota_prod_integer4(void * invec, void * inoutvec, size_t len);
void koota_band_integer4(void * invec, void * inoutvec, size_t len);
void koota_bor_integer4(void * invec, void * inoutvec, size_t len);
void koota_bxor_integer4(void * invec, void * inoutvec, size_t len);
void koota_land_integer4(void * invec, void * inoutvec, size_t len);
void koota_lor_integer4(void * invec, void * inoutvec, size_t len);
void koota_lxor_integer4(void * invec, void * inoutvec, size_t len);
void koota_min_integer8(void * invec, void * inoutvec, size_t len);
void koota_max_integer8(void * invec, void * inoutvec, size_t len);
void koota_sum_integer8(void * invec, void * inoutvec, size_t len);
void koota_prod_integer8(void * invec, void * inoutvec, size_t len);
void koota_band_integer8(void * invec, void * inoutvec, size_t len);
void koota_bor_integer8(void * invec, void * inoutvec, size_t len);
void koota_bxor_integer8(void * invec, void * inoutvec, size_t len);
void koota_land_integer8(void * invec, void * inoutvec, size_t len);
void koota_lor_integer8(void * invec, void * inoutvec, size_t len);
void koota_lxor_integer8(void * invec, void * inoutvec, size_t len);
void koota_min_integer16(void * invec, void * inoutvec, size_t len);
void koota_max_integer16(void * invec, void * inoutvec, size_t len);
void koota_sum_integer16(void * invec, void * inoutvec, size_t len);
void koota_prod_integer16(void * invec, void * inoutvec, size_t len);
void koota_band_integer16(void * invec, void * inoutvec, size_t len);
void koota_bor_integer16(void * invec, void * inoutvec, size_t len);
void koota_bxor_integer16(void * invec, void * inoutvec, size_t len);
void koota_land_integer16(void * invec, void * inoutvec, size_t len);
void koota_lor_integer16(void * invec, void * inoutvec, size_t len);
void koota_lxor_integer16(void * invec, void * inoutvec, size_t len);

#endif // C_PROTOTYPES_H
