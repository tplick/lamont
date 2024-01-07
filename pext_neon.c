// Jan 4, 2023

#include <arm_neon.h>
#include <caml/mlvalues.h>

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

uint64_t vector_pext(uint64_t pop_mask, uint64_t set_mask)
{
    uint8x8_t pop_vec = (uint8x8_t) pop_mask,
              set_vec = (uint8x8_t) set_mask,
              result_vec = {0},
              clz_vec,
              clo_vec,
              clb_vec,
              popcount_vec;

    set_vec &= pop_vec;
    popcount_vec = vcnt_u8(pop_vec);

    int i, rounds;

    for (rounds = 0; rounds < 4; rounds++){
        clz_vec = vclz_u8(pop_vec);
        pop_vec <<= clz_vec;
        clo_vec = vclz_u8(~pop_vec);
        pop_vec <<= clo_vec;
        clb_vec = clz_vec + clo_vec;

        result_vec <<= clo_vec;
        result_vec += set_vec >> (8 - clb_vec);
        set_vec <<= clb_vec;
    }

    uint64_t result = 0LL;
    uint64_t popcount_field = (uint64_t) vrev64_u8(popcount_vec),
             result_field = (uint64_t) vrev64_u8(result_vec);

    for (i = 0; i < 8; i++){
        result <<= (popcount_field & 255);
        result += (result_field & 255);

        popcount_field >>= 8;
        result_field >>= 8;
    }

    return result;
}

uint8x16_t separate(uint64_t mask)
{
    const uint64_t low_nibbles = 0x0f0f0f0f0f0f0f0fULL;
    return vcombine_u8((uint8x8_t)(mask & low_nibbles),
                       (uint8x8_t)((mask >> 4) & low_nibbles));
}

uint8x8_t rejoin(uint8x16_t r_vec, uint8x8_t popcount_vec)
{
    uint8x8_t low = vrev64_u8(vget_low_u8(r_vec)),
              high = vrev64_u8(vget_high_u8(r_vec));
    return low + (high << popcount_vec);
}

void vector_pext_3(uint64_t result_array[],
                   uint64_t pop_mask,
                   uint64_t a_mask, uint64_t b_mask, uint64_t c_mask)
{
    uint8x16_t pop_vec = separate(pop_mask),
               a_vec = separate(a_mask),
               b_vec = separate(b_mask),
               c_vec = separate(c_mask),
               ra_vec = {0},
               rb_vec = {0},
               rc_vec = {0},
               clz_vec,
               clo_vec,
               clb_vec,
               popcount_vec;

    a_vec &= pop_vec;
    b_vec &= pop_vec;
    c_vec &= pop_vec;
    popcount_vec = vcntq_u8(pop_vec);

    int i, rounds;

    for (rounds = 0; rounds < 2; rounds++){
        clz_vec = vclzq_u8(pop_vec);
        pop_vec <<= clz_vec;
        clo_vec = vclzq_u8(~pop_vec);
        pop_vec <<= clo_vec;
        clb_vec = clz_vec + clo_vec;

        ra_vec <<= clo_vec;
        rb_vec <<= clo_vec;
        rc_vec <<= clo_vec;

        ra_vec += a_vec >> (8 - clb_vec);
        rb_vec += b_vec >> (8 - clb_vec);
        rc_vec += c_vec >> (8 - clb_vec);

        a_vec <<= clb_vec;
        b_vec <<= clb_vec;
        c_vec <<= clb_vec;
    }

    uint8x8_t popcount_low = vrev64_u8(vget_low_u8(popcount_vec)),
              popcount_high = vrev64_u8(vget_high_u8(popcount_vec));
    uint64_t a_field = (uint64_t) rejoin(ra_vec, popcount_low),
             b_field = (uint64_t) rejoin(rb_vec, popcount_low),
             c_field = (uint64_t) rejoin(rc_vec, popcount_low);
    uint64_t popcount_field = (uint64_t) (popcount_low + popcount_high);

    uint64_t a_result = 0LL, b_result = 0LL, c_result = 0LL;

    for (i = 0; i < 8; i++){
        a_result <<= (popcount_field & 255);
        b_result <<= (popcount_field & 255);
        c_result <<= (popcount_field & 255);

        a_result += (a_field & 255);
        b_result += (b_field & 255);
        c_result += (c_field & 255);

        popcount_field >>= 8;
        a_field >>= 8;
        b_field >>= 8;
        c_field >>= 8;
    }

    result_array[0] = a_result;
    result_array[1] = b_result;
    result_array[2] = c_result;
}

CAMLprim value vector_pext_stub(value a, value b)
{
    return Val_long(vector_pext(Long_val(a), Long_val(b)));
}

uint64_t pext_3_values[3];

CAMLprim value vector_pext_3_stub(value a, value b, value c, value d)
{
    uint64_t a_ = Long_val(a),
             b_ = Long_val(b),
             c_ = Long_val(c),
             d_ = Long_val(d);
    vector_pext_3(pext_3_values, a_, b_, c_, d_);
    return Val_unit;
}

CAMLprim value vector_pext_3_retrieve(value idx)
{
    return Val_long(pext_3_values[Long_val(idx)]);
}

