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

    for (i = 0; i < 8; i++){
        result <<= popcount_vec[7-i];
        result += result_vec[7-i];
    }

/*
    volatile uint8_t popcount_array[8], result_array[8];
    vst1_u8(popcount_array, popcount_vec);
    vst1_u8(result_array, result_vec);

    for (i = 0; i < 8; i++){
        result <<= popcount_array[7-i];
        result += result_array[7-i];
    }
*/
    return result;
}

/*
int main(int argc, char * argv[])
{
    uint64_t pop_mask = atoi(argv[1]),
             set_mask = atoi(argv[2]);
    printf("pext(%llu, %llu) = %llu\n", pop_mask, set_mask, vector_pext(pop_mask, set_mask));
}
*/

CAMLprim value vector_pext_stub(value a, value b)
{
    return Val_long(vector_pext(Long_val(a), Long_val(b)));
}

