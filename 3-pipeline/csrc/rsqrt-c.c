// rv32i_fast_rsqrt.c
#include <stdint.h>
#include <stdbool.h>
#include <string.h>

#define printstr(ptr, length)                   \
    do {                                        \
        asm volatile(                           \
            "add a7, x0, 0x40;"                 \
            "add a0, x0, 0x1;" /* stdout */     \
            "add a1, x0, %0;"                   \
            "mv a2, %1;" /* length character */ \
            "ecall;"                            \
            :                                   \
            : "r"(ptr), "r"(length)             \
            : "a0", "a1", "a2", "a7", "memory");          \
    } while (0)

#define TEST_OUTPUT(msg, length) printstr(msg, length)

#define TEST_LOGGER(msg)                     \
    {                                        \
        char _msg[] = msg;                   \
        TEST_OUTPUT(_msg, sizeof(_msg) - 1); \
    }

uint64_t get_cycles(void)
{
    uint32_t lo, hi, hi2;
    do {
        __asm__ volatile ("csrr %0, cycleh" : "=r"(hi));
        __asm__ volatile ("csrr %0, cycle"  : "=r"(lo));
        __asm__ volatile ("csrr %0, cycleh" : "=r"(hi2));
    } while (hi != hi2);

    return ((uint64_t)hi << 32) | (uint64_t)lo;
}


/* Bare metal memcpy implementation */
void *memcpy(void *dest, const void *src, size_t n)
{
    uint8_t *d = (uint8_t *) dest;
    const uint8_t *s = (const uint8_t *) src;
    while (n--)
        *d++ = *s++;
    return dest;
}

/* --------- 32x32 -> 64 shift-add multiply (no MUL) ------------------------- */
/* Algorithm: for each set bit i in b, add (a << i) into 64-bit accumulator.  */
uint64_t mul32(uint32_t a, uint32_t b) {
    uint64_t r = 0;
    for (int i = 0; i < 32; ++i) {
        if (b & (1U << i))                 /* C01 */
            r += ((uint64_t)a << i);       /* C02 (be aware of casting) */
    }
    return r;
}

/* --------- clz: count leading zeros (special case x==0 returns 32) --------- */
int clz(uint32_t x) {
    if (!x) return 32;                 /* Special case: no bits set */
    int n = 0;
    if (!(x & 0xFFFF0000u)) { n += 16; x <<= 16; }
    if (!(x & 0xFF000000u)) { n +=  8; x <<=  8; }
    if (!(x & 0xF0000000u)) { n +=  4; x <<=  4; }
    if (!(x & 0xC0000000u)) { n +=  2; x <<=  2; }
    if (!(x & 0x80000000u)) { n +=  1; }
    return n;
}

// extern uint32_t fast_rsqrt(uint32_t x);

/* --------- Lookup table: initial estimates for 65536 / sqrt(2^exp) --------- */
static const uint32_t rsqrt_table[32] = {
    65536, 46341, 32768, 23170, 16384,  /* 2^0  to 2^4  */
    11585,  8192,  5793,  4096,  2896,  /* 2^5  to 2^9  */
     2048,  1448,  1024,   724,   512,  /* 2^10 to 2^14 */
      362,   256,   181,   128,    90,  /* 2^15 to 2^19 */
       64,    45,    32,    23,    16,  /* 2^20 to 2^24 */
       11,     8,     6,     4,     3,  /* 2^25 to 2^29 */
        2,     1                         /* 2^30, 2^31  */
};

/* --------- Fast reciprocal sqrt: returns y = 65536 / sqrt(x) --------------- */
/* Error ~3–8% after LUT + interpolation + 2 Newton iters (Q16 fixed-point).   */
uint32_t fast_rsqrt(uint32_t x) {
    /* Edge cases */
    if (x == 0) return 0xFFFFFFFFu;   /* Infinity representation */
    if (x == 1) return 65536u;        /* Exact */

    /* Step 1: find MSB position */
    int exp = 31 - clz(x);            /* C03 */

    /* Step 2: LUT initial estimate */
    uint32_t y = rsqrt_table[exp];    /* C04 */

    /* Step 3: Linear interpolation for non power of two */
    if (x > (1u << exp)) {
        /* Bound-safe next table entry; exp==31 → use 0 */
        uint32_t y_next = (exp < 31) ? rsqrt_table[exp + 1] : 0;  /* C05 */
        uint32_t delta  = y - y_next;                             /* C06 */
        /* frac = ((x - 2^exp) / 2^exp) in Q16 */
        uint32_t frac   = (uint32_t)(((((uint64_t)x - (1ULL << exp)) << 16) >> exp)); /* C07 */
        /* y -= delta * frac / 2^16 */
        y -= (uint32_t)(mul32(delta,frac) >> 16);          /* C08 */
    }

    /* Step 4: two Newton-Raphson iterations in Q16:
       y_{n+1} = y_n * (3/2 - x*y_n^2/2^16) / 1
       Implemented as: y = (y * ((3<<16) - (x*y^2>>16))) >> 17
    */
    for (int iter = 0; iter < 2; ++iter) {
        uint32_t y2  = (uint32_t)mul32(y, y);                     /* C09 (Q32) */
        uint32_t xy2 = (uint32_t)(mul32(x, y2) >> 16);            /* C10 (Q16) */
        // TEST_LOGGER("Debug:  (3u << 16) - xy2 = ");
        // print_dec((3u << 16) - xy2);
        // TEST_LOGGER("\n");
        y = (uint32_t)(mul32(y, (3u << 16) - xy2) >> 17);         /* C11 */
    }

    return y;
}


/* ------------------------- 小型測試 ------------------------------ */
int main(void) {

    uint64_t start_cycles, end_cycles, cycles_elapsed;

    uint32_t xs[] = {1, 4, 16, 20, 30, 100, 120, 130, 0, 0xFFFFFFFFu};
    int l = 4, j = 8;

    for (unsigned i = 0; i < sizeof(xs)/sizeof(xs[0]); ++i) {
        uint32_t x = xs[i];

        start_cycles = get_cycles();

        uint32_t y = fast_rsqrt(x);
        *(uint32_t *) l = y;

        end_cycles = get_cycles();
        cycles_elapsed = end_cycles - start_cycles;

        volatile uint32_t *p = (uint32_t *) j;
        p[0] = (uint32_t)(cycles_elapsed & 0xffffffffu);   // low word
        p[1] = (uint32_t)(cycles_elapsed >> 32);  

        l += 12;
        j += 12;
    }
    return 0;
}
