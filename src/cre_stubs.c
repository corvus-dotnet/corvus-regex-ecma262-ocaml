/* The C parts of corvus-regex-ecma262: the monotonic clock of the match timeout, and the search kernels. Every function
   here is called from OCaml as an external that allocates nothing ([@@noalloc], with untagged ints), so a call is a
   plain C call. The text is an OCaml string, which cannot move during a call that allocates nothing, and every
   position is an offset into it.

   Each kernel reads only below n, the end of the text, which the caller has checked lies within the string. Over
   x86-64 the kernels read 16 bytes at a time with SSE2 (which every x86-64 processor has), and some 32 at a time with
   AVX2 or use SSSE3's byte shuffle where the processor has them (asked once, at the first call); over AArch64 they
   read 16 bytes at a time with NEON (which every AArch64 processor has); over any other processor, and for the bytes
   past the last whole block, they read a byte at a time. They are the kernels of the Object Pascal engine's Swar unit
   (and the Rust crate's memchr and aho-corasick), written in C. */

#define CAML_NAME_SPACE
#include <caml/mlvalues.h>
#include <stdint.h>
#include <string.h>

#ifdef _WIN32
#include <windows.h>
#else
#include <time.h>
#endif

#if defined(__x86_64__) || defined(_M_X64)
#define CRE_X86 1
#include <immintrin.h>
#elif defined(__aarch64__) || defined(_M_ARM64)
#define CRE_NEON 1
#include <arm_neon.h>
#endif

#if defined(CRE_X86) && (defined(__GNUC__) || defined(__clang__))
#define CRE_TARGET(t) __attribute__((target(t)))
#define CRE_HAS_TARGETS 1
#else
#define CRE_TARGET(t)
#endif

#if defined(__GNUC__) || defined(__clang__)
#define CRE_CTZ32(x) __builtin_ctz(x)
#define CRE_CTZ64(x) __builtin_ctzll(x)
#else
static int CRE_CTZ32(uint32_t x) { unsigned long k; _BitScanForward(&k, x); return (int)k; }
static int CRE_CTZ64(uint64_t x) { unsigned long k; _BitScanForward64(&k, x); return (int)k; }
#endif

typedef const unsigned char *bytes;
#define TEXT(v) ((bytes)String_val(v))

/* The bytecode forms of the externals take and return tagged values; the native forms below take untagged ints. */
#define ARG(k) Long_val(argv[k])

/* ---------------------------------------------------------------------------------------------------------------- */
/* The clock */

/* The monotonic clock in nanoseconds. OCaml 4.14's standard library has no monotonic clock. */
intnat cre_now_ns(value unit)
{
    (void)unit;
#ifdef _WIN32
    static LARGE_INTEGER frequency;
    LARGE_INTEGER now;
    if (frequency.QuadPart == 0)
        QueryPerformanceFrequency(&frequency);
    QueryPerformanceCounter(&now);
    return (intnat)((double)now.QuadPart * 1e9 / (double)frequency.QuadPart);
#else
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return (intnat)ts.tv_sec * 1000000000 + (intnat)ts.tv_nsec;
#endif
}

value cre_now_ns_byte(value unit) { return Val_long(cre_now_ns(unit)); }

/* ---------------------------------------------------------------------------------------------------------------- */
/* What the processor has */

#ifdef CRE_X86
static int cpu_known, cpu_avx2, cpu_ssse3;

static void cpu_ask(void)
{
#if defined(__GNUC__) || defined(__clang__)
    __builtin_cpu_init();
    cpu_avx2 = __builtin_cpu_supports("avx2");
    cpu_ssse3 = __builtin_cpu_supports("ssse3");
#else
    int r[4];
    __cpuid(r, 1);
    cpu_ssse3 = (r[2] >> 9) & 1;
    cpu_avx2 = 0;
#endif
    cpu_known = 1;
}

static int has_avx2(void) { if (!cpu_known) cpu_ask(); return cpu_avx2; }
static int has_ssse3(void) { if (!cpu_known) cpu_ask(); return cpu_ssse3; }
#endif

/* Whether the processor has a byte shuffle (SSSE3 over x86-64, always over AArch64), which Teddy reads its tables
   with: without one the table finder reads its table a byte at a time. */
intnat cre_has_shuffle(value unit)
{
    (void)unit;
#if defined(CRE_X86)
    return has_ssse3();
#elif defined(CRE_NEON)
    return 1;
#else
    return 0;
#endif
}

value cre_has_shuffle_byte(value unit) { return Val_long(cre_has_shuffle(unit)); }

/* Whether the processor reads 32 bytes at a time (AVX2), which the searches for a literal's two rarest bytes pay for
   only there. */
intnat cre_has_wide(value unit)
{
    (void)unit;
#if defined(CRE_X86)
    return has_avx2();
#else
    return 0;
#endif
}

value cre_has_wide_byte(value unit) { return Val_long(cre_has_wide(unit)); }

/* ---------------------------------------------------------------------------------------------------------------- */
/* Masks of 16 lanes */

#if defined(CRE_NEON)
/* The lanes of a comparison as 4 bits each in a 64-bit word (the narrowing shift memchr uses, NEON having no movemask):
   the first lane set is the count of trailing zeros over 4. */
static inline uint64_t neon_mask(uint8x16_t eq)
{
    return vget_lane_u64(vreinterpret_u64_u8(vshrn_n_u16(vreinterpretq_u16_u8(eq), 4)), 0);
}
#endif

/* ---------------------------------------------------------------------------------------------------------------- */
/* One to four bytes, and ranges */

/* The first position in [from, n) whose byte is b, or -1: libc's memchr, which every platform has in vector code. */
intnat cre_index_byte1(value s, intnat from, intnat n, intnat b)
{
    if (from >= n) return -1;
    const unsigned char *p = memchr(TEXT(s) + from, (int)b, (size_t)(n - from));
    return p ? (intnat)(p - TEXT(s)) : -1;
}

value cre_index_byte1_byte(value s, value from, value n, value b)
{
    return Val_long(cre_index_byte1(s, Long_val(from), Long_val(n), Long_val(b)));
}

/* The first position in [from, n) whose byte is a or b, or -1. */
intnat cre_index_byte2(value s, intnat from, intnat n, intnat a, intnat b)
{
    bytes t = TEXT(s);
    intnat k = from;
#if defined(CRE_X86)
    __m128i va = _mm_set1_epi8((char)a), vb = _mm_set1_epi8((char)b);
    for (; k + 16 <= n; k += 16) {
        __m128i x = _mm_loadu_si128((const __m128i *)(t + k));
        int m = _mm_movemask_epi8(_mm_or_si128(_mm_cmpeq_epi8(x, va), _mm_cmpeq_epi8(x, vb)));
        if (m) return k + CRE_CTZ32((uint32_t)m);
    }
#elif defined(CRE_NEON)
    uint8x16_t va = vdupq_n_u8((uint8_t)a), vb = vdupq_n_u8((uint8_t)b);
    for (; k + 16 <= n; k += 16) {
        uint8x16_t x = vld1q_u8(t + k);
        uint64_t m = neon_mask(vorrq_u8(vceqq_u8(x, va), vceqq_u8(x, vb)));
        if (m) return k + (CRE_CTZ64(m) >> 2);
    }
#endif
    for (; k < n; k++)
        if (t[k] == a || t[k] == b) return k;
    return -1;
}

value cre_index_byte2_byte(value s, value from, value n, value a, value b)
{
    return Val_long(cre_index_byte2(s, Long_val(from), Long_val(n), Long_val(a), Long_val(b)));
}

/* The first position in [from, n) whose byte is one of a, b, c and d (which may repeat), or -1. */
intnat cre_index_byte4(value s, intnat from, intnat n, intnat a, intnat b, intnat c, intnat d)
{
    bytes t = TEXT(s);
    intnat k = from;
#if defined(CRE_X86)
    __m128i va = _mm_set1_epi8((char)a), vb = _mm_set1_epi8((char)b), vc = _mm_set1_epi8((char)c),
            vd = _mm_set1_epi8((char)d);
    for (; k + 16 <= n; k += 16) {
        __m128i x = _mm_loadu_si128((const __m128i *)(t + k));
        __m128i e = _mm_or_si128(_mm_or_si128(_mm_cmpeq_epi8(x, va), _mm_cmpeq_epi8(x, vb)),
                                 _mm_or_si128(_mm_cmpeq_epi8(x, vc), _mm_cmpeq_epi8(x, vd)));
        int m = _mm_movemask_epi8(e);
        if (m) return k + CRE_CTZ32((uint32_t)m);
    }
#elif defined(CRE_NEON)
    uint8x16_t va = vdupq_n_u8((uint8_t)a), vb = vdupq_n_u8((uint8_t)b), vc = vdupq_n_u8((uint8_t)c),
               vd = vdupq_n_u8((uint8_t)d);
    for (; k + 16 <= n; k += 16) {
        uint8x16_t x = vld1q_u8(t + k);
        uint8x16_t e = vorrq_u8(vorrq_u8(vceqq_u8(x, va), vceqq_u8(x, vb)), vorrq_u8(vceqq_u8(x, vc), vceqq_u8(x, vd)));
        uint64_t m = neon_mask(e);
        if (m) return k + (CRE_CTZ64(m) >> 2);
    }
#endif
    for (; k < n; k++)
        if (t[k] == a || t[k] == b || t[k] == c || t[k] == d) return k;
    return -1;
}

value cre_index_byte4_byte(value *argv, int argn)
{
    (void)argn;
    return Val_long(cre_index_byte4(argv[0], ARG(1), ARG(2), ARG(3), ARG(4), ARG(5), ARG(6)));
}

/* The first position in [from, n) whose byte is in one of nr ranges (one to four), or -1. ranges holds each range as
   two bytes, its least byte and its width (greatest less least). */
intnat cre_index_ranges(value s, intnat from, intnat n, value ranges, intnat nr)
{
    bytes t = TEXT(s), r = TEXT(ranges);
    intnat k = from;
#if defined(CRE_X86)
    __m128i lo[4], w[4];
    for (int q = 0; q < nr; q++) {
        lo[q] = _mm_set1_epi8((char)r[2 * q]);
        w[q] = _mm_set1_epi8((char)r[2 * q + 1]);
    }
    for (; k + 16 <= n; k += 16) {
        __m128i x = _mm_loadu_si128((const __m128i *)(t + k)), any = _mm_setzero_si128();
        for (int q = 0; q < nr; q++) {
            /* x - lo, unsigned, is at most the width when x is in the range: min(d, w) = d. */
            __m128i dlt = _mm_sub_epi8(x, lo[q]);
            any = _mm_or_si128(any, _mm_cmpeq_epi8(_mm_min_epu8(dlt, w[q]), dlt));
        }
        int m = _mm_movemask_epi8(any);
        if (m) return k + CRE_CTZ32((uint32_t)m);
    }
#elif defined(CRE_NEON)
    uint8x16_t lo[4], w[4];
    for (int q = 0; q < nr; q++) {
        lo[q] = vdupq_n_u8(r[2 * q]);
        w[q] = vdupq_n_u8(r[2 * q + 1]);
    }
    for (; k + 16 <= n; k += 16) {
        uint8x16_t x = vld1q_u8(t + k), any = vdupq_n_u8(0);
        for (int q = 0; q < nr; q++) any = vorrq_u8(any, vcleq_u8(vsubq_u8(x, lo[q]), w[q]));
        uint64_t m = neon_mask(any);
        if (m) return k + (CRE_CTZ64(m) >> 2);
    }
#endif
    for (; k < n; k++)
        for (int q = 0; q < nr; q++)
            if ((unsigned char)(t[k] - r[2 * q]) <= r[2 * q + 1]) return k;
    return -1;
}

value cre_index_ranges_byte(value s, value from, value n, value ranges, value nr)
{
    return Val_long(cre_index_ranges(s, Long_val(from), Long_val(n), ranges, Long_val(nr)));
}

/* The first position in [from, n) whose byte is not an ASCII member of the set whose nibble bitmap is nib (16 bytes:
   byte l holds bit h for the member 16 * h + l), or n. A byte at or beyond 0x80 has a high nibble of 8 or more, whose
   bit is none of the eight, so it ends the run. With a byte shuffle, sixteen bytes a step: the bitmap's byte of each
   byte's low nibble, and the bit of its high nibble. */
#if defined(CRE_HAS_TARGETS)
CRE_TARGET("ssse3")
static intnat ascii_run_ssse3(bytes t, intnat k, intnat n, bytes nib)
{
    __m128i map = _mm_loadu_si128((const __m128i *)nib);
    __m128i bit = _mm_setr_epi8(1, 2, 4, 8, 16, 32, 64, (char)128, 0, 0, 0, 0, 0, 0, 0, 0);
    __m128i low = _mm_set1_epi8(0x0F), zero = _mm_setzero_si128();
    for (; k + 16 <= n; k += 16) {
        __m128i x = _mm_loadu_si128((const __m128i *)(t + k));
        __m128i row = _mm_shuffle_epi8(map, _mm_and_si128(x, low));
        __m128i b = _mm_shuffle_epi8(bit, _mm_and_si128(_mm_srli_epi16(x, 4), low));
        int m = _mm_movemask_epi8(_mm_cmpeq_epi8(_mm_and_si128(row, b), zero));
        if (m) return k + CRE_CTZ32((uint32_t)m);
    }
    return k;
}
#endif

intnat cre_ascii_run_end(value s, intnat from, intnat n, value nibbles)
{
    bytes t = TEXT(s), nib = TEXT(nibbles);
    intnat k = from;
#if defined(CRE_HAS_TARGETS)
    if (has_ssse3()) {
        k = ascii_run_ssse3(t, k, n, nib);
        if (k + 16 <= n) return k;
    }
#elif defined(CRE_NEON)
    {
        uint8x16_t map = vld1q_u8(nib);
        static const uint8_t bits[16] = {1, 2, 4, 8, 16, 32, 64, 128, 0, 0, 0, 0, 0, 0, 0, 0};
        uint8x16_t bit = vld1q_u8(bits), low = vdupq_n_u8(0x0F);
        for (; k + 16 <= n; k += 16) {
            uint8x16_t x = vld1q_u8(t + k);
            uint8x16_t row = vqtbl1q_u8(map, vandq_u8(x, low));
            uint8x16_t b = vqtbl1q_u8(bit, vshrq_n_u8(x, 4));
            uint64_t m = neon_mask(vceqq_u8(vandq_u8(row, b), vdupq_n_u8(0)));
            if (m) return k + (CRE_CTZ64(m) >> 2);
        }
    }
#endif
    for (; k < n; k++) {
        unsigned char x = t[k];
        if (x >= 0x80 || !(nib[x & 15] & (1 << (x >> 4)))) return k;
    }
    return n;
}

value cre_ascii_run_end_byte(value s, value from, value n, value nibbles)
{
    return Val_long(cre_ascii_run_end(s, Long_val(from), Long_val(n), nibbles));
}

/* cre_ascii_run_end over UTF-16LE text from the byte base: the first unit at or after from and before n that is not an
   ASCII member of the set, or n. Sixteen units are packed into bytes with unsigned saturation (a unit at or beyond
   0x80 stays at or beyond 0x80, which ends the run) and tested as the bytes are. */
#if defined(CRE_HAS_TARGETS)
CRE_TARGET("ssse3")
static intnat ascii_run16_ssse3(bytes t, intnat k, intnat n, bytes nib)
{
    __m128i map = _mm_loadu_si128((const __m128i *)nib);
    __m128i bit = _mm_setr_epi8(1, 2, 4, 8, 16, 32, 64, (char)128, 0, 0, 0, 0, 0, 0, 0, 0);
    __m128i low = _mm_set1_epi8(0x0F), zero = _mm_setzero_si128();
    for (; k + 16 <= n; k += 16) {
        __m128i a = _mm_loadu_si128((const __m128i *)(t + 2 * k));
        __m128i b = _mm_loadu_si128((const __m128i *)(t + 2 * k + 16));
        /* packus saturates signed words: a unit from 0x80 to 0x7FFF packs to 0x80 or more, and one with its top bit
           set reads as negative and packs to 0, so it is marked 0xFF from its sign. */
        __m128i x = _mm_or_si128(_mm_packus_epi16(a, b), _mm_packs_epi16(_mm_srai_epi16(a, 15), _mm_srai_epi16(b, 15)));
        __m128i row = _mm_shuffle_epi8(map, _mm_and_si128(x, low));
        __m128i bb = _mm_shuffle_epi8(bit, _mm_and_si128(_mm_srli_epi16(x, 4), low));
        int m = _mm_movemask_epi8(_mm_cmpeq_epi8(_mm_and_si128(row, bb), zero));
        if (m) return k + CRE_CTZ32((uint32_t)m);
    }
    return k;
}
#endif

intnat cre_ascii_run_end16(value s, intnat base, intnat from, intnat n, value nibbles)
{
    bytes t = TEXT(s) + base, nib = TEXT(nibbles);
    intnat k = from;
#if defined(CRE_HAS_TARGETS)
    if (has_ssse3()) {
        k = ascii_run16_ssse3(t, k, n, nib);
        if (k + 16 <= n) return k;
    }
#elif defined(CRE_NEON)
    {
        uint8x16_t map = vld1q_u8(nib);
        static const uint8_t bits[16] = {1, 2, 4, 8, 16, 32, 64, 128, 0, 0, 0, 0, 0, 0, 0, 0};
        uint8x16_t bit = vld1q_u8(bits), low = vdupq_n_u8(0x0F);
        for (; k + 16 <= n; k += 16) {
            uint16x8_t a = vreinterpretq_u16_u8(vld1q_u8(t + 2 * k)), b = vreinterpretq_u16_u8(vld1q_u8(t + 2 * k + 16));
            uint8x16_t x = vcombine_u8(vqmovn_u16(a), vqmovn_u16(b));
            uint8x16_t row = vqtbl1q_u8(map, vandq_u8(x, low));
            uint8x16_t bb = vqtbl1q_u8(bit, vshrq_n_u8(x, 4));
            uint64_t m = neon_mask(vceqq_u8(vandq_u8(row, bb), vdupq_n_u8(0)));
            if (m) return k + (CRE_CTZ64(m) >> 2);
        }
    }
#endif
    for (; k < n; k++) {
        unsigned x = t[2 * k] | (t[2 * k + 1] << 8);
        if (x >= 0x80 || !(nib[x & 15] & (1 << (x >> 4)))) return k;
    }
    return n;
}

value cre_ascii_run_end16_byte(value s, value base, value from, value n, value nibbles)
{
    return Val_long(cre_ascii_run_end16(s, Long_val(base), Long_val(from), Long_val(n), nibbles));
}

/* The starts of the runs of len set bits (1 to 64) that lie wholly in the 64 bits of m: bit i of the result is set when
   bits i to i + len - 1 of m are. The runs are found by doubling: a run of 2k bits is a run of k bits whose bit k on is
   a run of k bits too. */
static inline uint64_t runs_in(uint64_t m, intnat len)
{
    uint64_t a = m;
    intnat have = 1;
    while (have * 2 <= len) {
        a &= a >> have;
        have *= 2;
    }
    if (len > have) a &= a >> (len - have);
    /* A start whose run would pass the top of the word is not one: the bits above it are not known. */
    if (len > 1) a &= ~0ULL >> (len - 1);
    return a;
}

/* The first position in [from, n) of a run of len bytes (1 to 64), each in one of nr ranges as cre_index_ranges takes
   them, or -1: the skip window's search, a start having a byte the window holds at each of its places. The vector forms
   read 64 bytes as a mask, one bit a byte, and find the runs in it (runs_in); the next block starts len - 1 bytes before
   the end of this one, so every run lies wholly in some block. */
intnat cre_index_run(value s, intnat from, intnat n, value ranges, intnat nr, intnat len)
{
    bytes t = TEXT(s), r = TEXT(ranges);
    intnat k = from;
#if defined(CRE_X86)
    __m128i lo[4], w[4];
    for (int q = 0; q < nr; q++) {
        lo[q] = _mm_set1_epi8((char)r[2 * q]);
        w[q] = _mm_set1_epi8((char)r[2 * q + 1]);
    }
    for (; k + 64 <= n; k += 64 - len + 1) {
        uint64_t m = 0;
        for (int b = 0; b < 4; b++) {
            __m128i x = _mm_loadu_si128((const __m128i *)(t + k + 16 * b)), any = _mm_setzero_si128();
            for (int q = 0; q < nr; q++) {
                __m128i dlt = _mm_sub_epi8(x, lo[q]);
                any = _mm_or_si128(any, _mm_cmpeq_epi8(_mm_min_epu8(dlt, w[q]), dlt));
            }
            m |= (uint64_t)(uint32_t)_mm_movemask_epi8(any) << (16 * b);
        }
        uint64_t a = runs_in(m, len);
        if (a) return k + CRE_CTZ64(a);
    }
#elif defined(CRE_NEON)
    uint8x16_t lo[4], w[4];
    for (int q = 0; q < nr; q++) {
        lo[q] = vdupq_n_u8(r[2 * q]);
        w[q] = vdupq_n_u8(r[2 * q + 1]);
    }
    static const uint8_t weights[16] = {1, 2, 4, 8, 16, 32, 64, 128, 1, 2, 4, 8, 16, 32, 64, 128};
    uint8x16_t wt = vld1q_u8(weights);
    for (; k + 64 <= n; k += 64 - len + 1) {
        uint64_t m = 0;
        for (int b = 0; b < 4; b++) {
            uint8x16_t x = vld1q_u8(t + k + 16 * b), any = vdupq_n_u8(0);
            for (int q = 0; q < nr; q++) any = vorrq_u8(any, vcleq_u8(vsubq_u8(x, lo[q]), w[q]));
            uint8x16_t bits = vandq_u8(any, wt);
            uint64_t half = (uint64_t)vaddv_u8(vget_low_u8(bits)) | ((uint64_t)vaddv_u8(vget_high_u8(bits)) << 8);
            m |= half << (16 * b);
        }
        uint64_t a = runs_in(m, len);
        if (a) return k + CRE_CTZ64(a);
    }
#endif
    intnat run = 0;
    for (; k < n; k++) {
        int hit = 0;
        for (int q = 0; q < nr; q++)
            if ((unsigned char)(t[k] - r[2 * q]) <= r[2 * q + 1]) hit = 1;
        if (hit) {
            if (++run >= len) return k - len + 1;
        } else {
            run = 0;
        }
    }
    return -1;
}

value cre_index_run_byte(value *argv, int argn)
{
    (void)argn;
    return Val_long(cre_index_run(argv[0], ARG(1), ARG(2), argv[3], ARG(4), ARG(5)));
}

/* The last position in [lo, hi) whose byte is one of the count (one to three) bytes of back, or -1: the give-back of
   a single character loop, memrchr of one to three bytes. */
intnat cre_last_of(value s, intnat lo, intnat hi, value back, intnat count)
{
    bytes t = TEXT(s), b = TEXT(back);
    unsigned char x = b[0], y = count > 1 ? b[1] : b[0], z = count > 2 ? b[2] : b[0];
    for (intnat k = hi - 1; k >= lo; k--)
        if (t[k] == x || t[k] == y || t[k] == z) return k;
    return -1;
}

value cre_last_of_byte(value s, value lo, value hi, value back, value count)
{
    return Val_long(cre_last_of(s, Long_val(lo), Long_val(hi), back, Long_val(count)));
}

/* ---------------------------------------------------------------------------------------------------------------- */
/* The pair search (PCRE2's fast_forward_char_pair_simd): two positions of the literals, each a few tests */

/* The first start in [at, last] whose byte at i passes one of ni tests and whose byte at j passes one of nj, or -1.
   tests holds each test as two bytes, a mask and a value, a byte x passing where (x | mask) == value: the ni tests of
   i, then the nj of j (each one to four). Every byte read is below n. */
intnat cre_pair_find(value s, intnat at, intnat last, intnat n, intnat i, intnat j, value tests, intnat ni, intnat nj)
{
    bytes t = TEXT(s), q = TEXT(tests);
    intnat reach = i > j ? i : j;
    intnat st = at;
#if defined(CRE_X86)
    __m128i mi[4], vi[4], mj[4], vj[4];
    for (int k = 0; k < ni; k++) { mi[k] = _mm_set1_epi8((char)q[2 * k]); vi[k] = _mm_set1_epi8((char)q[2 * k + 1]); }
    for (int k = 0; k < nj; k++) {
        mj[k] = _mm_set1_epi8((char)q[2 * (ni + k)]);
        vj[k] = _mm_set1_epi8((char)q[2 * (ni + k) + 1]);
    }
    for (; st <= last && st + reach + 16 <= n; st += 16) {
        __m128i xi = _mm_loadu_si128((const __m128i *)(t + st + i)), xj = _mm_loadu_si128((const __m128i *)(t + st + j));
        __m128i ai = _mm_setzero_si128(), aj = _mm_setzero_si128();
        for (int k = 0; k < ni; k++) ai = _mm_or_si128(ai, _mm_cmpeq_epi8(_mm_or_si128(xi, mi[k]), vi[k]));
        for (int k = 0; k < nj; k++) aj = _mm_or_si128(aj, _mm_cmpeq_epi8(_mm_or_si128(xj, mj[k]), vj[k]));
        int m = _mm_movemask_epi8(_mm_and_si128(ai, aj));
        if (m) {
            intnat hit = st + CRE_CTZ32((uint32_t)m);
            return hit <= last ? hit : -1;
        }
    }
#elif defined(CRE_NEON)
    uint8x16_t mi[4], vi[4], mj[4], vj[4];
    for (int k = 0; k < ni; k++) { mi[k] = vdupq_n_u8(q[2 * k]); vi[k] = vdupq_n_u8(q[2 * k + 1]); }
    for (int k = 0; k < nj; k++) { mj[k] = vdupq_n_u8(q[2 * (ni + k)]); vj[k] = vdupq_n_u8(q[2 * (ni + k) + 1]); }
    for (; st <= last && st + reach + 16 <= n; st += 16) {
        uint8x16_t xi = vld1q_u8(t + st + i), xj = vld1q_u8(t + st + j), ai = vdupq_n_u8(0), aj = vdupq_n_u8(0);
        for (int k = 0; k < ni; k++) ai = vorrq_u8(ai, vceqq_u8(vorrq_u8(xi, mi[k]), vi[k]));
        for (int k = 0; k < nj; k++) aj = vorrq_u8(aj, vceqq_u8(vorrq_u8(xj, mj[k]), vj[k]));
        uint64_t m = neon_mask(vandq_u8(ai, aj));
        if (m) {
            intnat hit = st + (CRE_CTZ64(m) >> 2);
            return hit <= last ? hit : -1;
        }
    }
#endif
    for (; st <= last && st + reach < n; st++) {
        unsigned char xi = t[st + i], xj = t[st + j];
        int pi = 0, pj = 0;
        for (int k = 0; k < ni; k++) pi |= (xi | q[2 * k]) == q[2 * k + 1];
        for (int k = 0; k < nj; k++) pj |= (xj | q[2 * (ni + k)]) == q[2 * (ni + k) + 1];
        if (pi && pj) return st;
    }
    return -1;
}

value cre_pair_find_byte(value *argv, int argn)
{
    (void)argn;
    return Val_long(cre_pair_find(argv[0], ARG(1), ARG(2), ARG(3), ARG(4), ARG(5), argv[6], ARG(7), ARG(8)));
}

/* The first start in [at, last] whose byte at i is a and whose byte at j is b, or -1 (memchr's search for the two
   rarest bytes of a needle, packedpair): 32 starts at a time with AVX2, 16 with SSE2 or NEON. */
#if defined(CRE_HAS_TARGETS)
CRE_TARGET("avx2")
static intnat eq2_avx2(bytes t, intnat st, intnat last, intnat n, intnat i, intnat j, unsigned char a, unsigned char b,
                       intnat *next)
{
    intnat reach = i > j ? i : j;
    __m256i va = _mm256_set1_epi8((char)a), vb = _mm256_set1_epi8((char)b);
    for (; st <= last && st + reach + 32 <= n; st += 32) {
        __m256i e = _mm256_and_si256(_mm256_cmpeq_epi8(_mm256_loadu_si256((const __m256i *)(t + st + i)), va),
                                     _mm256_cmpeq_epi8(_mm256_loadu_si256((const __m256i *)(t + st + j)), vb));
        uint32_t m = (uint32_t)_mm256_movemask_epi8(e);
        if (m) {
            intnat hit = st + CRE_CTZ32(m);
            *next = -1;
            return hit <= last ? hit : -1;
        }
    }
    *next = st;
    return -1;
}
#endif

intnat cre_eq2_find(value s, intnat at, intnat last, intnat n, intnat i, intnat j, intnat a, intnat b)
{
    bytes t = TEXT(s);
    intnat reach = i > j ? i : j;
    intnat st = at;
#if defined(CRE_HAS_TARGETS)
    if (has_avx2()) {
        intnat next;
        intnat hit = eq2_avx2(t, st, last, n, i, j, (unsigned char)a, (unsigned char)b, &next);
        if (next < 0) return hit;
        st = next;
    }
#endif
#if defined(CRE_X86)
    __m128i va = _mm_set1_epi8((char)a), vb = _mm_set1_epi8((char)b);
    for (; st <= last && st + reach + 16 <= n; st += 16) {
        __m128i e = _mm_and_si128(_mm_cmpeq_epi8(_mm_loadu_si128((const __m128i *)(t + st + i)), va),
                                  _mm_cmpeq_epi8(_mm_loadu_si128((const __m128i *)(t + st + j)), vb));
        int m = _mm_movemask_epi8(e);
        if (m) {
            intnat hit = st + CRE_CTZ32((uint32_t)m);
            return hit <= last ? hit : -1;
        }
    }
#elif defined(CRE_NEON)
    uint8x16_t va = vdupq_n_u8((uint8_t)a), vb = vdupq_n_u8((uint8_t)b);
    for (; st <= last && st + reach + 16 <= n; st += 16) {
        uint64_t m = neon_mask(vandq_u8(vceqq_u8(vld1q_u8(t + st + i), va), vceqq_u8(vld1q_u8(t + st + j), vb)));
        if (m) {
            intnat hit = st + (CRE_CTZ64(m) >> 2);
            return hit <= last ? hit : -1;
        }
    }
#endif
    for (; st <= last && st + reach < n; st++)
        if (t[st + i] == a && t[st + j] == b) return st;
    return -1;
}

value cre_eq2_find_byte(value *argv, int argn)
{
    (void)argn;
    return Val_long(cre_eq2_find(argv[0], ARG(1), ARG(2), ARG(3), ARG(4), ARG(5), ARG(6), ARG(7)));
}

/* ---------------------------------------------------------------------------------------------------------------- */
/* Teddy (Geoff Langdale's search for a few literals in Intel's Hyperscan, src/fdr/teddy.c in
   https://github.com/intel/hyperscan, which the Rust crate aho-corasick ports; the fullest description of it is the
   README of that port: https://github.com/BurntSushi/aho-corasick/blob/master/src/packed/teddy/README.md) */

/* The first start in [at, last] that passes the nibble tables of Teddy, or -1: for each of the positions 0, 1 and 2,
   the table of the low nibble of the byte at start + m and the table of its high nibble give a byte of buckets each,
   and a start passes where some bucket is in all six. tab holds 16 bytes of each table, low then high, for positions 0
   to 2: 96 bytes. The text is loaded at each position's offset, where Hyperscan and aho-corasick shift one load; the
   buckets' literals are compared by the caller. */
#if defined(CRE_HAS_TARGETS)
CRE_TARGET("ssse3")
static intnat teddy_ssse3(bytes t, intnat st, intnat last, intnat n, bytes tab, intnat *next)
{
    __m128i nib = _mm_set1_epi8(0x0F);
    __m128i l0 = _mm_loadu_si128((const __m128i *)(tab + 0)), h0 = _mm_loadu_si128((const __m128i *)(tab + 16));
    __m128i l1 = _mm_loadu_si128((const __m128i *)(tab + 32)), h1 = _mm_loadu_si128((const __m128i *)(tab + 48));
    __m128i l2 = _mm_loadu_si128((const __m128i *)(tab + 64)), h2 = _mm_loadu_si128((const __m128i *)(tab + 80));
    for (; st <= last && st + 2 + 16 <= n; st += 16) {
        __m128i x0 = _mm_loadu_si128((const __m128i *)(t + st));
        __m128i x1 = _mm_loadu_si128((const __m128i *)(t + st + 1));
        __m128i x2 = _mm_loadu_si128((const __m128i *)(t + st + 2));
        __m128i r = _mm_and_si128(_mm_shuffle_epi8(l0, _mm_and_si128(x0, nib)),
                                  _mm_shuffle_epi8(h0, _mm_and_si128(_mm_srli_epi16(x0, 4), nib)));
        r = _mm_and_si128(r, _mm_and_si128(_mm_shuffle_epi8(l1, _mm_and_si128(x1, nib)),
                                           _mm_shuffle_epi8(h1, _mm_and_si128(_mm_srli_epi16(x1, 4), nib))));
        r = _mm_and_si128(r, _mm_and_si128(_mm_shuffle_epi8(l2, _mm_and_si128(x2, nib)),
                                           _mm_shuffle_epi8(h2, _mm_and_si128(_mm_srli_epi16(x2, 4), nib))));
        int m = _mm_movemask_epi8(_mm_cmpeq_epi8(r, _mm_setzero_si128())) ^ 0xFFFF;
        if (m) {
            intnat hit = st + CRE_CTZ32((uint32_t)m);
            *next = -1;
            return hit <= last ? hit : -1;
        }
    }
    *next = st;
    return -1;
}
#endif

/* Whether the start st passes the tables, a byte at a time. A position at or past n is beyond the shortest literal, whose
   tables pass every byte, and is not read. */
static inline int teddy_passes(bytes t, intnat st, intnat n, bytes tab)
{
    unsigned char b = 0xFF;
    for (int m = 0; m < 3 && st + m < n; m++) {
        unsigned char x = t[st + m];
        b &= tab[32 * m + (x & 15)] & tab[32 * m + 16 + (x >> 4)];
    }
    return b != 0;
}

intnat cre_teddy_find(value s, intnat at, intnat last, intnat n, value table)
{
    bytes t = TEXT(s), tab = TEXT(table);
    intnat st = at;
#if defined(CRE_HAS_TARGETS)
    if (has_ssse3()) {
        intnat next;
        intnat hit = teddy_ssse3(t, st, last, n, tab, &next);
        if (next < 0) return hit;
        st = next;
    }
#elif defined(CRE_NEON)
    uint8x16_t nib = vdupq_n_u8(0x0F);
    uint8x16_t l0 = vld1q_u8(tab + 0), h0 = vld1q_u8(tab + 16), l1 = vld1q_u8(tab + 32), h1 = vld1q_u8(tab + 48);
    uint8x16_t l2 = vld1q_u8(tab + 64), h2 = vld1q_u8(tab + 80);
    for (; st <= last && st + 2 + 16 <= n; st += 16) {
        uint8x16_t x0 = vld1q_u8(t + st), x1 = vld1q_u8(t + st + 1), x2 = vld1q_u8(t + st + 2);
        uint8x16_t r = vandq_u8(vqtbl1q_u8(l0, vandq_u8(x0, nib)), vqtbl1q_u8(h0, vshrq_n_u8(x0, 4)));
        r = vandq_u8(r, vandq_u8(vqtbl1q_u8(l1, vandq_u8(x1, nib)), vqtbl1q_u8(h1, vshrq_n_u8(x1, 4))));
        r = vandq_u8(r, vandq_u8(vqtbl1q_u8(l2, vandq_u8(x2, nib)), vqtbl1q_u8(h2, vshrq_n_u8(x2, 4))));
        uint64_t m = neon_mask(vtstq_u8(r, r));
        if (m) {
            intnat hit = st + (CRE_CTZ64(m) >> 2);
            return hit <= last ? hit : -1;
        }
    }
#endif
    for (; st <= last && st < n; st++)
        if (teddy_passes(t, st, n, tab)) return st;
    return -1;
}

value cre_teddy_find_byte(value s, value at, value last, value n, value table)
{
    return Val_long(cre_teddy_find(s, Long_val(at), Long_val(last), Long_val(n), table));
}

/* ---------------------------------------------------------------------------------------------------------------- */
/* UTF-16 */

/* The first unit at or after from and before n (units from the byte base) of UTF-16LE text that is v, or -1. */
#if defined(CRE_HAS_TARGETS)
/* The first unit at or after k and before n that is v, thirty-two units a step, or the place the steps stopped at
   (*found 0). */
CRE_TARGET("avx2")
static intnat index_unit16_avx2(bytes t, intnat k, intnat n, intnat v, int *found)
{
    __m256i vv = _mm256_set1_epi16((short)v);
    for (; k + 32 <= n; k += 32) {
        __m256i a = _mm256_cmpeq_epi16(_mm256_loadu_si256((const __m256i *)(t + 2 * k)), vv);
        __m256i b = _mm256_cmpeq_epi16(_mm256_loadu_si256((const __m256i *)(t + 2 * k + 32)), vv);
        if (!_mm256_testz_si256(_mm256_or_si256(a, b), _mm256_or_si256(a, b))) {
            uint32_t ma = (uint32_t)_mm256_movemask_epi8(a);
            *found = 1;
            if (ma) return k + (CRE_CTZ32(ma) >> 1);
            return k + 16 + (CRE_CTZ32((uint32_t)_mm256_movemask_epi8(b)) >> 1);
        }
    }
    *found = 0;
    return k;
}
#endif

intnat cre_index_unit16(value s, intnat base, intnat from, intnat n, intnat v)
{
    bytes t = TEXT(s) + base;
    intnat k = from;
#if defined(CRE_HAS_TARGETS)
    if (has_avx2()) {
        int found;
        k = index_unit16_avx2(t, k, n, v, &found);
        if (found) return k;
    }
#endif
#if defined(CRE_X86)
    __m128i vv = _mm_set1_epi16((short)v);
    for (; k + 8 <= n; k += 8) {
        int m = _mm_movemask_epi8(_mm_cmpeq_epi16(_mm_loadu_si128((const __m128i *)(t + 2 * k)), vv));
        if (m) return k + (CRE_CTZ32((uint32_t)m) >> 1);
    }
#elif defined(CRE_NEON)
    uint16x8_t vv = vdupq_n_u16((uint16_t)v);
    for (; k + 8 <= n; k += 8) {
        uint16x8_t x = vreinterpretq_u16_u8(vld1q_u8(t + 2 * k));
        uint64_t m = neon_mask(vreinterpretq_u8_u16(vceqq_u16(x, vv)));
        if (m) return k + (CRE_CTZ64(m) >> 3);
    }
#endif
    for (; k < n; k++)
        if ((uint16_t)(t[2 * k] | (t[2 * k + 1] << 8)) == (uint16_t)v) return k;
    return -1;
}

value cre_index_unit16_byte(value s, value base, value from, value n, value v)
{
    return Val_long(cre_index_unit16(s, Long_val(base), Long_val(from), Long_val(n), Long_val(v)));
}

/* The first unit at or after from and before n of UTF-16LE text from the byte base that is in one of nr ranges (1 to
   8), given as 16-bit little-endian pairs of a least unit and a width (hi - lo), or -1. With AVX2 or NEON, sixteen or
   eight units a step: a unit x is in a range where x - lo, unsigned, is at most the width. */
#if defined(CRE_HAS_TARGETS)
CRE_TARGET("avx2")
static intnat index_units16_avx2(bytes t, intnat k, intnat n, bytes r, intnat nr, int *found)
{
    __m256i lo[8], w[8];
    for (int q = 0; q < nr; q++) {
        lo[q] = _mm256_set1_epi16((short)(r[4 * q] | (r[4 * q + 1] << 8)));
        w[q] = _mm256_set1_epi16((short)(r[4 * q + 2] | (r[4 * q + 3] << 8)));
    }
    for (; k + 16 <= n; k += 16) {
        __m256i x = _mm256_loadu_si256((const __m256i *)(t + 2 * k)), any = _mm256_setzero_si256();
        for (int q = 0; q < nr; q++) {
            __m256i d = _mm256_sub_epi16(x, lo[q]);
            any = _mm256_or_si256(any, _mm256_cmpeq_epi16(_mm256_min_epu16(d, w[q]), d));
        }
        uint32_t m = (uint32_t)_mm256_movemask_epi8(any);
        if (m) {
            *found = 1;
            return k + (CRE_CTZ32(m) >> 1);
        }
    }
    *found = 0;
    return k;
}
#endif

intnat cre_index_units16(value s, intnat base, intnat from, intnat n, value ranges, intnat nr)
{
    bytes t = TEXT(s) + base, r = TEXT(ranges);
    intnat k = from;
#if defined(CRE_HAS_TARGETS)
    if (has_avx2()) {
        int found;
        k = index_units16_avx2(t, k, n, r, nr, &found);
        if (found) return k;
    }
#elif defined(CRE_NEON)
    {
        uint16x8_t lo[8], w[8];
        for (int q = 0; q < nr; q++) {
            lo[q] = vdupq_n_u16((uint16_t)(r[4 * q] | (r[4 * q + 1] << 8)));
            w[q] = vdupq_n_u16((uint16_t)(r[4 * q + 2] | (r[4 * q + 3] << 8)));
        }
        for (; k + 8 <= n; k += 8) {
            uint16x8_t x = vreinterpretq_u16_u8(vld1q_u8(t + 2 * k)), any = vdupq_n_u16(0);
            for (int q = 0; q < nr; q++) any = vorrq_u16(any, vcleq_u16(vsubq_u16(x, lo[q]), w[q]));
            uint64_t m = neon_mask(vreinterpretq_u8_u16(any));
            if (m) return k + (CRE_CTZ64(m) >> 3);
        }
    }
#endif
    for (; k < n; k++) {
        uint16_t x = (uint16_t)(t[2 * k] | (t[2 * k + 1] << 8));
        for (int q = 0; q < nr; q++) {
            uint16_t l = (uint16_t)(r[4 * q] | (r[4 * q + 1] << 8)), wd = (uint16_t)(r[4 * q + 2] | (r[4 * q + 3] << 8));
            if ((uint16_t)(x - l) <= wd) return k;
        }
    }
    return -1;
}

value cre_index_units16_byte(value *argv, int argn)
{
    (void)argn;
    return Val_long(cre_index_units16(argv[0], ARG(1), ARG(2), ARG(3), argv[4], ARG(5)));
}

/* Writes the low byte of each of count units of UTF-16LE text from unit from (units from the byte base) into dst, the
   packed text the packed search reads. */
intnat cre_pack_low(value s, intnat base, intnat from, intnat count, value dst)
{
    bytes t = TEXT(s) + base + 2 * from;
    unsigned char *d = (unsigned char *)Bytes_val(dst);
    intnat k = 0;
#if defined(CRE_X86)
    __m128i lowmask = _mm_set1_epi16(0x00FF);
    for (; k + 16 <= count; k += 16) {
        __m128i a = _mm_and_si128(_mm_loadu_si128((const __m128i *)(t + 2 * k)), lowmask);
        __m128i b = _mm_and_si128(_mm_loadu_si128((const __m128i *)(t + 2 * k + 16)), lowmask);
        _mm_storeu_si128((__m128i *)(d + k), _mm_packus_epi16(a, b));
    }
#elif defined(CRE_NEON)
    for (; k + 16 <= count; k += 16) {
        uint8x16x2_t x = vld2q_u8(t + 2 * k);
        vst1q_u8(d + k, x.val[0]);
    }
#endif
    for (; k < count; k++) d[k] = t[2 * k];
    return 0;
}

value cre_pack_low_byte(value s, value base, value from, value count, value dst)
{
    return Val_long(cre_pack_low(s, Long_val(base), Long_val(from), Long_val(count), dst));
}
