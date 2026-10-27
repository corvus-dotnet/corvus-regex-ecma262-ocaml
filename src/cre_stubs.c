/* The C parts of corvus-regex-ecma262: the monotonic clock of the match timeout, and (with the search layer) the
   search kernels. Every function here is called from OCaml as an external that allocates nothing. */

#define CAML_NAME_SPACE
#include <caml/mlvalues.h>
#include <stdint.h>

#ifdef _WIN32
#include <windows.h>
#else
#include <time.h>
#endif

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

value cre_now_ns_byte(value unit)
{
    return Val_long(cre_now_ns(unit));
}
