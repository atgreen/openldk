/* SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0 */
#include <stdint.h>
#include <stdbool.h>
#include <errno.h>
#include <pthread.h>

int32_t ffi_add(int32_t a, int32_t b) { return a + b; }
int64_t ffi_long(int64_t a) { return a ^ INT64_C(0x1234567812345678); }
int8_t ffi_byte(int8_t a) { return (int8_t)(a - 1); }
int16_t ffi_short(int16_t a) { return (int16_t)(a - 2); }
uint16_t ffi_char(uint16_t a) { return (uint16_t)(a + 3); }
bool ffi_bool(bool a) { return !a; }
float ffi_float(float a) { return a * 1.5f; }
double ffi_double(double a, double b) { return a * b; }
int ffi_byte_callback(int8_t (*fn)(int8_t), int8_t value) { return fn(value); }
double ffi_float_callback(double (*fn)(float, double), float a, double b) { return fn(a, b); }
int ffi_alias(int *a, int *b) { a[0] = 41; return b[0] + 1; }
int ffi_errno(int value) { errno = value; return -1; }
struct pair { int32_t a; double b; };
struct pair ffi_pair(struct pair p, double x) { p.a += 5; p.b *= x; return p; }
typedef int64_t (*callback)(int64_t, int64_t);
struct request { callback fn; int64_t value; int64_t result; };
static void *run_callback(void *ptr) {
    struct request *r = ptr;
    r->result = r->fn(r->value, 7);
    return NULL;
}
int64_t ffi_thread_callback(callback fn, int64_t value) {
    struct request r = {fn, value, 0};
    pthread_t thread;
    if (pthread_create(&thread, NULL, run_callback, &r) != 0) return -1;
    pthread_join(thread, NULL);
    return r.result;
}
