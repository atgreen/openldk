/* SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0 */
/* The C ABI boundary. Objects and VM operations belong to the Lisp runtime. */
#include <jni.h>
#include <ffi.h>
#include <dlfcn.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

_Static_assert(sizeof(void *) == 8 && sizeof(jvalue) == 8, "JNI bridge requires a 64-bit host");
#if __BYTE_ORDER__ != __ORDER_LITTLE_ENDIAN__
#error JNI bridge currently requires a little-endian host
#endif

typedef uint64_t (*dispatcher)(const char *, jvalue *);
static dispatcher dispatch;
typedef struct { char *descriptor; } member;
static const struct JNINativeInterface_ native_table;
static _Thread_local JNIEnv thread_env = &native_table;
static _Thread_local int attached;
static _Thread_local int native_depth;

static const char *next_type(const char *s) {
    while (*s == '[') s++;
    if (*s == 'L') { while (*s && *s != ';') s++; }
    return *s ? s + 1 : s;
}

static void pack_va(jmethodID id, va_list ap, jvalue *out) {
    const char *s = ((member *)id)->descriptor + 1;
    for (int i = 0; *s && *s != ')'; i++, s = next_type(s)) {
        switch (*s) {
        case 'F': out[i].f = (jfloat)va_arg(ap, double); break;
        case 'D': out[i].d = va_arg(ap, double); break;
        case 'J': out[i].j = va_arg(ap, jlong); break;
        case 'L': case '[': out[i].l = va_arg(ap, jobject); break;
        default: out[i].i = va_arg(ap, int); break;
        }
    }
}

#include "table.inc"

static int valid_version(jint version) {
    return version == JNI_VERSION_1_1 || version == JNI_VERSION_1_2 ||
           version == JNI_VERSION_1_4 || version == JNI_VERSION_1_6 ||
           version == JNI_VERSION_1_8 || version == JNI_VERSION_9 ||
           version == JNI_VERSION_10 || version == JNI_VERSION_19 ||
           version == JNI_VERSION_20 || version == JNI_VERSION_21 || version == JNI_VERSION_24;
}

static jint JNICALL vm_attach(JavaVM *vm, void **env, void *args) {
    (void)vm;
    if (args && !valid_version(((JavaVMAttachArgs *)args)->version)) return JNI_EVERSION;
    jvalue a[3] = {{0}};
    if (!attached) {
        a[0].j = (intptr_t)&thread_env; a[1].j = (intptr_t)args;
        if ((jint)dispatch("AttachCurrentThread", a) != JNI_OK) return JNI_ERR;
        attached = 1;
    }
    *env = &thread_env;
    return JNI_OK;
}
static jint JNICALL vm_attach_daemon(JavaVM *vm, void **env, void *args) {
    (void)vm;
    if (args && !valid_version(((JavaVMAttachArgs *)args)->version)) return JNI_EVERSION;
    jvalue a[3] = {{0}};
    if (!attached) {
        a[0].j = (intptr_t)&thread_env; a[1].j = (intptr_t)args; a[2].i = 1;
        if ((jint)dispatch("AttachCurrentThread", a) != JNI_OK) return JNI_ERR;
        attached = 1;
    }
    *env = &thread_env;
    return JNI_OK;
}
static jint JNICALL vm_detach(JavaVM *vm) {
    (void)vm;
    if (native_depth) return JNI_ERR;
    if (attached) {
        jvalue a[1] = {{0}}; a[0].j = (intptr_t)&thread_env;
        dispatch("DetachCurrentThread", a); attached = 0;
    }
    return JNI_OK;
}
static jint JNICALL vm_getenv(JavaVM *vm, void **env, jint version) {
    (void)vm; *env = NULL;
    if (!valid_version(version)) return JNI_EVERSION;
    if (!attached) return JNI_EDETACHED;
    *env = &thread_env; return JNI_OK;
}
static jint JNICALL vm_destroy(JavaVM *vm) { (void)vm; return JNI_ERR; }
static const struct JNIInvokeInterface_ vm_table = {
    .DestroyJavaVM = vm_destroy, .AttachCurrentThread = vm_attach,
    .DetachCurrentThread = vm_detach, .GetEnv = vm_getenv,
    .AttachCurrentThreadAsDaemon = vm_attach_daemon
};
static JavaVM java_vm = &vm_table;

JNIEXPORT jint JNICALL JNI_GetCreatedJavaVMs(JavaVM **buffer, jsize length, jsize *count) {
    if (!dispatch) { *count = 0; return JNI_OK; }
    *count = 1;
    if (length > 0) buffer[0] = &java_vm;
    return JNI_OK;
}

void ldk_jni_init(dispatcher callback) { dispatch = callback; }
void *ldk_jni_env(void) { attached = 1; return &thread_env; }
void *ldk_jni_vm(void) { return &java_vm; }
void *ldk_jni_member(const char *signature) {
    member *m = malloc(sizeof(*m));
    if (!m) return NULL;
    m->descriptor = strdup(signature);
    if (!m->descriptor) { free(m); return NULL; }
    return m;
}
void *ldk_jni_open(const char *path) { return dlopen(path, RTLD_NOW | RTLD_LOCAL); }
const char *ldk_jni_error(void) { const char *e = dlerror(); return e ? e : "native library error"; }
void *ldk_jni_symbol(void *library, const char *name) { return dlsym(library, name); }
void ldk_jni_close(void *library) { dlclose(library); }
int ldk_jni_onload(void *function) {
    attached = 1; native_depth++;
    jint version = ((jint (JNICALL *)(JavaVM *, void *))function)(&java_vm, NULL);
    native_depth--;
    return valid_version(version) ? version : 0;
}

static ffi_type *ffi_java_type(char ch) {
    switch (ch) {
    case 'V': return &ffi_type_void;
    case 'Z': return &ffi_type_uint8;
    case 'B': return &ffi_type_sint8;
    case 'C': return &ffi_type_uint16;
    case 'S': return &ffi_type_sint16;
    case 'I': return &ffi_type_sint32;
    case 'J': return &ffi_type_sint64;
    case 'F': return &ffi_type_float;
    case 'D': return &ffi_type_double;
    default: return &ffi_type_pointer;
    }
}

int ldk_jni_invoke(void *function, const char *signature, uintptr_t receiver,
                   jvalue *args, jvalue *result) {
    ffi_cif cif;
    ffi_type *types[258]; void *values[258];
    JNIEnv *env = ldk_jni_env();
    void *target = (void *)receiver;
    types[0] = types[1] = &ffi_type_pointer;
    values[0] = &env; values[1] = &target;
    int n = 2;
    const char *s = signature + 1;
    while (*s && *s != ')' && n < 258) {
        types[n] = ffi_java_type(*s); values[n] = &args[n - 2];
        n++; s = next_type(s);
    }
    if (*s != ')') return -1;
    ffi_type *ret = ffi_java_type(s[1]);
    if (ffi_prep_cif(&cif, FFI_DEFAULT_ABI, n, ret, types) != FFI_OK) return -1;
    memset(result, 0, sizeof(*result));
    native_depth++;
    ffi_call(&cif, FFI_FN(function), result, values);
    native_depth--;
    return 0;
}
