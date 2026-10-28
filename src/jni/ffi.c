/* SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0 */
/* OpenLDK's libffi boundary for the JDK's Java fallback linker. */
#include <jni.h>
#include <ffi.h>
#include <dlfcn.h>
#include <stdint.h>
#include <stdlib.h>
#include <errno.h>
#include <wchar.h>

#define FFM(name) Java_jdk_internal_foreign_abi_fallback_LibFallback_##name
#define CONSTANT(name, type, value) \
    JNIEXPORT type JNICALL FFM(name)(JNIEnv *env, jclass cls) { \
        (void)env; (void)cls; return (type)(value); \
    }
CONSTANT(init, jboolean, JNI_TRUE)
CONSTANT(sizeofCif, jlong, sizeof(ffi_cif))
CONSTANT(ffi_1default_1abi, jint, FFI_DEFAULT_ABI)
CONSTANT(ffi_1type_1struct, jshort, FFI_TYPE_STRUCT)
CONSTANT(ffi_1sizeof_1short, jint, sizeof(short))
CONSTANT(ffi_1sizeof_1int, jint, sizeof(int))
CONSTANT(ffi_1sizeof_1long, jint, sizeof(long))
CONSTANT(ffi_1sizeof_1wchar, jint, sizeof(wchar_t))
CONSTANT(alignof_1long_1long, jint, _Alignof(long long))
CONSTANT(alignof_1double, jint, _Alignof(double))
#define TYPE(name) CONSTANT(ffi_1type_1##name, jlong, (uintptr_t)&ffi_type_##name)
TYPE(void)
TYPE(uint8)
TYPE(sint8)
TYPE(uint16)
TYPE(sint16)
TYPE(uint32)
TYPE(sint32)
TYPE(uint64)
TYPE(sint64)
TYPE(float)
TYPE(double)
TYPE(pointer)

void *ldk_ffi_default_symbol(const char *name) { return dlsym(RTLD_DEFAULT, name); }

JNIEXPORT jint JNICALL FFM(ffi_1prep_1cif)(JNIEnv *env, jclass cls, jlong cif,
        jint abi, jint nargs, jlong result, jlong args) {
    (void)env; (void)cls;
    return ffi_prep_cif((ffi_cif *)(uintptr_t)cif, (ffi_abi)abi, (unsigned)nargs,
                        (ffi_type *)(uintptr_t)result, (ffi_type **)(uintptr_t)args);
}
JNIEXPORT jint JNICALL FFM(ffi_1prep_1cif_1var)(JNIEnv *env, jclass cls, jlong cif,
        jint abi, jint fixed, jint total, jlong result, jlong args) {
    (void)env; (void)cls;
    return ffi_prep_cif_var((ffi_cif *)(uintptr_t)cif, (ffi_abi)abi,
                            (unsigned)fixed, (unsigned)total,
                            (ffi_type *)(uintptr_t)result, (ffi_type **)(uintptr_t)args);
}
JNIEXPORT jint JNICALL FFM(ffi_1get_1struct_1offsets)(JNIEnv *env, jclass cls,
        jint abi, jlong type, jlong offsets) {
    (void)env; (void)cls;
    return ffi_get_struct_offsets((ffi_abi)abi, (ffi_type *)(uintptr_t)type,
                                  (size_t *)(uintptr_t)offsets);
}

JNIEXPORT void JNICALL FFM(doDowncall)(JNIEnv *env, jclass cls, jlong cif_addr,
        jlong fn, jlong result, jlong args_addr, jobject capture_base, jlong capture,
        jint capture_mask, jobjectArray heap_bases, jint num_args) {
    (void)cls; (void)num_args;
    ffi_cif *cif = (ffi_cif *)(uintptr_t)cif_addr;
    void **args = (void **)(uintptr_t)args_addr;
    /* JNI critical arrays are copies in OpenLDK. Share one copy for aliases. */
    jobject *bases = calloc(cif->nargs + 1, sizeof(*bases));
    void **copies = calloc(cif->nargs + 1, sizeof(*copies));
    if (!bases || !copies) {
        free(bases); free(copies);
        (*env)->ThrowNew(env, (*env)->FindClass(env, "java/lang/OutOfMemoryError"), "FFM argument buffers");
        return;
    }
    unsigned count = 0;
    if (heap_bases) for (unsigned i = 0; i < cif->nargs; i++) {
        jobject base = (*env)->GetObjectArrayElement(env, heap_bases, (jsize)i);
        if (!base) continue;
        unsigned j = 0;
        while (j < count && !(*env)->IsSameObject(env, bases[j], base)) j++;
        if (j == count) {
            bases[j] = base;
            copies[j] = (*env)->GetPrimitiveArrayCritical(env, base, NULL);
            count++;
            if (!copies[j]) goto cleanup;
        } else (*env)->DeleteLocalRef(env, base);
        *(uintptr_t *)args[i] += (uintptr_t)copies[j];
    }
    void *capture_ptr = (void *)(uintptr_t)capture;
    if (capture_base) {
        unsigned j = 0;
        while (j < count && !(*env)->IsSameObject(env, bases[j], capture_base)) j++;
        if (j == count) {
            bases[j] = capture_base;
            copies[j] = (*env)->GetPrimitiveArrayCritical(env, capture_base, NULL);
            count++;
            if (!copies[j]) goto cleanup;
        }
        capture_ptr = (char *)copies[j] + capture;
    }
    ffi_call(cif, FFI_FN((uintptr_t)fn), (void *)(uintptr_t)result, args);
    int saved_errno = errno;
    if (capture_mask & 4) *(int *)capture_ptr = saved_errno;
cleanup:
    for (unsigned j = 0; j < count; j++) if (copies[j])
        (*env)->ReleasePrimitiveArrayCritical(env, bases[j], copies[j], 0);
    free(bases); free(copies);
}

typedef struct {
    JavaVM *vm;
    jobject target;
    jclass klass;
    jmethodID method;
} upcall_context;

static void upcall(ffi_cif *cif, void *result, void **args, void *data) {
    (void)cif;
    upcall_context *context = data;
    JNIEnv *env = NULL;
    jint status = (*context->vm)->GetEnv(context->vm, (void **)&env, JNI_VERSION_1_8);
    int detach = status == JNI_EDETACHED;
    if (detach && (*context->vm)->AttachCurrentThread(context->vm, (void **)&env, NULL) != JNI_OK) abort();
    if (!env) abort();
    jvalue values[3] = {{.j = (jlong)(uintptr_t)result}, {.j = (jlong)(uintptr_t)args}, {.l = context->target}};
    (*env)->CallStaticVoidMethodA(env, context->klass, context->method, values);
    if ((*env)->ExceptionCheck(env)) (*env)->FatalError(env, "Uncaught exception in FFM upcall");
    if (detach) (*context->vm)->DetachCurrentThread(context->vm);
}

JNIEXPORT jint JNICALL FFM(createClosure)(JNIEnv *env, jclass cls, jlong cif,
                                         jobject target, jlongArray pointers) {
    upcall_context *context = calloc(1, sizeof(*context));
    if (!context) return FFI_BAD_TYPEDEF;
    void *executable = NULL;
    ffi_closure *closure = ffi_closure_alloc(sizeof(*closure), &executable);
    if (!closure) { free(context); return FFI_BAD_TYPEDEF; }
    (*env)->GetJavaVM(env, &context->vm);
    context->target = (*env)->NewGlobalRef(env, target);
    context->klass = (*env)->NewGlobalRef(env, cls);
    context->method = (*env)->GetStaticMethodID(env, cls, "doUpcall", "(JJLjava/lang/invoke/MethodHandle;)V");
    ffi_status status = FFI_BAD_TYPEDEF;
    if (context->target && context->klass && context->method)
        status = ffi_prep_closure_loc(closure, (ffi_cif *)(uintptr_t)cif, upcall, context, executable);
    if (status != FFI_OK) {
        if (context->target) (*env)->DeleteGlobalRef(env, context->target);
        if (context->klass) (*env)->DeleteGlobalRef(env, context->klass);
        ffi_closure_free(closure); free(context); return status;
    }
    jlong out[3] = {(jlong)(uintptr_t)closure, (jlong)(uintptr_t)executable, (jlong)(uintptr_t)context};
    (*env)->SetLongArrayRegion(env, pointers, 0, 3, out);
    return FFI_OK;
}
JNIEXPORT void JNICALL FFM(freeClosure)(JNIEnv *env, jclass cls, jlong closure, jlong data) {
    (void)cls;
    upcall_context *context = (upcall_context *)(uintptr_t)data;
    (*env)->DeleteGlobalRef(env, context->target);
    (*env)->DeleteGlobalRef(env, context->klass);
    ffi_closure_free((void *)(uintptr_t)closure);
    free(context);
}
