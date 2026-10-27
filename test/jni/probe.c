/* SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0 */
#define _POSIX_C_SOURCE 200809L
#include <jni.h>
#include <pthread.h>
#include <stdatomic.h>
#include <sched.h>
#include <time.h>
#include <string.h>
#include <stdarg.h>

static JavaVM *vm;
static jclass probe_class;
static jobject remembered;
static void *thread_main(void *out);
static jint registered(JNIEnv *env, jclass cls, jint n) { (void)env; (void)cls; return n * 3; }

JNIEXPORT jint JNICALL JNI_OnLoad(JavaVM *machine, void *reserved) {
    (void)reserved; vm = machine;
    JNIEnv *env;
    if ((*vm)->GetEnv(vm, (void **)&env, JNI_VERSION_1_8) != JNI_OK) return JNI_ERR;
    jclass cls = (*env)->FindClass(env, "JNIProbe");
    if (!cls) return JNI_ERR;
    probe_class = (*env)->NewGlobalRef(env, cls);
    JNINativeMethod entry = {"registered", "(I)I", (void *)registered};
    if ((*env)->RegisterNatives(env, cls, &entry, 1)) return JNI_ERR;
    pthread_t thread; jint value = 0;
    if (pthread_create(&thread, NULL, thread_main, &value)) return JNI_ERR;
    pthread_join(thread, NULL);
    if (value != 28) return JNI_ERR;
    return JNI_VERSION_1_8;
}

#define ECHO(Type, type) \
JNIEXPORT type JNICALL Java_JNIProbe_echo##Type(JNIEnv *env, jclass cls, type n) { \
    (void)env; (void)cls; return n; }
ECHO(Boolean, jboolean)
ECHO(Byte, jbyte)
ECHO(Char, jchar)
ECHO(Short, jshort)
ECHO(Int, jint)
ECHO(Long, jlong)
ECHO(Float, jfloat)
ECHO(Double, jdouble)
ECHO(Object, jobject)
JNIEXPORT jint JNICALL Java_JNIProbe_overloaded__I(JNIEnv *env, jclass cls, jint n) { (void)env; (void)cls; return n + 1; }
JNIEXPORT jlong JNICALL Java_JNIProbe_overloaded__J(JNIEnv *env, jclass cls, jlong n) { (void)env; (void)cls; return n + 2; }

JNIEXPORT jstring JNICALL Java_JNIProbe_strings(JNIEnv *env, jclass cls, jstring input) {
    (void)cls;
    jboolean copied;
    const char *utf = (*env)->GetStringUTFChars(env, input, &copied);
    jstring result = (*env)->NewStringUTF(env, utf);
    (*env)->ReleaseStringUTFChars(env, input, utf);
    const jchar *chars = (*env)->GetStringChars(env, result, &copied);
    jstring final = (*env)->NewString(env, chars, (*env)->GetStringLength(env, result));
    (*env)->ReleaseStringChars(env, result, chars);
    return final;
}

JNIEXPORT jintArray JNICALL Java_JNIProbe_arrays(JNIEnv *env, jclass cls, jintArray input) {
    (void)cls;
    jint *values = (*env)->GetIntArrayElements(env, input, NULL);
    values[0] = 11;
    (*env)->ReleaseIntArrayElements(env, input, values, JNI_COMMIT);
    values[0] = 999;
    (*env)->ReleaseIntArrayElements(env, input, values, JNI_ABORT);
    jint change = 22;
    (*env)->SetIntArrayRegion(env, input, 1, 1, &change);
    jintArray result = (*env)->NewIntArray(env, 2);
    jint output[] = {-7, 42};
    (*env)->SetIntArrayRegion(env, result, 0, 2, output);
    return result;
}

static jdouble call_v(JNIEnv *env, jclass cls, jmethodID id, ...) {
    va_list ap; va_start(ap, id);
    jdouble result = (*env)->CallStaticDoubleMethodV(env, cls, id, ap);
    va_end(ap); return result;
}
JNIEXPORT jint JNICALL Java_JNIProbe_callbacks(JNIEnv *env, jclass cls) {
    (*env)->PushLocalFrame(env, 16);
    jmethodID init = (*env)->GetMethodID(env, cls, "<init>", "(I)V");
    jobject object = (*env)->NewObject(env, cls, init, 20);
    jmethodID plus = (*env)->GetMethodID(env, cls, "plus", "(I)I");
    jint a = (*env)->CallIntMethod(env, object, plus, 1);
    jvalue args[3] = {{.i = 2}, {.f = 3.5f}, {.d = 4.5}};
    jmethodID sum = (*env)->GetStaticMethodID(env, cls, "sum", "(IFD)D");
    jdouble b = (*env)->CallStaticDoubleMethodA(env, cls, sum, args);
    jdouble c = call_v(env, cls, sum, 1, (double)2.5f, 3.5);
    jint d = (*env)->CallNonvirtualIntMethod(env, object, cls, plus, -10);
    (*env)->PopLocalFrame(env, NULL);
    return a + (jint)b + (jint)c + d;
}
JNIEXPORT jint JNICALL Java_JNIProbe_fields(JNIEnv *env, jobject self, jint n) {
    jclass cls = (*env)->GetObjectClass(env, self);
    (*env)->MonitorEnter(env, self);
    jfieldID value = (*env)->GetFieldID(env, cls, "value", "I");
    jint result = (*env)->GetIntField(env, self, value) + n;
    (*env)->SetIntField(env, self, value, result);
    jfieldID total = (*env)->GetStaticFieldID(env, cls, "total", "J");
    (*env)->SetStaticLongField(env, cls, total, (jlong)99);
    (*env)->MonitorExit(env, self);
    return result;
}
JNIEXPORT void JNICALL Java_JNIProbe_raise(JNIEnv *env, jclass cls) {
    (void)cls;
    (*env)->ThrowNew(env, (*env)->FindClass(env, "java/lang/IllegalArgumentException"), "from JNI");
}
JNIEXPORT void JNICALL Java_JNIProbe_propagate(JNIEnv *env, jclass cls) {
    jmethodID id = (*env)->GetStaticMethodID(env, cls, "fail", "()V");
    (*env)->CallStaticVoidMethod(env, cls, id);
}
JNIEXPORT jboolean JNICALL Java_JNIProbe_clearException(JNIEnv *env, jclass cls) {
    Java_JNIProbe_propagate(env, cls);
    jboolean pending = (*env)->ExceptionCheck(env);
    jthrowable exception = (*env)->ExceptionOccurred(env);
    (*env)->ExceptionClear(env);
    return pending && exception != NULL && !(*env)->ExceptionCheck(env);
}
JNIEXPORT void JNICALL Java_JNIProbe_remember(JNIEnv *env, jclass cls, jobject object) {
    (void)cls; remembered = (*env)->NewGlobalRef(env, object);
}
JNIEXPORT jobject JNICALL Java_JNIProbe_recall(JNIEnv *env, jclass cls) {
    (void)cls; return (*env)->NewLocalRef(env, remembered);
}
JNIEXPORT void JNICALL Java_JNIProbe_forget(JNIEnv *env, jclass cls) {
    (void)cls; (*env)->DeleteGlobalRef(env, remembered); remembered = NULL;
}
static void *thread_main(void *out) {
    JNIEnv *env;
    if ((*vm)->GetEnv(vm, (void **)&env, JNI_VERSION_1_8) != JNI_EDETACHED) return NULL;
    if ((*vm)->AttachCurrentThread(vm, (void **)&env, NULL) != JNI_OK) return NULL;
    if ((*env)->MonitorEnter(env, probe_class) != 0) return NULL;
    jmethodID method = (*env)->GetStaticMethodID(env, probe_class, "registered", "(I)I");
    *(jint *)out = (*env)->CallStaticIntMethod(env, probe_class, method, 9) + 1;
    method = (*env)->GetStaticMethodID(env, probe_class, "attachedIdentity", "()I");
    if ((*env)->CallStaticIntMethod(env, probe_class, method) != 28) *(jint *)out = -1;
    if ((*env)->MonitorExit(env, probe_class) != 0) *(jint *)out = -1;
    (*vm)->DetachCurrentThread(vm);
    if ((*vm)->GetEnv(vm, (void **)&env, JNI_VERSION_1_8) != JNI_EDETACHED) { *(jint *)out = -1; return NULL; }
    JavaVMAttachArgs options = {JNI_VERSION_1_8, "JNI daemon", NULL};
    if ((*vm)->AttachCurrentThreadAsDaemon(vm, (void **)&env, &options) != JNI_OK) { *(jint *)out = -1; return NULL; }
    if ((*env)->MonitorEnter(env, probe_class) != 0) { *(jint *)out = -1; return NULL; }
    method = (*env)->GetStaticMethodID(env, probe_class, "daemonIdentity", "()I");
    if ((*env)->CallStaticIntMethod(env, probe_class, method) != 14) *(jint *)out = -1;
    // Detachment must release JNI-owned monitors even without MonitorExit.
    (*vm)->DetachCurrentThread(vm);
    return NULL;
}
JNIEXPORT jint JNICALL Java_JNIProbe_attachedThread(JNIEnv *env, jclass cls) {
    (void)env; (void)cls;
    pthread_t thread; jint result = -1;
    if (pthread_create(&thread, NULL, thread_main, &result)) return -1;
    pthread_join(thread, NULL);
    return result;
}

JNIEXPORT jint JNICALL Java_JNIProbe_caf_000e9(JNIEnv *env, jclass cls) { (void)env; (void)cls; return 31; }
JNIEXPORT jint JNICALL Java_JNIProbe__0d801_0dc00(JNIEnv *env, jclass cls) { (void)env; (void)cls; return 32; }

#define ARRAY_TEST(Name, type, first, second) do { \
    type values[2] = {first, second}, output[2] = {0}; \
    type##Array array = (*env)->New##Name##Array(env, 2); \
    (*env)->Set##Name##ArrayRegion(env, array, 0, 2, values); \
    type *elements = (*env)->Get##Name##ArrayElements(env, array, NULL); \
    if (!elements || elements[0] != values[0] || elements[1] != values[1]) return JNI_FALSE; \
    elements[0] = second; \
    (*env)->Release##Name##ArrayElements(env, array, elements, 0); \
    (*env)->Get##Name##ArrayRegion(env, array, 0, 2, output); \
    if (output[0] != second || output[1] != second) return JNI_FALSE; \
    elements = (*env)->GetPrimitiveArrayCritical(env, array, NULL); \
    if (!elements || elements[0] != second) return JNI_FALSE; \
    (*env)->ReleasePrimitiveArrayCritical(env, array, elements, JNI_ABORT); \
} while (0)
JNIEXPORT jboolean JNICALL Java_JNIProbe_advanced(JNIEnv *env, jclass cls) {
    (void)cls;
    ARRAY_TEST(Boolean, jboolean, 0, 1);
    ARRAY_TEST(Byte, jbyte, -117, 99);
    ARRAY_TEST(Char, jchar, 65534, 60000);
    ARRAY_TEST(Short, jshort, -30000, 29000);
    ARRAY_TEST(Int, jint, -2000000000, 1000000000);
    ARRAY_TEST(Long, jlong, -9000000000000000000LL, 8000000000000000000LL);
    ARRAY_TEST(Float, jfloat, -1.25f, 3.5f);
    ARRAY_TEST(Double, jdouble, -1.25, 3.5);
    jclass integer = (*env)->FindClass(env, "java/lang/Integer");
    jfieldID type = (*env)->GetStaticFieldID(env, integer, "TYPE", "Ljava/lang/Class;");
    jclass primitive = (*env)->GetStaticObjectField(env, integer, type);
    if ((*env)->GetSuperclass(env, primitive) != NULL || (*env)->ExceptionCheck(env)) return JNI_FALSE;
    jclass array_class = (*env)->GetObjectClass(env, (*env)->NewIntArray(env, 1));
    jclass object_class = (*env)->FindClass(env, "java/lang/Object");
    if (!(*env)->IsSameObject(env, object_class, (*env)->GetSuperclass(env, array_class))) return JNI_FALSE;
    jclass list = (*env)->FindClass(env, "java/util/List");
    if (!(*env)->GetMethodID(env, list, "iterator", "()Ljava/util/Iterator;")) return JNI_FALSE;
    if ((*env)->GetSuperclass(env, list) != NULL || (*env)->ExceptionCheck(env)) return JNI_FALSE;
    if ((*env)->EnsureLocalCapacity(env, 64) != 0) return JNI_FALSE;
    (*env)->PushLocalFrame(env, 4);
    jstring string = (*env)->NewStringUTF(env, "frame result");
    jobject weak = (*env)->NewWeakGlobalRef(env, string);
    if ((*env)->GetObjectRefType(env, weak) != JNIWeakGlobalRefType) return JNI_FALSE;
    string = (*env)->PopLocalFrame(env, string);
    if (!(*env)->IsSameObject(env, weak, string)) return JNI_FALSE;
    (*env)->DeleteWeakGlobalRef(env, weak);
    if ((*env)->GetObjectRefType(env, string) != JNILocalRefType) return JNI_FALSE;
    jclass string_class = (*env)->FindClass(env, "java/lang/String");
    jobjectArray objects = (*env)->NewObjectArray(env, 2, string_class, NULL);
    (*env)->SetObjectArrayElement(env, objects, 1, string);
    if (!(*env)->IsSameObject(env, (*env)->GetObjectArrayElement(env, objects, 1), string)) return JNI_FALSE;
    (*env)->GetObjectArrayElement(env, objects, -1);
    if (!(*env)->ExceptionCheck(env)) return JNI_FALSE;
    (*env)->ExceptionClear(env);
    (*env)->DeleteLocalRef(env, string);
    // An unpopped frame must still be released when this native method returns.
    (*env)->PushLocalFrame(env, 4);
    (*env)->NewStringUTF(env, "automatic frame cleanup");
    return !(*env)->ExceptionCheck(env);
}

static unsigned char direct_memory[16];
JNIEXPORT jobject JNICALL Java_JNIProbe_direct(JNIEnv *env, jclass cls) {
    (void)cls; return (*env)->NewDirectByteBuffer(env, direct_memory, sizeof(direct_memory));
}
JNIEXPORT jint JNICALL Java_JNIProbe_directCheck(JNIEnv *env, jclass cls, jobject buffer) {
    (void)cls;
    if ((*env)->GetDirectBufferCapacity(env, buffer) != 16) return -1;
    unsigned char *address = (*env)->GetDirectBufferAddress(env, buffer);
    return address ? address[2] : -1;
}
JNIEXPORT jboolean JNICALL Java_JNIProbe_reflection(JNIEnv *env, jclass cls, jobject method, jobject field) {
    jmethodID mid = (*env)->FromReflectedMethod(env, method);
    jfieldID fid = (*env)->FromReflectedField(env, field);
    if (mid != (*env)->GetStaticMethodID(env, cls, "sum", "(IFD)D")) return JNI_FALSE;
    if (fid != (*env)->GetFieldID(env, cls, "value", "I")) return JNI_FALSE;
    jobject reflected_method = (*env)->ToReflectedMethod(env, cls, mid, JNI_TRUE);
    jobject reflected_field = (*env)->ToReflectedField(env, cls, fid, JNI_FALSE);
    return mid == (*env)->FromReflectedMethod(env, reflected_method) &&
           fid == (*env)->FromReflectedField(env, reflected_field);
}
JNIEXPORT jboolean JNICALL Java_JNIProbe_synchronizedCall(JNIEnv *env, jclass cls) {
    jclass thread = (*env)->FindClass(env, "java/lang/Thread");
    jmethodID method = (*env)->GetStaticMethodID(env, thread, "holdsLock", "(Ljava/lang/Object;)Z");
    return (*env)->CallStaticBooleanMethod(env, thread, method, cls);
}
JNIEXPORT jclass JNICALL Java_JNIProbe_define(JNIEnv *env, jclass cls, jbyteArray bytes, jobject loader) {
    (void)cls;
    jbyte *data = (*env)->GetByteArrayElements(env, bytes, NULL);
    jclass result = (*env)->DefineClass(env, "JNIDefined", loader, data, (*env)->GetArrayLength(env, bytes));
    (*env)->ReleaseByteArrayElements(env, bytes, data, JNI_ABORT);
    return result;
}

static _Atomic int shutdown_ready;
static void *shutdown_main(void *unused) {
    (void)unused;
    JNIEnv *env;
    JavaVMAttachArgs options = {JNI_VERSION_1_8, "JNI shutdown worker", NULL};
    if ((*vm)->AttachCurrentThread(vm, (void **)&env, &options) != JNI_OK) {
        atomic_store(&shutdown_ready, -1); return NULL;
    }
    jmethodID method = (*env)->GetStaticMethodID(env, probe_class, "shutdownDone", "()V");
    atomic_store(&shutdown_ready, 1);
    struct timespec pause = {0, 200000000};
    nanosleep(&pause, NULL);
    (*env)->CallStaticVoidMethod(env, probe_class, method);
    (*vm)->DetachCurrentThread(vm);
    return NULL;
}
JNIEXPORT void JNICALL Java_JNIForeignShutdown_start(JNIEnv *env, jclass cls) {
    (void)cls;
    pthread_t thread;
    if (pthread_create(&thread, NULL, shutdown_main, NULL)) {
        (*env)->ThrowNew(env, (*env)->FindClass(env, "java/lang/AssertionError"), "pthread_create failed"); return;
    }
    pthread_detach(thread);
    while (!atomic_load(&shutdown_ready)) sched_yield();
    if (atomic_load(&shutdown_ready) < 0)
        (*env)->ThrowNew(env, (*env)->FindClass(env, "java/lang/AssertionError"), "JNI attach failed");
}
