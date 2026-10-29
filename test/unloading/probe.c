/* SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0 */
#include <jni.h>
#include <fcntl.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
static jobject pinned;
static jweak weak_type, weak_helper;
static jmethodID cached_method;
static jfieldID cached_field;
static char *log_path;
static void event(const char *value) {
    int fd = open(log_path, O_WRONLY | O_CREAT | O_APPEND, 0600);
    if (fd >= 0) { (void)write(fd, value, strlen(value)); (void)close(fd); }
}
static void release_pin(JNIEnv *env, jclass type) {
    (void)type;
    /* Keep a local reference until this native invocation returns. */
    (void)(*env)->NewLocalRef(env, pinned);
    (*env)->DeleteGlobalRef(env, pinned);
    pinned = NULL;
}
JNIEXPORT jint JNICALL Java_payload_UnloadingNative_version(JNIEnv *env, jclass type) {
    (void)env; (void)type; return 25;
}
JNIEXPORT jint JNICALL Java_payload_UnloadingNative_cachedVersion(JNIEnv *env, jclass type) {
    if (!(*env)->GetStaticObjectField(env, type, cached_field)) return -1;
    return (*env)->CallStaticIntMethod(env, type, cached_method);
}
JNIEXPORT void JNICALL Java_payload_UnloadingNative_pin(JNIEnv *env, jclass type) {
    pinned = (*env)->NewGlobalRef(env, type);
}
JNIEXPORT jint JNICALL JNI_OnLoad(JavaVM *vm, void *reserved) {
    (void)reserved;
    JNIEnv *env;
    if ((*vm)->GetEnv(vm, (void **)&env, JNI_VERSION_1_8) != JNI_OK) return JNI_ERR;
    const char *path = getenv("OPENLDK_UNLOAD_LOG");
    if (!path) return JNI_ERR;
    log_path = strdup(path);
    if (!log_path) return JNI_ERR;
    jclass type = (*env)->FindClass(env, "payload/UnloadingNative");
    jclass helper = (*env)->FindClass(env, "UnloadingJNIProbe");
    if (!type || !helper) return JNI_ERR;
    weak_type = (*env)->NewWeakGlobalRef(env, type);
    weak_helper = (*env)->NewWeakGlobalRef(env, helper);
    cached_method = (*env)->GetStaticMethodID(env, type, "version", "()I");
    cached_field = (*env)->GetStaticFieldID(env, type, "self", "Ljava/lang/Object;");
    JNINativeMethod method = {"releasePin", "()V", (void *)release_pin};
    if (!cached_method || !cached_field || (*env)->RegisterNatives(env, helper, &method, 1) != JNI_OK) return JNI_ERR;
    if (!(*env)->GetStaticObjectField(env, type, cached_field)
        || (*env)->ExceptionCheck(env)) return JNI_ERR;
    event("L\n");
    return JNI_VERSION_1_8;
}
JNIEXPORT void JNICALL JNI_OnUnload(JavaVM *vm, void *reserved) {
    (void)reserved;
    JNIEnv *env;
    if ((*vm)->GetEnv(vm, (void **)&env, JNI_VERSION_1_8) != JNI_OK) { event("BAD_ENV\n"); return; }
    if (pinned) event("BAD_PIN\n");
    if ((*env)->NewLocalRef(env, weak_type)) event("BAD_WEAK\n");
    /* OnUnload FindClass must use the system loader, not the dead owner. */
    jclass helper = (*env)->FindClass(env, "UnloadingJNIProbe");
    if (!helper || (*env)->ExceptionCheck(env)) {
        event("BAD_FINDCLASS\n");
        (*env)->ExceptionClear(env);
    }
    jclass expected = (*env)->NewLocalRef(env, weak_helper);
    if (!(*env)->IsSameObject(env, helper, expected)) event("BAD_LOADER\n");
    if (helper) (*env)->UnregisterNatives(env, helper);
    (*env)->DeleteWeakGlobalRef(env, weak_type);
    (*env)->DeleteWeakGlobalRef(env, weak_helper);
    event("U\n");
    free(log_path);
    log_path = NULL;
}
