/* SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0 */
#include <jni.h>
static jint registered(JNIEnv *env, jclass type) { (void)env; (void)type; return 27; }
JNIEXPORT jint JNICALL Java_policyclient_Target_symbol(JNIEnv *env, jclass type) {
    (void)env; (void)type; return 26;
}
JNIEXPORT jint JNICALL JNI_OnLoad(JavaVM *vm, void *reserved) {
    JNIEnv *env;
    (void)reserved;
    if ((*vm)->GetEnv(vm, (void **)&env, JNI_VERSION_1_8) != JNI_OK) return JNI_ERR;
    jclass type = (*env)->FindClass(env, "policyclient/Target");
    if (!type) return JNI_ERR;
    JNINativeMethod method = {"registered", "()I", (void *)registered};
    if ((*env)->RegisterNatives(env, type, &method, 1) != JNI_OK) return JNI_ERR;
    return JNI_VERSION_1_8;
}
