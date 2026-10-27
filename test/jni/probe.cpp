// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
#include <jni.h>
extern "C" JNIEXPORT jint JNICALL Java_JNIProbe_cpp(JNIEnv *env, jclass cls, jint n) {
    jmethodID method = env->GetStaticMethodID(cls, "registered", "(I)I");
    return env->CallStaticIntMethod(cls, method, n) + 24;
}
