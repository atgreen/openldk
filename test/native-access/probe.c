/* SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0 */
#include <jni.h>
JNIEXPORT jint JNICALL Java_NativeAccessProbe_ping(JNIEnv *env, jclass type) {
    (void)env; (void)type;
    return 25;
}
