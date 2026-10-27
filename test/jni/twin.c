/* SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0 */
#include <jni.h>
JNIEXPORT jint JNICALL Java_NativeTwin_value(JNIEnv *env, jclass cls) {
    (void)env; (void)cls; return TWIN_VALUE;
}
