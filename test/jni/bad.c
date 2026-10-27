/* SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0 */
#include <jni.h>
JNIEXPORT jint JNICALL JNI_OnLoad(JavaVM *vm, void *reserved) {
    (void)vm; (void)reserved; return 0x12345678;
}
