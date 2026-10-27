// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
public class JNIForeignShutdown {
    static native void start();
    public static void main(String[] args) {
        System.load(args[0]);
        start();
        System.out.println("Java main returned");
    }
}
