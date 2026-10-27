// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
interface InitConstants {
    Object VALUE = new Object();
}
public class InterfaceInitProbe implements InitConstants {
    static final Object COPY = InterfaceInitProbe.VALUE;
    public static void main(String[] args) {
        if (COPY == null || COPY != InitConstants.VALUE) throw new AssertionError("Inherited interface field was read before initialization");
        System.out.println("Inherited interface constant initialized before use");
    }
}
