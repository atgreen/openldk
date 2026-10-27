// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
public class Hook {
    public static Class<?> literal(boolean use) {
        if (!use) return String.class;
        return Trigger.class;
    }
    public static void discardLiteral(boolean use) {
        if (use) { Class<?> unused = Trigger.class; }
    }
    public static Class<?> arrayLiteral() { return Trigger[][].class; }
    private int bias = 7;
    public static int staticRun(boolean execute) {
        if (!execute) {
            int sum = 0;
            for (int i = 0; i < 4; i++) sum += i;
            return sum + 1;
        }
        return new Trigger().value();
    }
    public synchronized int instanceRun(boolean execute) {
        if (!execute) {
            synchronized (this) {
                try { return 1 / 0; }
                catch (ArithmeticException expected) { return bias; }
            }
        }
        return new Trigger().value() + bias;
    }
}
