/* { dg-output "Scoped Values passed" } */
// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
package aaa;

import java.util.NoSuchElementException;

public class scopedvalues {
    static final ScopedValue<String> VALUE = ScopedValue.newInstance();
    static final ScopedValue<Integer> OTHER = ScopedValue.newInstance();
    static volatile Throwable failure;
    static volatile boolean start;

    static void check(boolean condition) {
        if (!condition) throw new AssertionError("scoped value mismatch");
    }

    static void exercise(String label) {
        try {
            while (!start) Thread.yield();
            check(!VALUE.isBound());
            for (int i = 0; i < 200; i++) {
                ScopedValue.where(VALUE, label).run(() -> {
                    check(VALUE.get().equals(label));
                    ScopedValue.where(VALUE, "nested").where(OTHER, 42).run(() -> {
                        check(VALUE.get().equals("nested"));
                        check(OTHER.get() == 42);
                        Thread.yield();
                    });
                    check(VALUE.get().equals(label));
                    check(!OTHER.isBound());
                    try {
                        ScopedValue.where(VALUE, "exception").run(() -> {
                            throw new IllegalStateException();
                        });
                    } catch (IllegalStateException expected) { }
                    check(VALUE.get().equals(label));
                });
                check(!VALUE.isBound());
            }
        } catch (Throwable t) { failure = t; }
    }

    public static void main(String[] args) throws Exception {
        check(!VALUE.isBound());
        try { VALUE.get(); throw new AssertionError("unbound get"); }
        catch (NoSuchElementException expected) { }
        Thread a = new Thread(() -> exercise("a"));
        Thread b = new Thread(() -> exercise("b"));
        Thread virtual = Thread.ofVirtual().unstarted(() -> exercise("virtual"));
        ScopedValue.where(VALUE, "parent").run(() -> { a.start(); b.start(); virtual.start(); });
        start = true;
        a.join(15000); b.join(15000); virtual.join(15000);
        check(!a.isAlive() && !b.isAlive() && !virtual.isAlive());
        if (failure != null) throw new AssertionError(failure);
        check(!VALUE.isBound());
        ScopedValue.where(VALUE, null).run(() -> check(VALUE.isBound() && VALUE.get() == null));
        String result = ScopedValue.where(VALUE, "call").call(() -> VALUE.get());
        check(result.equals("call"));
        check(!VALUE.isBound());
        System.out.println("Scoped Values passed");
    }
}
