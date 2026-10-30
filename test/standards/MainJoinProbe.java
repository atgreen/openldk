// Copyright (C) 2026 Anthony Green <green@moxielogic.com>
// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
public class MainJoinProbe {
    static volatile boolean returning;
    public static void main(String[] args) {
        Thread owner = Thread.currentThread();
        if (!owner.isAlive()) throw new AssertionError("Main is not alive before return");
        new Thread(() -> {
            try {
                owner.join(10000);
                if (owner.isAlive() || owner.getState() != Thread.State.TERMINATED || !returning)
                    throw new AssertionError("Main termination was not published");
                System.out.println("Child joining main after its return passed");
            } catch (Throwable error) {
                error.printStackTrace(); System.exit(1);
            }
        }).start();
        returning = true;
    }
}
