/* { dg-output "Memory ordering passed" } */
// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
package aaa;

import java.lang.invoke.MethodHandles;
import java.lang.invoke.VarHandle;

public class memoryordering {
    static class Parent { volatile int flag; }
    static class State extends Parent { int payload; long wide; }
    static volatile Throwable failure;
    static volatile int staticFlag;
    static int staticPayload;
    static final int ROUNDS = 2000;

    static void check(boolean condition) {
        if (!condition) throw new AssertionError("memory ordering mismatch");
    }

    public static void main(String[] args) throws Exception {
        State state = new State();
        Thread reader = new Thread(() -> {
            try {
                for (int i = 1; i <= ROUNDS; i++) {
                    while (state.flag != 1) Thread.yield();
                    check(state.payload == i && state.wide == 0x1234567800000000L + i);
                    state.flag = 0;
                }
                while (staticFlag == 0) Thread.yield();
                check(staticPayload == 73);
            } catch (Throwable t) { failure = t; }
        });
        reader.start();
        for (int i = 1; i <= ROUNDS; i++) {
            while (state.flag != 0) {
                if (failure != null) throw new AssertionError(failure);
                Thread.yield();
            }
            state.payload = i;
            state.wide = 0x1234567800000000L + i;
            state.flag = 1;
        }
        staticPayload = 73;
        staticFlag = 1;
        reader.join(15000);
        check(!reader.isAlive());
        if (failure != null) throw new AssertionError(failure);
        VarHandle array = MethodHandles.arrayElementVarHandle(int[].class);
        int[] cells = new int[2];
        Thread publisher = new Thread(() -> {
            cells[0] = 91;
            array.setRelease(cells, 1, 1);
        });
        publisher.start();
        while ((int) array.getAcquire(cells, 1) == 0) Thread.yield();
        check(cells[0] == 91);
        publisher.join();
        check(array.compareAndSet(cells, 1, 1, 2));
        check((int) array.getAndAdd(cells, 1, 3) == 2);
        check((int) array.getVolatile(cells, 1) == 5);
        VarHandle.fullFence();
        VarHandle.acquireFence();
        VarHandle.releaseFence();
        VarHandle.loadLoadFence();
        VarHandle.storeStoreFence();
        System.out.println("Memory ordering passed");
    }
}
