// Copyright (C) 2026 Anthony Green <green@moxielogic.com>
// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
public class ThreadImage {
    static Thread saved, cleanupThread;
    static int warmups;
    static native void installCleanupMap();
    public static class CleanupMap extends java.util.concurrent.ConcurrentHashMap<Object, Object> {
        public void clear() {
            cleanupThread = Thread.currentThread();
            super.clear();
        }
    }
    public static void main(String[] args) throws Exception {
        if (args.length > 0 && args[0].equals("warm")) {
            MainThreadProbe.main(new String[0]);
            saved = Thread.currentThread();
            installCleanupMap(); warmups++;
        } else {
            if (warmups != 1 || saved == null) throw new AssertionError("Warmup state was not retained");
            if (saved == Thread.currentThread()) throw new AssertionError("Build-time main thread was reused");
            if (saved.isAlive() || saved.getState() != Thread.State.TERMINATED)
                throw new AssertionError("Build-time thread was not retired");
            if (cleanupThread == null) throw new AssertionError("Final cache cleanup callback did not run");
            if (cleanupThread.isAlive() || cleanupThread.getState() != Thread.State.TERMINATED)
                throw new AssertionError("Thread used during final cache cleanup was not retired");
            MainThreadProbe.main(new String[0]);
            System.out.println("Saved thread identities retire and restarted main stays alive");
        }
    }
}
