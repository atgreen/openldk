// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
class PrimitiveCodeBaseline {
    public static long loop(long x, int n) {
        for (int i = 0; i < n; ++i) x = x * 3 + 1;
        return x;
    }
}
class PrimitiveCodeOptimized {
    public static long loop(long x, int n) {
        for (int i = 0; i < n; ++i) x = x * 3 + 1;
        return x;
    }
}
