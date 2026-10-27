// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
public class Base {
    static { System.setProperty("hierarchy.base.initialized", "yes"); }
    public static int field = 37;
    public static int answer() { return 42; }
    public int value() { return 42; }
}
