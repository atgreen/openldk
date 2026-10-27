// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
public class LibraryOwner {
    static void tail(String path) { System.load(path); }
    public static boolean load(String path) {
        try { tail(path); return false; }
        catch (UnsatisfiedLinkError expected) { return true; }
    }
}
