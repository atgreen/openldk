// Copyright (C) 2026 Anthony Green <green@moxielogic.com>
// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
package dispatch.a;
public class Base {
    String m() { return "base"; }
    private String hidden() { return "private-base"; }
    public static class Same extends Base { String m() { return "same"; } }
    public static class Bridge extends Base { public String m() { return "bridge"; } }
    public static class Back extends dispatch.b.Foreign { public String m() { return "back"; } }
}
