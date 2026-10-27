// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
public class Caller {
    public static Base make() { return new Leaf(); }
    public static int staticValue() { return Leaf.answer(); }
    public static int staticField() { return Leaf.field; }
}
