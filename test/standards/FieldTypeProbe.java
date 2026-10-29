// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
public class FieldTypeProbe {
    static int initializations;
    static class Owner { public ColdType target; }
    static class ColdType {
        static { initializations++; }
        public int value;
        public void ping() { }
    }
    static void check(boolean value) { if (!value) throw new AssertionError(); }
    public static void main(String[] args) throws Exception {
        Class<?> type = Owner.class.getDeclaredField("target").getType();
        check(initializations == 0);
        check(type.getName().equals("FieldTypeProbe$ColdType"));
        check(type.getClassLoader() == Owner.class.getClassLoader());
        check(type.getDeclaredMethod("ping").getReturnType() == void.class);
        check(type.getDeclaredField("value").getType() == int.class);
        check(type == ColdType.class);
        check(initializations == 0);
        new ColdType();
        check(initializations == 1);
        System.out.println("Field types: canonical mirrors, metadata, loaders and delayed initialization passed");
    }
}
