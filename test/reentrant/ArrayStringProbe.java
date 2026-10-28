// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
public class ArrayStringProbe {
    public static void main(String[] args) {
        Object[] arrays = {new int[]{41}, new String[]{"x"}, new int[2][3], new byte[0]};
        for (Object array : arrays) {
            String expected = array.getClass().getName() + "@" + Integer.toHexString(System.identityHashCode(array));
            if (!array.toString().equals(expected) || !String.valueOf(array).equals(expected))
                throw new AssertionError("Array Object.toString must return its Java identity string");
        }
        System.out.println("Array Object.toString and String.valueOf preserve Java identity strings");
    }
}
