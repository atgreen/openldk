// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
public class ArrayCaller {
    public static void check() throws Exception {
        Object array = new ArrayChild[1];
        if (!(array instanceof ArrayParent[])) throw new AssertionError("Superclass instanceof");
        if (!(array instanceof ArrayContract[])) throw new AssertionError("Interface instanceof");
        ArrayParent[] parents = (ArrayParent[]) array;
        ArrayContract[] contracts = (ArrayContract[]) array;
        if (parents.length != 1 || contracts.length != 1) throw new AssertionError("Array length");
        ArrayChild child = new ArrayChild();
        Object image = child.handle;
        if (image == null || ((ArrayParent) child).handle != 37)
            throw new AssertionError("Shadowed field overwrote superclass storage");
        ((ArrayParent) child).handle = 91;
        if (child.handle != image || ((ArrayParent) child).handle != 91)
            throw new AssertionError("Direct access crossed shadowed fields");
        var parentField = ArrayParent.class.getField("handle");
        var childField = ArrayChild.class.getField("handle");
        if (parentField.getLong(child) != 91 || childField.get(child) != image)
            throw new AssertionError("Reflection crossed shadowed fields");
        parentField.setLong(child, 103);
        if (((ArrayParent) child).handle != 103 || child.handle != image)
            throw new AssertionError("Reflective write crossed shadowed fields");
    }
}
