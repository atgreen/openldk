/* { dg-output "OK" } */

package aaa;

import java.util.*;

// Class.getNestMembers (getNestMembers0): the nest host, then its members,
// as Hibernate reads them building its entity metamodel.
public class nestmembers
{
    class A {}
    static class B { class C {} }
    interface I {}

    public static void main(String[] args)
    {
        List<String> got = new ArrayList<>();
        for (Class<?> c : new Class<?>[] { nestmembers.class, A.class, B.C.class, I.class, int.class, int[].class })
            got.add(c.getSimpleName() + ":" + c.getNestHost().getSimpleName() + ":"
                    + Arrays.stream(c.getNestMembers()).map(Class::getSimpleName).sorted().toList()
                    + ":" + c.isNestmateOf(nestmembers.class));
        got.add(String.valueOf(nestmembers.class.getNestMembers()[0] == nestmembers.class));
        String members = "[A, B, C, I, nestmembers]";
        String expected = "[nestmembers:nestmembers:" + members + ":true, A:nestmembers:" + members + ":true, "
            + "C:nestmembers:" + members + ":true, I:nestmembers:" + members + ":true, "
            + "int:int:[int]:false, int[]:int[]:[int[]]:false, true]";
        System.out.println(expected.equals(got.toString()) ? "OK" : got.toString());
    }
}
