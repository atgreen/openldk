/* { dg-output "OK" } */

package aaa;

import java.lang.reflect.*;
import java.util.*;

// Spring Boot binds @ConfigurationProperties records through reflection:
// record components, the canonical constructor's parameter names (javac
// writes MethodParameters for it) and its generic parameter types.
public class records
{
    record Rec(String text, List<String> names, int repeat) {}

    static <T extends Comparable<T>> List<T> sorted(Map<String, ? extends T> m) { return null; }

    public static void main(String[] args) throws Exception
    {
        List<String> got = new ArrayList<>();
        got.add(String.valueOf(Rec.class.isRecord()));
        got.add(String.valueOf(records.class.isRecord()));
        got.add(Arrays.toString(Rec.class.getRecordComponents()));
        got.add(String.valueOf(Rec.class.getRecordComponents()[1].getGenericType()));
        got.add(String.valueOf(Rec.class.getRecordComponents()[2].getAccessor().invoke(new Rec("a", List.of(), 7))));
        Constructor<?> c = Rec.class.getDeclaredConstructors()[0];
        got.add(Arrays.toString(c.getParameters()));
        got.add(String.valueOf(c.getParameters()[0].isNamePresent()));
        Method m = records.class.getDeclaredMethod("sorted", Map.class);
        got.add(String.valueOf(m.getGenericReturnType()));
        got.add(Arrays.toString(m.getGenericParameterTypes()));
        got.add(String.valueOf(m.getParameters()[0].isNamePresent()));
        String expected = "[true, false, [java.lang.String text, java.util.List names, int repeat], "
            + "java.util.List<java.lang.String>, 7, "
            + "[java.lang.String text, java.util.List<java.lang.String> names, int repeat], true, "
            + "java.util.List<T>, [java.util.Map<java.lang.String, ? extends T>], false]";
        System.out.println(expected.equals(got.toString()) ? "OK" : got.toString());
    }
}
