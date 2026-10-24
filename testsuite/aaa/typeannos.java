/* { dg-output "OK" } */

package aaa;

import java.lang.annotation.*;
import java.lang.reflect.*;
import java.util.*;

// Type annotations through reflection (getTypeAnnotationBytes0): Spring 7
// reads JSpecify's @Nullable, a TYPE_USE annotation, this way.
public class typeannos
{
    @Target(ElementType.TYPE_USE) @Retention(RetentionPolicy.RUNTIME) @interface N {}

    @N String field;
    List<@N String> generic;
    String plain;

    @N String method(@N String p, int q) { return p; }
    typeannos(@N Object o) {}

    public static void main(String[] args) throws Exception
    {
        List<String> got = new ArrayList<>();
        got.add(Arrays.toString(typeannos.class.getDeclaredField("field").getAnnotatedType().getAnnotations()));
        got.add(String.valueOf(typeannos.class.getDeclaredField("generic").getAnnotatedType()));
        got.add(Arrays.toString(typeannos.class.getDeclaredField("plain").getAnnotatedType().getAnnotations()));
        Method m = typeannos.class.getDeclaredMethod("method", String.class, int.class);
        got.add(Arrays.toString(m.getAnnotatedReturnType().getAnnotations()));
        got.add(Arrays.toString(m.getAnnotatedParameterTypes()[0].getAnnotations()));
        got.add(Arrays.toString(m.getAnnotatedParameterTypes()[1].getAnnotations()));
        Constructor<?> c = typeannos.class.getDeclaredConstructor(Object.class);
        got.add(Arrays.toString(c.getAnnotatedParameterTypes()[0].getAnnotations()));
        String expected = "[[@aaa.typeannos.N()], java.util.List<@aaa.typeannos.N() java.lang.String>, [], "
            + "[@aaa.typeannos.N()], [@aaa.typeannos.N()], [], [@aaa.typeannos.N()]]";
        System.out.println(expected.equals(got.toString()) ? "OK" : got.toString());
    }
}
