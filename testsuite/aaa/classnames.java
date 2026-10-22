/* { dg-output "OK" } */

package aaa;

// Class names of nested, local and anonymous classes, which come from the
// InnerClasses attribute (Class.getSimpleBinaryName0).
public class classnames
{
    static class Nested {}
    class Inner {}

    public static void main(String[] args)
    {
        class Local {}
        Object anon = new Object() {};
        StringBuilder sb = new StringBuilder();
        for (Class<?> c : new Class<?>[] { classnames.class, Nested.class, Inner.class, Local.class, anon.getClass() })
            sb.append(c.getSimpleName()).append(':').append(c.isAnonymousClass()).append(':')
              .append(c.isLocalClass()).append(':').append(c.isMemberClass()).append(' ');
        String want = "classnames:false:false:false Nested:false:false:true Inner:false:false:true Local:false:true:false :true:false:false ";
        System.out.println(want.equals(sb.toString()) ? "OK" : "FAIL: " + sb);
    }
}
