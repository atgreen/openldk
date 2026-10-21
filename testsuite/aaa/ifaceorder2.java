/* { dg-output "OK" } */

package aaa;

// Interfaces listed in orders that contradict across classes, as in the
// Kotlin compiler's Java model: Member puts Annotated before Modified,
// BinaryModified puts Modified before MapAnnotated (an Annotated), and
// MethodBase implements both.  OpenLDK ordered each class's interfaces as
// Java lists them, and CLOS could not build MethodBase's precedence list.
public class ifaceorder2
{
    interface Element { default String element() { return "element"; } }
    interface Annotated extends Element { default String annotated() { return "annotated"; } }
    interface Modified extends Element { default String modified() { return "modified"; } }
    interface Member extends Annotated, Modified {}
    interface MapAnnotated extends Annotated {}
    interface BinaryModified extends Modified, MapAnnotated {}
    static abstract class MethodBase implements Member, BinaryModified {}
    static class Method extends MethodBase {}

    public static void main(String[] args)
    {
        Method m = new Method();
        String result = m.element() + " " + m.annotated() + " " + m.modified() + " "
            + (((Object) m) instanceof MapAnnotated);
        System.out.println("element annotated modified true".equals(result) ? "OK" : "FAIL: " + result);
    }
}
