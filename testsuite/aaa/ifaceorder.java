/* { dg-output "OK" } */

package aaa;

// A class whose interfaces, in the order Java lists them, contradict an
// order its superclass already fixes: Item lists Named before Annotated,
// while Base's FsItem puts Nav (a superinterface of Annotated) before
// Renamable (a subinterface of Named).  Java allows this; CLOS can't build
// a class precedence list for that order of direct superclasses.  The
// Kotlin compiler's KtCommonFile, over IntelliJ's PSI, is shaped like this.
public class ifaceorder
{
    interface Nav { default String nav() { return "nav"; } }
    interface Named { default String name() { return "named"; } }
    interface Renamable extends Named {}
    interface FsItem extends Nav, Renamable {}
    interface El extends Nav {}
    interface Annotated extends El { default String ann() { return "ann"; } }

    static class Base implements FsItem {}
    static class Item extends Base implements Named, Annotated {}

    public static void main(String[] args)
    {
        Item i = new Item();
        Object o = i;
        String result = i.nav() + " " + i.name() + " " + i.ann() + " "
            + (o instanceof Renamable) + " " + (o instanceof El);
        System.out.println("nav named ann true true".equals(result) ? "OK" : "FAIL: " + result);
    }
}
