/* { dg-output "OK" } */

package aaa;

// A new object's fields start out default, including one named clazz
// (Groovy's ClassNode has one, and is resolved when it is set).  OpenLDK
// set any field named clazz to the object's own class.
public class clazzfield
{
    static class Node {
        Class<?> clazz;
        String name;
        Node(String name) { this.name = name; }
        Node(Class<?> c) { this.clazz = c; this.name = c.getName(); }
        boolean isResolved() { return clazz != null; }
    }

    public static void main(String[] args) throws Exception
    {
        Node plain = new Node("x.Y");
        Node real = new Node(String.class);
        Node reflected = Node.class.getDeclaredConstructor(String.class).newInstance("z");
        String result = plain.isResolved() + " " + real.isResolved() + " " + reflected.isResolved();
        System.out.println("false true false".equals(result) ? "OK" : "FAIL: " + result);
    }
}
