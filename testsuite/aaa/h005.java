/* { dg-output "java.lang" } */

package aaa;

public class h005
{
    public static void main (String args[])
    {
        // create instance of a class Character
        final Object o = new Character('!');

        // get a runtime class of an object "o"
        final Class c = o.getClass();

        System.out.println(args[0].hashCode());

        Package p = c.getPackage();
        System.out.println(p);
    }
}
