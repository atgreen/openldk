/* { dg-output "OK" } */

package aaa;

public class h007
{
    public static void main (String args[])
    {
        System.out.println (new StackTraceElement("myclass", "mymethod", "myfile.java", 2025));
        System.out.println ("OK");
    }
}
