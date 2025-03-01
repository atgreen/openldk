/* { dg-output "OK" } */

package aaa;

public class h006
{
    public static void main (String args[])
    {
        // Create an array of int (base type)
        int[] intArray = new int[10];

        // Check if the object is an array and if it is an array of int
        if (intArray instanceof int[]) {
            System.out.println("OK");
        } else {
            System.out.println("BAD");
        }
    }
}
