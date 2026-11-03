/* { dg-output "OK" } */

package aaa;

import java.io.Serializable;

public class arrayinterfaces
{
    public static void main(String[] args)
    {
        Serializable[] serializable = new Serializable[2];
        serializable[0] = new int[0];
        serializable[1] = new String[0];

        Cloneable[] cloneable = new Cloneable[2];
        cloneable[0] = new long[0];
        cloneable[1] = new Object[0];

        try
        {
            Object[] covariant = serializable;
            covariant[0] = new Object();
            throw new AssertionError("incompatible store succeeded");
        }
        catch (ArrayStoreException expected)
        {
            System.out.println("OK");
        }
    }
}
