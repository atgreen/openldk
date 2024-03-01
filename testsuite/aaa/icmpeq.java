/* { dg-output "" } */

package aaa;

public class icmpeq
{
    static int compare (int a, int b)
    {
        if (a != b)
            return 1;
        else
            return 0;
    }

    public static void main(String[] args)
    {
        compare (1, 2);
    }
}
