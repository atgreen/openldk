/* { dg-output "OK\r\nDone\r\nFin\r\n" } */

package aaa;

public class h004
{
    public static int x = 0;

    public static void main(String[] args)
    {
        try {
            try {
                try {
                    x = 10 / x ;
                } finally {
                    System.out.println ("OK");
                }
            } finally {
                System.out.println ("Done");
            }
        } catch (Exception e) {
            System.out.println ("Fin");
        }
    }
}
