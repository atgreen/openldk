/* { dg-output "^OK!\r\nDone" } */

public class h003
{
    public static int x = 0;

    public static void main(String[] args)
    {
        try {
            x = 10 / x ;
        } catch (Exception e) {
            System.out.println ("OK!");
        }
        System.out.println ("Done.");
    }
}
